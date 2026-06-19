#!/usr/bin/env python3
"""pdf-annotation-extractor.py -- Extract PDF annotations to annotation-import JSON.

Reads standard PDF annotations (as written by Highlights on iPad/Mac, Skim,
Preview, Acrobat, ...) and emits JSON in the same envelope the KOReader
backend produces, so that the existing `org-roam-annotation-import.el`
machinery can consume it via a thin Emacs backend.

Why a standalone script (not elisp): quad-based text recovery and pixmap
clipping for figure snapshots are trivial in PyMuPDF and painful in elisp.
Emacs stays the JSON -> org-roam layer.

Annotation handling
-------------------
* Highlight / Underline / StrikeOut / Squiggly  (text markup, type 8/9/10/11)
    Geometry lives in `annot.vertices` as a flat list of quad points (4 points
    = one line). The marked TEXT is not stored in the annotation -- it must be
    recovered by clipping each quad's rect against the page text. Multi-line
    highlights have several quads; we extract each separately and join, instead
    of clipping the whole bounding box (which would grab unrelated text on
    intermediate lines).

* Square  (type 4)  -- Highlights' "snapshot a figure" rectangle selection.
    No selectable text underneath, so we rasterise the region to a PNG in a
    fixed image directory and record its path; the Emacs side links it with
    [[file:...]].

* FreeText / Text  (typewriter / sticky notes) -- captured as note-only
    annotations (quote empty, the typed content in :text).

Annotation ID
-------------
Prefer the PDF /NM name (`annot.info["id"]`). Many third-party readers,
Highlights included in some cases, leave it empty; then we fall back to a
SHA1 content hash of (page, quad-geometry, quote, note). We deliberately do
NOT use the PDF xref for identity: it is not stable across PDF
rewrites/optimisation.

Output schema (per file)
------------------------
{
  "title":       <pdf metadata title or filename stem>,
  "author":      <pdf metadata author or null>,
  "url":         null,
  "source_tag":  "highlights",
  "updated_at":  <ISO8601 generation time>,
  "entries": [ {annotation}, ... ]      # see _annotation_record
}

Usage
-----
    pdf-annotation-extractor.py FILE.pdf [FILE2.pdf ...] \
        [--out DIR] [--image-dir DIR] [--dpi N] [--title TITLE]

If --out is omitted, JSON is written next to each PDF (FILE.json).
Images go to --image-dir (default: <out>/images), named
    <pdf-stem>-p<page>-<short-id>.png
"""

from __future__ import annotations

import argparse
import datetime as _dt
import hashlib
import json
import re
import sys
from pathlib import Path

try:
    import pymupdf as fitz  # PyMuPDF >= 1.24 exposes the `pymupdf` name
except ImportError:  # pragma: no cover - older installs
    import fitz

# --- PDF annotation subtype numbers (MuPDF / Adobe) ------------------------
PDF_ANNOT_TEXT = 0
PDF_ANNOT_FREETEXT = 2
PDF_ANNOT_SQUARE = 4
PDF_ANNOT_HIGHLIGHT = 8
PDF_ANNOT_UNDERLINE = 9
PDF_ANNOT_SQUIGGLY = 10
PDF_ANNOT_STRIKEOUT = 11

TEXT_MARKUP = {
    PDF_ANNOT_HIGHLIGHT,
    PDF_ANNOT_UNDERLINE,
    PDF_ANNOT_SQUIGGLY,
    PDF_ANNOT_STRIKEOUT,
}
NOTE_TYPES = {PDF_ANNOT_TEXT, PDF_ANNOT_FREETEXT}

# Per-quad clip allowance: many writers make the markup rect slightly too
# tight to fully cover the glyphs, so we pad before clipping text.
CLIP_PAD = 2.0


def _short_hash(*parts) -> str:
    payload = "\0".join("" if p is None else str(p) for p in parts)
    return hashlib.sha1(payload.encode("utf-8")).hexdigest()[:16]


def _normalise_ws(s: str | None) -> str:
    if not s:
        return ""
    # Join hyphenated line breaks ("anno-\ntation" -> "annotation"),
    # then collapse remaining whitespace.
    s = re.sub(r"-\n", "", s)
    s = re.sub(r"\s+", " ", s)
    return s.strip()


def _quads_from_vertices(vertices):
    """Yield fitz.Quad objects from a flat vertex list (groups of 4 points)."""
    if not vertices:
        return
    # vertices is a list of (x, y) pairs; 4 consecutive pairs = one quad.
    for i in range(0, len(vertices) - 3, 4):
        pts = vertices[i : i + 4]
        try:
            yield fitz.Quad(pts)
        except Exception:
            continue


# Two words are treated as the same physical word (a duplicate emitted by
# overlapping marked-content groups) when their bbox centres are within this
# many points of each other. OCR text layers that double-up a region place the
# two copies almost exactly on top of one another, so a small tolerance is
# enough and won't merge genuinely distinct adjacent words.
_DUP_TOL = 3.0


def _word_list(page):
    """Return the page word list once, as (Rect, text) pairs, in reading order.

    Uses the same dehyphenation that `search_for` relies on, so hyphenated
    line breaks collapse and the phantom-space artifacts are reduced.
    """
    flags = fitz.TEXT_DEHYPHENATE | fitz.TEXT_PRESERVE_WHITESPACE | fitz.TEXT_PRESERVE_LIGATURES
    words = page.get_text("words", flags=flags, sort=True)
    return [(fitz.Rect(w[:4]), w[4]) for w in words if w[4].strip()]


def _dedup_overlapping(selected):
    """Drop words whose bbox centre coincides with an already-kept word.

    This collapses the duplicate runs that overlapping OCR marked-content
    groups produce (the entry-1 "...remains without value" doubling),
    mirroring what TextPage does for `search_for` overlapping rectangles.
    Keeps the first occurrence (reading order), which is normally the
    less-garbled copy.
    """
    kept = []
    centres = []
    for rect, text in selected:
        cx = (rect.x0 + rect.x1) / 2.0
        cy = (rect.y0 + rect.y1) / 2.0
        dup = any(abs(cx - px) <= _DUP_TOL and abs(cy - py) <= _DUP_TOL
                  for px, py in centres)
        if not dup:
            kept.append(text)
            centres.append((cx, cy))
    return kept


def _text_under_markup(page, annot, word_cache) -> str:
    """Recover marked text by selecting page words whose bbox lies in a quad.

    Instead of `get_text(clip=...)` per quad -- which re-emits the overlapping
    text of every marked-content group and produces doubled OCR garbage -- we
    take the page's word list once and keep each word whose bbox centre falls
    inside (a slightly padded) quad rect. Duplicates from stacked content
    groups are then removed positionally. This matches the structured approach
    `search_for` uses and brings results to rough parity with Highlights.
    """
    words = word_cache.get(page.number)
    if words is None:
        words = _word_list(page)
        word_cache[page.number] = words

    quads = list(_quads_from_vertices(annot.vertices))
    rects = ([q.rect for q in quads]
             if quads else [annot.rect])
    rects = [r + (-CLIP_PAD, -CLIP_PAD, CLIP_PAD, CLIP_PAD) for r in rects]

    selected = []
    for wrect, wtext in words:
        wcx = (wrect.x0 + wrect.x1) / 2.0
        wcy = (wrect.y0 + wrect.y1) / 2.0
        if any((r.x0 <= wcx <= r.x1 and r.y0 <= wcy <= r.y1) for r in rects):
            selected.append((wrect, wtext))

    return _normalise_ws(" ".join(_dedup_overlapping(selected)))


def _annotation_id(annot, page_no, quote, note, geom_key) -> str:
    nm = (annot.info or {}).get("id") or ""
    nm = nm.strip()
    if nm:
        return f"highlights-{nm}"
    return "highlights-" + _short_hash(page_no, geom_key, quote, note)


def _render_image(page, annot, image_dir: Path, stem: str, ann_id: str, dpi: int) -> str:
    image_dir.mkdir(parents=True, exist_ok=True)
    short = ann_id.split("-")[-1][:8]
    fname = f"{stem}-p{page.number + 1}-{short}.png"
    out = image_dir / fname
    zoom = dpi / 72.0
    mat = fitz.Matrix(zoom, zoom)
    rect = annot.rect + (-CLIP_PAD, -CLIP_PAD, CLIP_PAD, CLIP_PAD)
    pix = page.get_pixmap(matrix=mat, clip=rect)
    pix.save(out)
    return str(out)


def _chapter_for_point(toc_entries, page_no):
    """Return the deepest TOC title whose page <= page_no, or None."""
    chapter = None
    for level, title, pg in toc_entries:
        if pg is not None and pg - 1 <= page_no:
            chapter = title
        elif pg is not None and pg - 1 > page_no:
            break
    return chapter


def _annotation_record(page, annot, *, image_dir, stem, dpi, toc, word_cache):
    atype = annot.type[0]
    page_no = page.number
    info = annot.info or {}
    note = _normalise_ws(info.get("content"))
    created = info.get("creationDate") or info.get("modDate") or ""

    quote = ""
    image_path = None

    if atype in TEXT_MARKUP:
        quote = _text_under_markup(page, annot, word_cache)
        geom_key = str(annot.vertices)
    elif atype == PDF_ANNOT_SQUARE:
        geom_key = str(tuple(annot.rect))
        # ann_id needs to exist before naming the image; compute it here.
        tmp_id = _annotation_id(annot, page_no, quote, note, geom_key)
        image_path = _render_image(page, annot, Path(image_dir), stem, tmp_id, dpi)
    elif atype in NOTE_TYPES:
        geom_key = str(tuple(annot.rect))
    else:
        return None  # ink, line, polygon, widgets, links -> skip

    ann_id = _annotation_id(annot, page_no, quote, note, geom_key)

    rec = {
        "id": ann_id,
        "source": "Highlights",
        "anki": True,
        "quote": quote,
        "text": note,
        "page": page_no + 1,
        "chapter": _chapter_for_point(toc, page_no),
        "color": _color_hex(annot),
        "updated_at": created,
    }
    if image_path:
        rec["image"] = image_path
    return rec


def _color_hex(annot) -> str | None:
    colors = annot.colors or {}
    stroke = colors.get("stroke") or []
    if len(stroke) == 3:
        # Clamp to [0,1]: some writers emit slightly out-of-range channels
        # (the source of the malformed 7-digit "#112767b" hex).
        ch = (max(0, min(255, int(round(c * 255)))) for c in stroke)
        return "#{:02x}{:02x}{:02x}".format(*ch)
    return None


def extract(pdf_path: Path, *, image_dir: Path, dpi: int, title_override=None):
    doc = fitz.open(pdf_path)
    meta = doc.metadata or {}
    stem = pdf_path.stem
    toc = doc.get_toc(simple=True) or []

    records = []
    word_cache = {}
    for page in doc:
        for annot in page.annots() or []:
            try:
                rec = _annotation_record(
                    page, annot, image_dir=image_dir, stem=stem, dpi=dpi, toc=toc,
                    word_cache=word_cache,
                )
            except Exception as exc:  # keep going on a single bad annot
                print(
                    f"  warn: skipped annot on page {page.number + 1}: {exc}",
                    file=sys.stderr,
                )
                rec = None
            if rec and (rec["quote"] or rec["text"] or rec.get("image")):
                records.append(rec)

    doc.close()

    title = title_override or (meta.get("title") or "").strip() or stem
    author = (meta.get("author") or "").strip() or None

    return {
        "title": title,
        "author": author,
        "url": None,
        "source_tag": "highlights",
        "updated_at": _dt.datetime.now().astimezone().isoformat(timespec="seconds"),
        "entries": records,
    }


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("pdfs", nargs="+", type=Path, help="PDF file(s) to extract")
    p.add_argument("--out", type=Path, default=None, help="output directory for JSON (default: next to each PDF)")
    p.add_argument("--image-dir", type=Path, default=None, help="directory for figure snapshots (default: <out>/images)")
    p.add_argument("--dpi", type=int, default=200, help="render DPI for figure snapshots (default: 200)")
    p.add_argument("--title", default=None, help="override the document title (single-file use)")
    args = p.parse_args(argv)

    if args.title and len(args.pdfs) > 1:
        p.error("--title only makes sense with a single PDF")

    for pdf in args.pdfs:
        if not pdf.exists():
            print(f"error: {pdf} does not exist", file=sys.stderr)
            continue
        out_dir = args.out or pdf.parent
        out_dir.mkdir(parents=True, exist_ok=True)
        image_dir = args.image_dir or (out_dir / "images")

        print(f"Extracting {pdf} ...", file=sys.stderr)
        data = extract(pdf, image_dir=image_dir, dpi=args.dpi, title_override=args.title)
        out_json = out_dir / (pdf.stem + ".json")
        out_json.write_text(json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8")
        n = len(data["entries"])
        n_img = sum(1 for e in data["entries"] if e.get("image"))
        print(f"  -> {out_json}  ({n} annotations, {n_img} images)", file=sys.stderr)


if __name__ == "__main__":
    main()
