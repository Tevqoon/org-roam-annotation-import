;;; highlights-pdf-backend.el --- Import Highlights/PDF annotations into Org-roam -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This backend imports annotations from JSON produced by
;; `pdf-annotation-extractor.py', which reads STANDARD PDF annotations
;; (as written by Highlights on iPad/Mac, Skim, Preview, Acrobat, ...).
;; Highlights stores its markup as ordinary PDF annotation dictionaries
;; and does not lock the file in, so the Python side just walks the PDF
;; with PyMuPDF; there is no proprietary sidecar.
;;
;; Architecture mirrors `koreader-json-backend':
;;   PDF --(PyMuPDF)--> JSON --(this file)--> org-roam
;; This file is the thin JSON->roam half.  It is ~the KOReader backend
;; with two differences:
;;
;;   1. ID source.  The extractor prefers the PDF /NM name and only
;;      hashes content as a fallback, so the IDs already arrive stable
;;      and prefixed ("highlights-...").  We pass them through verbatim
;;      rather than re-hashing here (the KOReader backend must hash
;;      because KOReader exposes no stable id).
;;
;;   2. Figure snapshots.  Highlights' rectangle selections are exported
;;      as Square annotations with a rasterised PNG saved to a fixed
;;      directory.  Such annotations carry an :image path and are written
;;      with a custom :write-fn that emits an org [[file:...]] link as the
;;      Front body, instead of quoted text.
;;
;; As with KOReader, PDF metadata titles are unreliable, so the import
;; prompts for the destination org-roam node.  Create proper book/paper
;; notes with `js-book-capture' first, then point this importer at them.

;;; Code:

(require 'org-roam-annotation-import)
(require 'org-roam)
(require 'json)

(defcustom highlights-pdf-json-file-pattern "\\.json\\'"
  "Regexp matching JSON annotation files emitted by the PDF extractor."
  :group 'annotation
  :type 'string)

(defvar highlights-pdf--load-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory this file was loaded from.
Used to locate the bundled `pdf-annotation-extractor.py'.  Captured at
load time because `load-file-name' is only bound while loading.")

(defcustom highlights-pdf-extractor-script
  (expand-file-name "scripts/pdf-annotation-extractor.py" highlights-pdf--load-dir)
  "Path to the `pdf-annotation-extractor.py' script.
Defaults to the copy shipped under scripts/ in this package directory,
so no configuration is needed for a normal install.  Override only if
you keep the script elsewhere.  Used by `highlights-pdf-import-pdf'."
  :group 'annotation
  :type 'file)

(defcustom highlights-pdf-python "python3"
  "Python interpreter used to run the extractor (must have PyMuPDF)."
  :group 'annotation
  :type 'string)

(defcustom highlights-pdf-image-relocate-fn nil
  "Optional function to rewrite an extracted image path before linking.
Called with the absolute PNG path the extractor wrote and must return
the path to store in the org [[file:...]] link.  Use this to move images
under your org-roam image tree, e.g. into a per-note attachment dir.
When nil, the extractor's path is linked as-is."
  :group 'annotation
  :type '(choice (const :tag "Link as-is" nil) function))

;;;; JSON parsing

(defun highlights-pdf--parse-json-file (file)
  "Parse extractor JSON FILE into a plist (keys are :keywords)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object nil)))

;;;; Image write-fn

(defun highlights-pdf--image-front-body (annotation entry-title entry-url)
  "Build a Front body that embeds the figure image as an org file link.
Falls back to any :text note beneath the image."
  (let* ((raw   (plist-get annotation :image))
         (path  (if (and raw highlights-pdf-image-relocate-fn)
                    (funcall highlights-pdf-image-relocate-fn raw)
                  raw))
         (note  (plist-get annotation :text))
         (link  (when path (format "[[file:%s]]" path)))
         (src   (cond ((and entry-url entry-title)
                       (format "Source: [[%s][%s]]" entry-url entry-title))
                      (entry-title (format "Source: %s" entry-title))
                      (t nil)))
         (parts (delq nil (list link
                                (and note (not (string-empty-p note)) note)
                                src))))
    (string-join parts "\n\n")))

(defun highlights-pdf--write-image-annotation (annotation entry-title entry-url)
  "Custom :write-fn for figure snapshots.
Writes properties + a Front body containing an org image link.  Honours
the same Manual guard as the default writer so a hand-edited Front (e.g.
an OCR'd caption) survives re-import."
  (let* ((updated-at (plist-get annotation :updated-at))
         (chapter    (plist-get annotation :chapter))
         (page       (plist-get annotation :page))
         (source     (plist-get annotation :source))
         (manual-p   (string= "t" (org-entry-get nil "Manual")))
         (deck       (annotation--resolve-anki-deck (plist-get annotation :anki)))
         (tags       (annotation--current-outline-tags entry-title))
         (front-body (highlights-pdf--image-front-body
                      annotation entry-title entry-url)))
    (when updated-at (org-set-property "Updated-at" updated-at))
    (when source     (org-set-property "Source"     source))
    (when chapter    (org-set-property "Chapter"    chapter))
    (when page       (org-set-property "Page"       (format "%s" page)))
    (org-set-property "Figure" "t")
    (when deck (annotation--set-anki-properties tags deck))
    (unless manual-p
      (annotation--upsert-child-heading "Front" front-body))))

;;;; Entry transformation

(defun highlights-pdf--transform-annotation (a)
  "Transform one extractor annotation plist A into the standard format.
Image annotations are routed through the custom :write-fn; text-markup
annotations use the default writer."
  (let ((base (list :id         (plist-get a :id)
                    :source     (or (plist-get a :source) "Highlights")
                    :anki       (plist-get a :anki)
                    :quote      (plist-get a :quote)
                    :text       (plist-get a :text)
                    :page       (plist-get a :page)
                    :chapter    (plist-get a :chapter)
                    :updated-at (plist-get a :updated-at))))
    (if (plist-get a :image)
        (plist-put (plist-put base :image (plist-get a :image))
                   :write-fn #'highlights-pdf--write-image-annotation)
      base)))

(defun highlights-pdf--transform-json (json-data)
  "Transform extractor JSON-DATA into a standard entry plist."
  (let* ((title       (plist-get json-data :title))
         (author      (plist-get json-data :author))
         (entries     (plist-get json-data :entries))
         (updated-on  (plist-get json-data :updated_at))
         (annotations (mapcar #'highlights-pdf--transform-annotation entries)))
    (list :title       title
          :author      author
          :url         (plist-get json-data :url)
          :source-tag  (or (plist-get json-data :source_tag) "highlights")
          :updated-at  updated-on
          :annotations annotations)))

;;;; Node selection (mirrors koreader--select-node)

(defun highlights-pdf--select-node (default-title)
  "Prompt for the org-roam node to receive annotations.
DEFAULT-TITLE (from PDF metadata) pre-fills the prompt.  Selecting an
existing note returns its node; typing a new title returns a fresh node
that `annotation--org-roam-node-open-or-create' files on first write."
  (let ((node (org-roam-node-read default-title nil nil nil
                                  "PDF note (select or type new title): ")))
    (if (and node (org-roam-node-title node))
        node
      (user-error "No note selected"))))

;;;; Import a JSON file

(defun highlights-pdf--import-json-file (file)
  "Import annotations from a single extractor JSON FILE.
Prompts for the destination org-roam node."
  (let* ((json-data (highlights-pdf--parse-json-file file))
         (entry     (highlights-pdf--transform-json json-data))
         (node      (highlights-pdf--select-node (plist-get entry :title))))
    (setq entry (plist-put entry :node node))
    (annotation-debug 1 "Importing from: %s" file)
    (annotation-debug 2 "Title: %s, Annotations: %d"
                      (plist-get entry :title)
                      (length (plist-get entry :annotations)))
    (annotation--update-entries (list entry))))

(defun highlights-pdf--find-json-files (directory)
  "Find JSON files matching `highlights-pdf-json-file-pattern' in DIRECTORY."
  (directory-files directory t highlights-pdf-json-file-pattern))

;;;; Interactive entry points

;;;###autoload
(defun highlights-pdf-import-json-file (file)
  "Import annotations from an extractor JSON FILE.
Prompts for the file, then for the destination org-roam node."
  (interactive
   (list (read-file-name "PDF-annotation JSON file: "
                         annotation-default-json-directory
                         nil t nil
                         (lambda (f) (or (file-directory-p f)
                                         (string-match-p highlights-pdf-json-file-pattern f))))))
  (highlights-pdf--import-json-file file)
  (message "Imported annotations from %s" (file-name-nondirectory file)))

;;;###autoload
(defun highlights-pdf-import-json-directory (directory)
  "Import all extractor JSON files in DIRECTORY.
Prompts once per file for its destination org-roam node."
  (interactive
   (list (read-directory-name "Directory with PDF-annotation JSON files: "
                              annotation-default-json-directory)))
  (let ((files (highlights-pdf--find-json-files directory))
        (count 0))
    (if (null files)
        (message "No JSON files found in %s" directory)
      (dolist (file files)
        (condition-case err
            (progn
              (highlights-pdf--import-json-file file)
              (setq count (1+ count)))
          (error
           (annotation-debug 0 "Error importing %s: %s"
                             file (error-message-string err)))))
      (message "Imported annotations from %d files" count))))

(defvar highlights-pdf--python-checked nil
  "Cache: non-nil once `highlights-pdf-python' was verified to import PyMuPDF.
Reset it (or restart Emacs) after changing the interpreter or its packages.")

(defun highlights-pdf--ensure-python ()
  "Signal a clear error unless `highlights-pdf-python' can import PyMuPDF.
The result is cached in `highlights-pdf--python-checked' so the check
runs at most once per session.  This turns a raw ModuleNotFoundError
traceback in the process buffer into an actionable message."
  (unless highlights-pdf--python-checked
    (unless (executable-find highlights-pdf-python)
      (user-error "Python interpreter not found: %s (set `highlights-pdf-python')"
                  highlights-pdf-python))
    (unless (eq 0 (call-process highlights-pdf-python nil nil nil
                                "-c" "import pymupdf"))
      (user-error
       "PyMuPDF not importable by %s; run: %s -m pip install -r requirements.txt"
       highlights-pdf-python highlights-pdf-python))
    (setq highlights-pdf--python-checked t)))

;;;###autoload
(defun highlights-pdf-import-pdf (pdf)
  "Run the extractor on PDF, then import the resulting JSON.
Writes JSON (and any figure PNGs) next to PDF, then prompts for the
destination org-roam node.  Requires `highlights-pdf-extractor-script'
and a `highlights-pdf-python' with PyMuPDF installed."
  (interactive (list (read-file-name "PDF file: " nil nil t nil
                                     (lambda (f) (or (file-directory-p f)
                                                     (string-match-p "\\.pdf\\'" f))))))
  (unless (file-exists-p highlights-pdf-extractor-script)
    (user-error "Extractor script not found: %s" highlights-pdf-extractor-script))
  (highlights-pdf--ensure-python)
  (let* ((json (concat (file-name-sans-extension pdf) ".json"))
         (status (call-process highlights-pdf-python nil "*pdf-annotation-extractor*" nil
                               highlights-pdf-extractor-script (expand-file-name pdf))))
    (unless (eq status 0)
      (user-error "Extractor failed (exit %s); see *pdf-annotation-extractor*" status))
    (highlights-pdf--import-json-file json)
    (message "Imported annotations from %s" (file-name-nondirectory pdf))))

(provide 'highlights-pdf-backend)
;;; highlights-pdf-backend.el ends here
