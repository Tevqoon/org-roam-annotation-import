;;; org-roam-annotation-import.el --- Sync annotation highlights with vulpea -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.5
;; Package-Requires: ((emacs "29.1") (org "9.4") (vulpea "2.0"))
;; Keywords: org, vulpea, annotations

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates annotation highlight syncing with Org-roam.
;; Annotations are optionally exported as anki-editor flashcards: each
;; annotation heading carries ANKI_NOTE_TYPE / ANKI_DECK / ANKI_TAGS
;; properties only when the annotation plist contains :anki t.
;;
;; The content written for each annotation is controlled by :write-fn
;; in the annotation plist.  If absent, the default
;; `annotation--write-annotation-content' is used, which produces the
;; standard Front/Hint heading structure.  Backends that want different
;; structure (e.g. Zotero) supply their own writer.
;;
;; Backends (wallabag, zotero, koreader, ...) produce entry plists
;; which this module writes into vulpea notes.

;;; Code:

(require 'org)
(require 'vulpea)
(require 'subr-x)

(defgroup annotation nil
  "Import annotations into org-roam."
  :group 'annotation
  :prefix "annotation-")

(defcustom annotation-debug-level 0
  "Debug level for annotation imports.
0 - No debug output.
1 - Basic debug output.
2 - Detailed debug output."
  :group 'annotation
  :type 'integer)

(defcustom annotation-anki-note-type "Basic"
  "Anki note type used for exported annotations."
  :group 'annotation
  :type 'string)

(defcustom annotation-anki-deck "Annotations"
  "Anki deck used for exported annotations."
  :group 'annotation
  :type 'string)

(defcustom annotation-default-json-directory nil
  "Default directory containing JSON annotation exports.
Used by file-based backends (e.g. KOReader) as the starting point
for interactive file/directory prompts.  nil means no default."
  :group 'annotation
  :type '(choice (directory :tag "Directory") (const :tag "None" nil)))

(defcustom annotation-auto-manual t
  "When non-nil, stamp :Manual: t on each annotation as it is created.
This freezes the quote/Front body immediately after the first import,
so later re-syncs never overwrite it — useful when imported text needs
hand-correction (e.g. garbled LaTeX from PDF OCR).  Notes/Hint and
properties continue to sync regardless.  Set to nil to import without
the guard and add :Manual: t manually only where wanted."
  :group 'annotation
  :type 'boolean)

(defun annotation-debug (level msg &rest args)
  "Print debug message MSG at debug LEVEL with ARGS."
  (when (>= annotation-debug-level level)
    (apply 'message (concat "[Annotations] " msg) args)))

;;;; Slugification & tag derivation

(defun annotation--slugify (s)
  "Aggressively slugify S for use as an Anki tag.
Replaces whitespace with underscores, strips quotes, brackets, commas,
slashes, semicolons. Keeps colons (Anki uses :: for hierarchy, single
colon is safe), hyphens, periods, and alphanumerics."
  (when s
    (let* ((s (string-trim s))
           (s (replace-regexp-in-string
               "[\"'`\u2018\u2019\u201c\u201d,;/\\\\()\\[\\]{}<>!?*|]" "" s))
           (s (replace-regexp-in-string "[[:space:]]+" "_" s))
           (s (replace-regexp-in-string "_+" "_" s))
           (s (replace-regexp-in-string "\\`_+\\|_+\\'" "" s)))
      s)))

(defun annotation--current-outline-tags (entry-title)
  "Collect tag context for the annotation heading at point."
  (let* ((path (org-get-outline-path))
         (tail (member "Annotations" path))
         (after-annotations (if tail (cdr tail) path))
         (all (append (and entry-title (list entry-title)) after-annotations))
         (slugs (delq nil (mapcar #'annotation--slugify all)))
         (slugs (delete "" slugs))
         (slugs (delete-dups slugs)))
    (string-join slugs " ")))

;;;; Entry-plist value coercion

(defun annotation--id-as-string (id)
  "Return ID as a string. Accepts integers, strings, or nil."
  (cond
   ((null id) nil)
   ((stringp id) id)
   ((numberp id) (number-to-string id))
   (t (format "%s" id))))

(defun annotation--generate-id (backend &rest parts)
  "Generate a stable annotation ID for BACKEND from PARTS.
PARTS are hashed together so that the same content always yields
the same ID across re-imports.  Use this for sources that lack a
stable server-side identifier (KOReader, plain-text, clipboard).

The BACKEND name is prefixed (and folded into the hash) so IDs from
different backends can never collide.  nil parts are ignored."
  (let* ((clean (delq nil (mapcar (lambda (p)
                                    (and p (format "%s" p)))
                                  parts)))
         (payload (mapconcat #'identity (cons backend clean) "\0"))
         (digest  (secure-hash 'sha1 payload)))
    (format "%s-%s" backend (substring digest 0 16))))

;;;; Vulpea note access

(defcustom annotation-new-note-file-name "%<%Y%m%d%H%M%S>-${slug}.org"
  "`vulpea-create' FILE-NAME template for a newly created annotation note.
Backends may let-bind this (see `annotation--vulpea-note-open-or-create's
FILE-NAME-TEMPLATE argument) to file new notes somewhere specific, e.g.
a literature backend filing under references/ by citekey."
  :group 'annotation
  :type 'string)

(defcustom annotation-new-note-head "#+startup: content"
  "`vulpea-create' :head template for a newly created annotation note."
  :group 'annotation
  :type 'string)

(defun annotation--vulpea-note-by-ref (url)
  "Find a vulpea note whose ROAM_REFS property contains URL.
`vulpea-db-query-by-property' matches a property's whole value exactly,
so a note with several space-separated ROAM_REFS wouldn't be found by
matching a single URL against it -- filter after the fact instead."
  (seq-find
   (lambda (note)
     (member url (split-string
                  (or (cdr (assoc "ROAM_REFS" (vulpea-note-properties note))) "")
                  " " t)))
   (vulpea-db-query-by-property-key "ROAM_REFS")))

(defun annotation--vulpea-note-by-title-or-alias (title)
  "Find a vulpea note whose title or an alias matches TITLE exactly."
  (seq-find
   (lambda (note)
     (or (string= (vulpea-note-title note) title)
         (member title (vulpea-note-aliases note))))
   (vulpea-db-query)))

(defun annotation--vulpea-note-at-point ()
  "Return the vulpea note for the ID at or above point, or nil."
  (when-let* ((id (org-entry-get nil "ID" t)))
    (vulpea-db-get-by-id id)))

(defun annotation--vulpea-add-ref (url)
  "Add URL to the ROAM_REFS property at point (file- or heading-level),
unless it's already present."
  (let* ((existing (org-entry-get nil "ROAM_REFS"))
         (refs (if existing (split-string existing " " t) nil)))
    (unless (member url refs)
      (org-entry-put nil "ROAM_REFS" (string-join (append refs (list url)) " ")))))

(defun annotation--vulpea-note-open-or-create (existing title url &optional file-name-template head)
  "Return a buffer visiting the vulpea note for an annotation entry.
EXISTING is an already-resolved `vulpea-note', or nil to create a new
note titled TITLE (with ROAM_REFS URL, if URL is non-nil).
FILE-NAME-TEMPLATE and HEAD override `annotation-new-note-file-name'
and `annotation-new-note-head' for this call only."
  (find-file-noselect
   (vulpea-note-path
    (or existing
        (vulpea-create title
                        (or file-name-template annotation-new-note-file-name)
                        :head (or head annotation-new-note-head)
                        :properties (when url (list (cons "ROAM_REFS" url))))))))

;;;; Heading helpers

(defun annotation--org-find-subheading (predicate)
  "Find direct subheading of current heading where PREDICATE returns non-nil.
PREDICATE is called with point at each candidate subheading.
If found, move point to the subheading and return t.
If not found, leave point at the original position and return nil."
  (let* ((start-pos (point))
         (target-level (1+ (or (org-current-level) 0)))
         (limit (save-excursion
                  (if (org-current-level)
                      (org-end-of-subtree t t)
                    (goto-char (point-max)))
                  (point)))
         (found-pos nil))
    (save-excursion
      (when (org-current-level) (org-back-to-heading t))
      (while (and (not found-pos)
                  (outline-next-heading)
                  (< (point) limit))
        (when (and (= (org-current-level) target-level)
                   (funcall predicate))
          (setq found-pos (point)))))
    (if found-pos
        (progn (goto-char found-pos) t)
      (goto-char start-pos)
      nil)))

(defun annotation--is-heading-p (heading)
  "Predicate: heading at point is titled HEADING (case-sensitive)."
  (lambda ()
    (string= (org-get-heading t t t t) heading)))

(defun annotation--replace-heading-body (new-body)
  "Replace body text under heading at point, preserving subheadings.
Deletes everything between heading meta-data and first child heading
\(or end of subtree), inserts NEW-BODY in its place. Point is preserved."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let* ((start (point))
           (end (save-excursion
                  (if (outline-next-heading)
                      (point)
                    (point-max)))))
      (delete-region start end)
      (when (and new-body (not (string-empty-p new-body)))
        (insert new-body "\n")))))

(defun annotation--insert-child-heading (title body)
  "Insert a child heading TITLE with BODY under heading at point.
The new heading is one level deeper, inserted at end-of-subtree.
Point is preserved."
  (let ((parent-level (or (org-current-level) 0)))
    (save-excursion
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert (make-string (1+ parent-level) ?*) " " title "\n")
      (when (and body (not (string-empty-p body)))
        (insert body "\n")))))

(defun annotation--delete-child-heading (title)
  "Delete child heading TITLE under heading at point, if it exists."
  (save-excursion
    (when (annotation--org-find-subheading (annotation--is-heading-p title))
      (org-cut-subtree))))

(defun annotation--upsert-child-heading (title body)
  "Ensure a child heading TITLE exists under heading at point with BODY.
If TITLE exists, replace its body. Otherwise, create it with BODY.
If BODY is nil/empty, delete TITLE instead."
  (if (and body (not (string-empty-p body)))
      (save-excursion
        (if (annotation--org-find-subheading (annotation--is-heading-p title))
            (annotation--replace-heading-body body)
          (annotation--insert-child-heading title body)))
    (annotation--delete-child-heading title)))

(defun annotation--goto-or-insert-child (heading)
  "Ensure a direct child heading HEADING exists under the heading at point.
Move point onto it. Returns t if it pre-existed, nil if newly created."
  (if (annotation--org-find-subheading (annotation--is-heading-p heading))
      t
    (annotation--insert-child-heading heading nil)
    (annotation--org-find-subheading (annotation--is-heading-p heading))
    nil))

;;;; Front body formatting & Anki properties

(defun annotation--format-front-body (quote url entry-title)
  "Build the Front field body. Combines QUOTE with a Source link.
URL may be nil. ENTRY-TITLE is used as the link description."
  (let* ((quote (or quote ""))
         (source-line
          (cond
           ((and url entry-title) (format "Source: [[%s][%s]]" url entry-title))
           (url                   (format "Source: [[%s]]" url))
           (entry-title           (format "Source: %s" entry-title))
           (t                     nil))))
    (if source-line
        (concat quote "\n\n" source-line)
      quote)))

(defun annotation--resolve-anki-deck (anki)
  "Resolve an :anki plist value ANKI to a deck name string, or nil.
nil/absent  -> nil   (do not ankify)
t           -> `annotation-anki-deck' (the default deck)
a string    -> that string (a non-default deck)"
  (cond
   ((null anki) nil)
   ((eq anki t) annotation-anki-deck)
   ((stringp anki) anki)
   (t annotation-anki-deck)))

(defun annotation--set-anki-properties (tags deck)
  "Set ANKI_NOTE_TYPE, ANKI_DECK (to DECK), ANKI_TAGS on the heading at point."
  (unless (equal (org-entry-get nil "ANKI_NOTE_TYPE") annotation-anki-note-type)
    (org-set-property "ANKI_NOTE_TYPE" annotation-anki-note-type))
  (when (and deck (not (equal (org-entry-get nil "ANKI_DECK") deck)))
    (org-set-property "ANKI_DECK" deck))
  (when (and tags (not (string-empty-p tags))
             (not (equal (org-entry-get nil "ANKI_TAGS") tags)))
    (org-set-property "ANKI_TAGS" tags)))

;;;; Default annotation content writer

(defun annotation--write-annotation-content (annotation entry-title entry-url)
  "Write properties + Front + Hint into the annotation heading at point.
ANNOTATION is the plist. ENTRY-TITLE, ENTRY-URL are parent entry data.

When the heading has a Manual property of t, the Front body is left
untouched (so hand-edited content — e.g. corrected LaTeX — survives
re-import).  Hint and properties are still refreshed from the source.

Anki properties are set when ANNOTATION's :anki value is non-nil:
t uses the default deck (`annotation-anki-deck'), a string names a
specific deck.  If ANNOTATION contains :write-fn, that function is
called instead of this one — see `annotation--process-annotation'."
  (let* ((updated-at (plist-get annotation :updated-at))
         (quote      (plist-get annotation :quote))
         (text       (plist-get annotation :text))
         (chapter    (plist-get annotation :chapter))
         (page       (plist-get annotation :page))
         (source     (plist-get annotation :source))
         (manual-p   (string= "t" (org-entry-get nil "Manual")))
         (deck       (annotation--resolve-anki-deck (plist-get annotation :anki)))
         (front-body (annotation--format-front-body quote entry-url entry-title))
         (tags       (annotation--current-outline-tags entry-title)))
    (when updated-at (org-set-property "Updated-at" updated-at))
    (when source     (org-set-property "Source"     source))
    (when chapter    (org-set-property "Chapter"    chapter))
    (when page       (org-set-property "Page"       (format "%s" page)))
    ;; Anki export: deck resolved from :anki (nil disables)
    (when deck
      (annotation--set-anki-properties tags deck))
    ;; Front: protected when Manual is set
    (unless manual-p
      (annotation--upsert-child-heading "Front" front-body))
    (annotation--upsert-child-heading "Hint"  text)))

;;;; Annotation processing

(defun annotation--entry-updated-at (entry)
  "Return the latest annotation :updated-at in ENTRY, or nil.
Only annotation timestamps are considered, since those are what get
stored as `Updated-at' properties; the entry's own :updated-at is
ignored because it is never persisted and drifts independently."
  (let ((times (delq nil
                     (mapcar (lambda (a) (plist-get a :updated-at))
                             (plist-get entry :annotations)))))
    (car (sort times #'string>))))

(defun annotation--id-matches-p (annotation-id)
  "Predicate: heading at point has matching Annotation-ID property."
  (lambda ()
    (let ((heading-id (org-entry-get nil "Annotation-ID")))
      (and heading-id (string= heading-id annotation-id)))))

(defun annotation--process-annotation (annotation entry-title entry-url)
  "Process ANNOTATION in the container heading at point.
Find existing by id and update, or insert new.

The content writer is determined by :write-fn in ANNOTATION.
If absent, `annotation--write-annotation-content' is used."
  (let* ((annotation-id (annotation--id-as-string (plist-get annotation :id)))
         (parent-level  (or (org-current-level) 0))
         (write-fn      (or (plist-get annotation :write-fn)
                            #'annotation--write-annotation-content)))
    (save-excursion
      (if (annotation--org-find-subheading
           (annotation--id-matches-p annotation-id))
          ;; Existing annotation: check if it actually changed.
          (let* ((stored-updated-at   (org-entry-get nil "Updated-at"))
                 (incoming-updated-at (plist-get annotation :updated-at)))
            (if (and stored-updated-at
                     incoming-updated-at
                     (string= stored-updated-at incoming-updated-at))
                (annotation-debug 1 "Skipping unchanged annotation %s" annotation-id)
              (funcall write-fn annotation entry-title entry-url)))
        ;; New annotation: insert heading, then write content.
        (let (new-heading-pos)
          (save-excursion
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (setq new-heading-pos (point))
            (insert (make-string (1+ parent-level) ?*) " Annotation\n"))
          (goto-char new-heading-pos)
          (org-set-property "Annotation-ID" annotation-id)
          ;; Write the body FIRST (the Manual guard must see no flag yet),
          ;; then stamp Manual so subsequent syncs leave the quote alone.
          (funcall write-fn annotation entry-title entry-url)
          (when annotation-auto-manual
            (org-set-property "Manual" "t")))))))

;;;; Chapter container

(defun annotation--enter-chapter-container (annotation)
  "If ANNOTATION has a non-empty :chapter, descend into (creating if needed) a child of that name."
  (let ((chapter (plist-get annotation :chapter)))
    (when (and chapter (not (string-empty-p chapter)))
      (annotation--goto-or-insert-child chapter))))

;;;; Entry processing

(defun annotation--max-stored-updated-at ()
  "Return the latest Updated-at property value in the current buffer, or nil."
  (let ((times nil))
    (org-map-entries
     (lambda ()
       (when-let ((ts (org-entry-get nil "Updated-at")))
         (push ts times)))
     nil 'file)
    (car (sort times #'string>))))

(defun annotation--process-entry (entry)
  "Find or create a note for ENTRY, then upsert each of its annotations.
If ENTRY carries a :note, that note is used directly and the
title/url-based resolution is skipped."
  (save-window-excursion
    (let* ((url         (plist-get entry :url))
           (title       (plist-get entry :title))
           (annotations (plist-get entry :annotations))
           (incoming-updated-at (annotation--entry-updated-at entry))
           (existing (or (plist-get entry :note)
                        (and url (annotation--vulpea-note-by-ref url))
                        (annotation--vulpea-note-by-title-or-alias title)))
           (buffer (annotation--vulpea-note-open-or-create existing title url)))
      (with-current-buffer buffer
        (let ((stored-updated-at (annotation--max-stored-updated-at)))
          (unless (and stored-updated-at
                       incoming-updated-at
                       (string= stored-updated-at incoming-updated-at))
            (save-excursion
              (let ((heading-level (org-current-level)))
                (when heading-level (org-narrow-to-subtree))
                (if heading-level
                    (org-back-to-heading t)
                  (goto-char (point-min)))
                (when (and url existing
                           (not (member url (split-string
                                             (or (cdr (assoc "ROAM_REFS" (vulpea-note-properties existing))) "")
                                             " " t))))
                  (annotation--vulpea-add-ref url))
                (vulpea-buffer-tags-add
                 (delq nil (list "annotations" (plist-get entry :source-tag))))
                (when-let ((author (plist-get entry :author))
                           (slug   (annotation--slugify author)))
                  (vulpea-buffer-tags-add (list slug)))
                (annotation--goto-or-insert-child "Annotations")
                (dolist (annotation annotations)
                  (save-excursion
                    (annotation--enter-chapter-container annotation)
                    (annotation--process-annotation annotation title url)))
                (widen)
                ;; Reaching here means the entry's annotations were
                ;; (re)written; save and report the file as modified.
                (save-buffer)
                (buffer-file-name)))))))))

(defvar annotation--recently-modified-files nil
  "List of files modified by the most recent `annotation--update-entries' call.")

(defun annotation--update-entries (entries)
  "Update each entry in ENTRIES.
Stores the set of modified file paths in `annotation--recently-modified-files'."
  (setq annotation--recently-modified-files nil)
  (dolist (entry entries)
    (when-let ((file (annotation--process-entry entry)))
      (push file annotation--recently-modified-files)))
  (setq annotation--recently-modified-files
        (delete-dups (nreverse annotation--recently-modified-files)))
  (message "Annotation sync done: %d file(s) modified"
           (length annotation--recently-modified-files)))

;;;; Bulk re-import support

;;;###autoload
(defun annotation-wipe-buffer ()
  "Delete the `* Annotations' subtree in the current buffer.
Useful before re-importing from scratch via a backend.
Saves the buffer after wiping."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\* Annotations[ \t]*$" nil t)
        (progn
          (beginning-of-line)
          (org-cut-subtree)
          (save-buffer)
          (message "Wiped Annotations subtree in %s"
                   (or (buffer-file-name) (buffer-name))))
      (message "No `* Annotations' heading found in %s"
               (or (buffer-file-name) (buffer-name))))))

(provide 'org-roam-annotation-import)
;;; org-roam-annotation-import.el ends here.
