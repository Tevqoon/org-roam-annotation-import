;;; org-roam-annotation-import.el --- Sync annotation highlights with Org-roam -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.3
;; Package-Requires: ((emacs "27.1") (org "9.4") (org-roam "2.0"))
;; Keywords: org, org-roam, annotations

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
;; Annotations are exported as anki-editor flashcards: each annotation
;; heading carries ANKI_NOTE_TYPE / ANKI_DECK / ANKI_TAGS properties,
;; with a `Front' subheading (quote + source link) and an optional
;; `Hint' subheading (the user's note).
;;
;; Backends (wallabag, koreader, ...) produce entry plists which this
;; module writes into org-roam nodes.

;;; Code:

(require 'org)
(require 'org-roam)
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

;;;; Org-roam node access

(defun annotation--org-roam-node-open-or-create (node)
  "Find and open or create an Org-roam NODE."
  (if (org-roam-node-file node)
      (org-roam-node-visit node nil t)
    (org-roam-capture-
     :node node
     :templates '(("d" "default" plain "%?"
                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: content")
                   :unnarrowed t
                   :immediate-finish t))
     :props '(:finalize find-file)))
  (current-buffer))

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

(defun annotation--set-anki-properties (tags)
  "Set ANKI_NOTE_TYPE, ANKI_DECK, ANKI_TAGS on the heading at point.
TAGS is a space-separated tag string."
  (org-set-property "ANKI_NOTE_TYPE" annotation-anki-note-type)
  (org-set-property "ANKI_DECK" annotation-anki-deck)
  (when (and tags (not (string-empty-p tags)))
    (org-delete-property "ANKI_TAGS")
    (org-set-property "ANKI_TAGS" tags)))

;;;; Annotation processing

(defun annotation--id-matches-p (annotation-id)
  "Predicate: heading at point has matching Annotation-ID property."
  (lambda ()
    (let ((heading-id (org-entry-get nil "Annotation-ID")))
      (and heading-id (string= heading-id annotation-id)))))

(defun annotation--write-annotation-content (annotation entry-title entry-url)
  "Write properties + Front + Hint into the annotation heading at point.
ANNOTATION is the plist. ENTRY-TITLE, ENTRY-URL are parent entry data."
  (let* ((updated-at (plist-get annotation :updated-at))
         (quote      (plist-get annotation :quote))
         (text       (plist-get annotation :text))
         (chapter    (plist-get annotation :chapter))
         (page       (plist-get annotation :page))
         (source     (plist-get annotation :source))
         (front-body (annotation--format-front-body quote entry-url entry-title))
         (tags       (annotation--current-outline-tags entry-title)))
    (when updated-at (org-set-property "Updated-at" updated-at))
    (when source     (org-set-property "Source"     source))
    (when chapter    (org-set-property "Chapter"    chapter))
    (when page       (org-set-property "Page"       (format "%s" page)))
    (annotation--set-anki-properties tags)
    (annotation--upsert-child-heading "Front" front-body)
    (annotation--upsert-child-heading "Hint"  text)))

(defun annotation--process-annotation (annotation entry-title entry-url)
  "Process ANNOTATION in the container heading at point.
Find existing by id and update, or insert new."
  (let* ((annotation-id (annotation--id-as-string (plist-get annotation :id)))
         (parent-level  (or (org-current-level) 0)))
    (save-excursion
      (if (annotation--org-find-subheading
           (annotation--id-matches-p annotation-id))
          ;; Existing annotation: point now on it. Rewrite its content.
          (annotation--write-annotation-content annotation entry-title entry-url)
        ;; New annotation: insert a heading, then move point to its
        ;; start so writes target it.
        (let (new-heading-pos)
          (save-excursion
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (setq new-heading-pos (point))
            (insert (make-string (1+ parent-level) ?*) " Annotation\n"))
          (goto-char new-heading-pos)
          (org-set-property "Annotation-ID" annotation-id)
          (annotation--write-annotation-content annotation entry-title entry-url))))))

;;;; Chapter container

(defun annotation--enter-chapter-container (annotation)
  "If ANNOTATION has a non-empty :chapter, descend into (creating if needed) a child of that name."
  (let ((chapter (plist-get annotation :chapter)))
    (when (and chapter (not (string-empty-p chapter)))
      (annotation--goto-or-insert-child chapter))))

;;;; Entry processing

(defun annotation--process-entry (entry)
  "Find or create a node for ENTRY, then upsert each of its annotations."
  (save-window-excursion
    (let* ((url         (plist-get entry :url))
           (title       (plist-get entry :title))
           (annotations (plist-get entry :annotations))
           (node (or (when url (org-roam-node-from-ref url))
                     (org-roam-node-from-title-or-alias title)
                     (org-roam-node-create :title title
                                           :refs (when url (list url))
                                           :id   (org-id-new))))
           (buffer (annotation--org-roam-node-open-or-create node)))
      (with-current-buffer buffer
        (save-excursion
          (let ((heading-level (org-current-level)))
            (when heading-level (org-narrow-to-subtree))
            (when url (org-roam-ref-add url))
            (org-roam-tag-add '("annotations"))
	    (when-let ((author (plist-get entry :author))
		       (slug   (annotation--slugify author)))
	      (org-roam-tag-add (list slug)))

            ;; Position at the start of the node's scope before
            ;; finding/inserting the Annotations container.
            (if heading-level
                (org-back-to-heading t)
              (goto-char (point-min)))
            (annotation--goto-or-insert-child "Annotations")
            (dolist (annotation annotations)
              (save-excursion
                (annotation--enter-chapter-container annotation)
                (annotation--process-annotation annotation title url)))
            (save-buffer)
            (widen)))))))

(defun annotation--update-entries (entries)
  "Update each entry in ENTRIES."
  (dolist (entry entries)
    (annotation--process-entry entry)))

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
