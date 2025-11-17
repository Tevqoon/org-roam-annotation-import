;;; org-roam-annotation-import.el --- Sync Readwise highlights with Org-mode -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.1
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

;; Original:
;; This package integrates annotation highlight syncing with Org-mroam.
;; It provides commands to import
;; them into an Org buffer or a specified file.

;;; Code:

;;; org-roam-annotation-import.el --- Import annotations from various sources -*- lexical-binding: t; -*-

(require 'org)
(require 'org-roam)

(defgroup annotation nil
  "Import annotations into org-roam."
  :group 'annotation
  :prefix "annotation-")

(defcustom annotation-debug-level 0
  "Debug level for the org-readwise package.
0 - No debug output.
1 - Basic debug output.
2 - Detailed debug output."
  :group 'annotation
  :type 'integer)

(defun annotation-debug (level msg &rest args)
  "Print debug message MSG at debug LEVEL with ARGS."
  (when (>= annotation-debug-level level)
    (apply 'message (concat "[Annotations] " msg) args)))

(defun annotation--org-roam-node-open-or-create (node)
  "Find and open or create an Org-roam NODE."
  (if (org-roam-node-file node)
      (org-roam-node-visit node nil t)
    (org-roam-capture-
     :node node
     :templates '(("d" "default" plain "%?"
		   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: content")
		   :unnarrowed t
		   :immediate-finish t)
		  )
     :props '(:finalize find-file)))
  (current-buffer))

(defun annotation--insert-heading-at-point (level title &optional body annotation-id updated-at)
  "A helper function to inserting a heading of a given level at point.
LEVEL is the level to insert to. If nil, insert a top level heading.
TITLE is the title of the heading.
BODY is the underneath the heading.
ANNOTATION-ID is the annotation id used for annotation bookkeeping.
UPDATED-AT is a timestamp of when the annotation was last updated.

It is assumed the point is at the proper spot, eiher after cutting a previous
subtree or at the end of the outline path.

Leaves the point at the end of the current subtree to facilitate either
easy subheading insertion or insertion of the next heading of the same level."
  
  (let ((stars (make-string (+ 1 (or level 0)) ?*)))
    ;; Insert heading
    (save-excursion
      (insert stars " " title "\n")
      (when annotation-id
	(org-set-property "Annotation-ID" annotation-id))
      (when updated-at
	(org-set-property "Updated-at" updated-at))
      (when body
	(insert body "\n")))
    ))

(defun annotation--org-find-subheading (predicate)
  "Find direct subheading satisfying PREDICATE under current heading/file.
PREDICATE is called with point at each subheading.
If found, moves point to the subheading and returns t.
If not found, moves point to insertion location (end of current subtree)
and returns nil."
  (let* ((start-pos (point))
         (target-level (1+ (or (org-current-level) 0)))
         (limit (save-excursion 
                  (org-end-of-subtree t t)
                  (point)))
         (found-pos nil))
    ;; Search within our subtree only
    (save-excursion
      (while (and (not found-pos)
                  (outline-next-heading)
                  (< (point) limit))
        (when (and (= (org-current-level) target-level)
                   (funcall predicate))
          (setq found-pos (point)))))
    ;; Move to found position or insertion point
    (if found-pos
        (progn (goto-char found-pos) t)
      (goto-char start-pos)
      (org-end-of-subtree t t)
      nil)))

(defun annotation--is-heading-p (heading)
  "A predicate to see if the heading at point is titled HEADING."
  (lambda ()
    (string= (org-get-heading t t t t) heading)))

(defun annotation--goto-or-insert-heading (heading &optional body)
  "Find a HEADING of one level lower than current of a given name.
Go there. If it doesn't exist, create it first. Optionally insert a BODY.

Return t if the heading was found as opposed to created anew."
  (let ((level (or (org-current-level) 0))
	(found (annotation--org-find-subheading (annotation--is-heading-p heading))))
    (unless found
      (if body
	  (annotation--insert-heading-at-point level heading body)
	(annotation--insert-heading-at-point level heading)))
    found))

(defun annotation--delete-heading (heading)
  "Find a HEADING of one level lower than current of a given name.
If it exists, delete it."
  (save-excursion
    (let ((level (or (org-current-level) 0)))
      (when (annotation--org-find-subheading (annotation--is-heading-p heading))
	(org-cut-subtree)))))

(defun annotation--id-matches-p (annotation-id)
  "Return predicate to check if a heading has matching ANNOTATION-ID property."
  (lambda ()
    (let ((heading-id (org-entry-get nil "annotation-id")))
      (and heading-id
           (string= heading-id annotation-id)))))

(defun annotation--org-replace-heading-text (new-text)
  "Replace text content under current heading, preserving subheadings.
Replaces everything between heading line and first subheading (or end).
NEW-TEXT is inserted as the new content."
  (save-excursion
    (org-end-of-meta-data t)
    (let ((start (point)))
      (outline-next-heading)
      (delete-region start (point))
    (insert new-text "\n"))
  ))

(defun annotation--process-annotation (annotation &optional heading-level)
  "Process the current ANNOTATION.
Assumes the point is in the correct narrowed buffer.

For a given ANNOTATION, either update its extant version
or insert it as a new heading.

If HEADING-LEVEL is not provided, it is assumed to be 0, i.e. of a file node."
  (let* ((heading-level (or heading-level 0))
         (annotation-id (number-to-string (plist-get annotation :id)))
         (quote (plist-get annotation :quote))
         (text (plist-get annotation :text))
         (updated-at (plist-get annotation :updated-at)))

    (save-excursion
      (let ((pred (annotation--id-matches-p annotation-id)))
	(if (annotation--org-find-subheading pred)
	    ;; Update existing annotation
	    ;; In particular, keep potentially custom heading title while replacing text only.
	    (let ((timestamp (org-entry-get nil "updated-at")))
	      (when (or (not timestamp)
			(time-less-p (parse-iso8601-time-string timestamp)
				     (parse-iso8601-time-string updated-at))) 
		(org-set-property "updated-at" updated-at)
		(annotation--org-replace-heading-text quote)
		(when (or (null text)
			  (string-empty-p text))
		  (annotation--delete-heading "Note"))
		(and text (not (string-empty-p text))
		     (annotation--goto-or-insert-heading "Note" text)
		     (annotation--org-replace-heading-text text))))
	  ;; Insert new annotation
	  (annotation--insert-heading-at-point (+ 1 heading-level) "Annotation" quote annotation-id updated-at)
	  (when (and text (not (string-empty-p text)))
	    (annotation--insert-heading-at-point (+ 2 heading-level) "Note" text)))))))

;; Quadratic for now, searches linearly through all annotations for each entry.
;; Will see how it performs for a book with many, likely ok.
;; TODO: Monitor quadratic search
(defun annotation--process-entry (entry)
  "Find or create node for our ENTRY, update annotations, insert new ones.

For every annotation, first get or create the file/relevant entry.
Then save the point at the beginning of the relevant search area."
  (save-window-excursion
    (let* ((heading-level nil)
	   (url (plist-get entry :url))
	   (title (plist-get entry :title))
	   (node (or (when url (org-roam-node-from-ref url))
		     (org-roam-node-from-title-or-alias title)
		     (org-roam-node-create :title title :refs (list url) :id (org-id-new))))
	   (buffer (annotation--org-roam-node-open-or-create node))
	   (annotations (plist-get entry :annotations)))

      (with-current-buffer buffer
	(save-excursion
	  ;; Record our heading level to differentiate files from headings
	  (setq heading-level (org-current-level)) ; nil if file

	  (when heading-level
	    (org-narrow-to-subtree))

	  ;; Ensure we have all the right data to proceed
	  (when url
	    (org-roam-ref-add url))
	  (org-roam-tag-add '("annotations"))
	  (annotation--goto-or-insert-heading "Annotations")

	  ;; Process each annotation in turn
	  (dolist (annotation annotations)
	    (annotation--process-annotation annotation heading-level))

	  (save-buffer)
	  (widen)
	  )))))

(defun annotation--update-entries (entries)
  "Update each entry in the list of ENTRIES."
  (dolist (entry entries)
	  (annotation--process-entry entry)))

(provide 'org-roam-annotation-import)
;;; org-roam-annotation-import.el ends here.


