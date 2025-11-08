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
  (when (>= readwise-debug-level level)
    (apply 'message (concat "[Readwise] " msg) args)))

(defun annotation--sanitize-filename (title)
  "Convert TITLE to a safe filename."
  (let ((sanitized (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" title)))
    (replace-regexp-in-string "\\s+" "-" (string-trim sanitized))))

(defun annotation--get-target-for-entry (entry)
  "For a given ENTRY, find the node containing it, creating it if need be.
Assumes that the entry has at least one relevant annotation. In particular,
if a file needs to be created, it will be, irrespective of if the annotations
it contains is an empty list.

The target node can either be a file or a heading. We return the level
of the heading, defaulting to nil if we are at the top level,
mirroring `org-current-level'.

To select the node, the following order is applied:
1. The :url attribute, if extant, using `org-roam-node-from-ref'.
2. The sanitized :title attribute, if extant.
3. In case none of the attributes are present, we return nil silently.
   A message is logged. This is taken to mean that no annotations should
   be inserted. Something went wrong if we have neither url nor title.

If a new node is created, it gets the filetag #annotations for roam-db interops."
  
  (let* ((url (plist-get entry :url))
         (title (plist-get entry :title))
         (node (when url (org-roam-node-from-ref url)))
         (node-file nil)
         (node-id nil)
         (heading-level nil))
    (cond
     ;; Case 1: Node exists via URL lookup
     (node
      (setq node-file (org-roam-node-file node))
      (setq node-id (org-roam-node-id node))
      (with-current-buffer (find-file-noselect node-file)
        (org-with-point-at (org-roam-node-point node)
          (setq heading-level (org-current-level)))))
     
     ;; Case 2: Need to create new node
     ((and title url)
      (let* ((sanitized-title (annotation--sanitize-filename title))
	    (filename (concat sanitized-title ".org"))
            (filepath (expand-file-name
                       filename
                       org-roam-directory)))
        ;; Ensure directory exists
        (unless (file-exists-p (file-name-directory filepath))
          (make-directory (file-name-directory filepath) t))
        
        ;; Create file with org-roam properties
        (with-current-buffer (find-file-noselect filepath)
          (erase-buffer)
          (org-mode)
          (org-set-property "ID" (org-id-new))
          (org-set-property "ROAM_REFS" url)
	  (insert (format "#+title: " title))
          (org-roam-tag-add "annotations")
          (save-buffer)
          (setq node-file filepath)
          (setq node-id (org-id-get))
          (setq heading-level 1))))
     
     ;; Case 3: Missing required attributes
     (t
      (annotation-debug 1 "Entry missing both url and title, skipping")
      (setq node-file nil)))
    
    ;; Return target plist or nil
    (when node-file
      (list :node-file node-file
            :node-id node-id
            :node-kind heading-level))))

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
  
  (let ((stars (make-string (or level 1) ?*)))
    ;; Insert heading
    (insert stars " " title "\n")
    
    ;; Insert properties if provided
    (when (or annotation-id updated-at)
      (insert ":PROPERTIES:\n")
      (when annotation-id
        (insert (format ":ANNOTATION-ID: %s\n" annotation-id)))
      (when updated-at
        (insert (format ":UPDATED-AT: %s\n" updated-at)))
      (insert ":END:\n"))
    
    ;; Insert body if provided
    (when body
      (insert body "\n"))
    
    ;; Add blank line for spacing
    (insert "\n")))

(defun annotation--insert-annotation-at-point (level annotation)
  "Format and insert the ANNOTATION at point.
Use LEVEL in case we're at an id."
  (let* ((annotation-id (plist-get annotation :id))
	 (quote (plist-get annotation :quote))
	 (text (plist-get annotation :text))
	 (updated-at (plist-get annotation :updated-at))
	 (annotation-level (+ 1 (or level 0))))
    (annotation--insert-heading-at-point annotation-level "Annotation" quote annotation-id updated-at)
    (when text
      (annotation--insert-heading-at-point (+ 1 annotation-level) "Note" text))))

(defun annotation--find-annotation-with-id (annotation-id)
  "Find first subheading with given ANNOTATION-ID.
Leave the point in said subheading, or after all of the subheadings otherwise."
  (let ((found nil)
        (start-pos (point)))
    ;; Move to first subheading
    (when (org-goto-first-child)
      (catch 'found
        (while (not found)
          ;; Check if current heading has matching annotation-id
          (when (string= annotation-id
                         (org-entry-get (point) "ANNOTATION-ID"))
            (setq found t)
            (throw 'found t))
          
          ;; Try to move to next sibling
          (unless (org-goto-sibling)
            ;; No more siblings, we're after all subheadings
            (throw 'found nil)))))
    found))

(defun annotation--process-annotation (target annotation)
  "Process the current ANNOTATION at TARGET.

For a given ANNOTATION, go to TARGET and either update its extant version
or insert it as a new heading."
  (let* ((heading-level (plist-get target :node-kind))
         (node-id (plist-get target :node-id))
         (node-file (plist-get target :node-file))
         (annotation-id (plist-get annotation :id))
         (quote (plist-get annotation :quote))
         (text (plist-get annotation :text))
         (updated-at (plist-get annotation :updated-at)))
    
    (with-current-buffer (find-file-noselect node-file)
      ;; Navigate to the target node
      (org-id-goto node-id)
      
      ;; Move to end of entry to search/append
      (org-end-of-subtree t t)
      
      ;; Try to find existing annotation
      (org-back-to-heading t)
      (let ((found (annotation--find-annotation-with-id annotation-id)))
        (if found
            ;; Update existing annotation by cutting and reinserting
            (progn
              (org-cut-subtree)
              (annotation--insert-annotation-at-point heading-level annotation))
          
          ;; Insert new annotation at end
          (org-end-of-subtree t t)
          (annotation--insert-annotation-at-point heading-level annotation)))
      
      (save-buffer))))

;; Quadratic for now, searches linearly through all annotations for each entry.
;; Will see how it performs for a book with many, likely ok.
;; TODO: Monitor quadratic search
(defun annotation--update-entry (entry)
  "Find node for our ENTRY, update existing annotations, insert new ones.

For every annotation, first get or create the file/relevant entry.
Then save the point at the beginning of the relevant search area."
  (let* ((target (annotation--get-target-for-entry entry))
	 (annotations (plist-get entry :annotations)))
    (dolist (annotation annotations)
	    (annotation--process-annotation target annotation))
    ))

;; TODO: Optimization: precheck if any annotation changed since last update
;; THen save a timestamp for the whole node as well as for each annotation,...
;; Actually, this means we need to open the buffer to check for the annotations anyway.
;; Probably not much overhead to just check each annotation then. Low priority.
;; (defun annotation--get-last-update (entry))

(defun annotation--update-entries (entries)
  "Update each entry in the list of ENTRIES."
  (dolist (entry entries)
	  (annotation--update-entry entry)))

(provide 'org-roam-annotation-import)
;;; org-roam-annotation-import.el ends here.
