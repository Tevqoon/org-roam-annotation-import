;;; wallabag-backend.el --- Sync Wallabag highlights with Org-roam -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.1

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
;; This package integrates (Readwise) highlight syncing with Org-mode.
;; It provides commands to fetch highlights from Readwise and insert
;; them into an Org buffer or a specified file.
;;
;; This fork separates the insertion logic from the importing, allowing for multiple backends.
;; Further, we use org-roam for the annotations, specifying the file by using roam-refs.

(require 'org-roam-annotation-import)
(require 'wallabag)

(provide 'wallabag-backend)

(defun annotation--wallabag-gather-annotations ()
  (let* ((wallabag-entries (wallabag-db-select))
	 (entries
	  (cl-loop for entry in wallabag-entries
		   for annotations = (alist-get 'annotations entry)
		   when (and annotations 
			     (vectorp annotations) 
			     (> (length annotations) 0))
		   collect (list
			    :version 1
			    :id (alist-get 'id entry)
			    :title (alist-get 'title entry)
			    :url (alist-get 'url entry)
			    :updated-at (alist-get 'updated_at entry)
			    :annotations 
			    (cl-loop for annot across annotations
				     collect (list
					      :id (alist-get 'id annot)
					      :source "Wallabag"
					      :quote (alist-get 'quote annot)
					      :text (alist-get 'text annot)
					      :created-at (alist-get 'created_at annot)
					      :updated-at (alist-get 'updated_at annot)))))))
    entries))

(defun wallabag-synchronise-annotations ()
  (interactive)
  (let* ((entries (annotation--wallabag-gather-annotations)))
    (annotation--update-entries entries)))
