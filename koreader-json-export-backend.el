;;; koreader-json-backend.el --- Sync Koreader highlights with Org-roam -*- lexical-binding: t; -*-
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

;; This backend imports annotations from KOReader JSON export files.
;; KOReader exports highlights and notes to JSON format which can be
;; synced to org-roam nodes.

;;; Code:

(require 'org-roam-annotation-import)
(require 'json)

(defcustom koreader-json-file-pattern "\\.json\\'"
  "Regexp pattern to match KOReader JSON annotation files."
  :group 'annotation
  :type 'string)

(defun koreader--parse-json-file (file)
  "Parse a KOReader JSON FILE and return its contents as a plist."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun koreader--generate-annotation-id (entry source-title)
  "Generate a unique ID for ENTRY from SOURCE-TITLE.
Uses page, time, and text content to create a stable hash."
  (let ((page (plist-get entry :page))
        (time (plist-get entry :time))
        (text (plist-get entry :text)))
    (annotation--generate-id "koreader" source-title page time text)))

(defun koreader--transform-entry (entry source-title)
  "Transform a KOReader ENTRY into the standard annotation format.
SOURCE-TITLE is used for ID generation."
  (let ((text (plist-get entry :text))
        (note (plist-get entry :note))
        (page (plist-get entry :page))
        (chapter (plist-get entry :chapter))
        (time (plist-get entry :time)))
    (list :id (koreader--generate-annotation-id entry source-title)
          :source "KOReader"
          :quote text
          :text note
          :page page
          :chapter chapter
          :updated-at time)))

(defun koreader--transform-json (json-data)
  "Transform JSON-DATA from KOReader format to standard entry format."
  (let* ((title (plist-get json-data :title))
         (author (plist-get json-data :author))
         (entries (plist-get json-data :entries))
         (created-on (plist-get json-data :created_on))
         (annotations (mapcar (lambda (e) (koreader--transform-entry e title))
                              entries)))
    (list :title title
          :author author
          :url nil
          :updated-at created-on
          :annotations annotations)))

(defun koreader--import-json-file (file)
  "Import annotations from a single KOReader JSON FILE."
  (let* ((json-data (koreader--parse-json-file file))
         (entry (koreader--transform-json json-data)))
    (annotation-debug 1 "Importing from: %s" file)
    (annotation-debug 2 "Title: %s, Annotations: %d"
                      (plist-get entry :title)
                      (length (plist-get entry :annotations)))
    (annotation--update-entries (list entry))))

(defun koreader--find-json-files (directory)
  "Find all JSON files matching `koreader-json-file-pattern' in DIRECTORY."
  (directory-files directory t koreader-json-file-pattern))

;;;###autoload
(defun koreader-import-json-file (file)
  "Import annotations from a KOReader JSON FILE.
Prompts for file selection interactively."
  (interactive
   (list (read-file-name "KOReader JSON file: "
                         annotation-default-json-directory
                         nil t nil
                         (lambda (f) (or (file-directory-p f)
                                         (string-match-p koreader-json-file-pattern f))))))
  (koreader--import-json-file file)
  (message "Imported annotations from %s" (file-name-nondirectory file)))

;;;###autoload
(defun koreader-import-json-directory (directory)
  "Import annotations from all KOReader JSON files in DIRECTORY.
Uses `annotation-default-json-directory' if set, otherwise prompts."
  (interactive
   (list (read-directory-name "Directory with KOReader JSON files: "
                              annotation-default-json-directory)))
  (let ((files (koreader--find-json-files directory))
        (count 0))
    (if (null files)
        (message "No JSON files found in %s" directory)
      (dolist (file files)
        (condition-case err
            (progn
              (koreader--import-json-file file)
              (setq count (1+ count)))
          (error
           (annotation-debug 0 "Error importing %s: %s" file (error-message-string err)))))
      (message "Imported annotations from %d files" count))))

;;;###autoload
(defun koreader-import-from-default-directory ()
  "Import all KOReader JSON files from `annotation-default-json-directory'.
Signals an error if the directory is not configured."
  (interactive)
  (unless annotation-default-json-directory
    (user-error "Set `annotation-default-json-directory' first"))
  (koreader-import-json-directory annotation-default-json-directory))

(provide 'koreader-json-backend)
;;; koreader-json-backend.el ends here
