;;; wallabag-backend.el --- Sync Wallabag highlights with Org-roam -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.3

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

;; This backend replaces the wallabag.el/SQLite dependency with direct calls
;; to the Wallabag REST API, using the credentials already configured as:
;;   wallabag-host, wallabag-username, wallabag-password,
;;   wallabag-clientid, wallabag-secret
;;
;; Token acquisition and capability detection are synchronous (fast, one-shot).
;; Entry fetching is async: pages are fetched recursively via request.el
;; callbacks, so Emacs is not blocked during the sync.

;;; Code:

(require 'org-roam-annotation-import)
(require 'request)
(require 'json)

;;;; ----------------------------------------------------------------
;;;; Token management (synchronous -- fast, prerequisite for everything)
;;;; ----------------------------------------------------------------

(defvar wb--access-token nil
  "Current OAuth2 access token string, or nil if not yet fetched.")

(defvar wb--refresh-token nil
  "Current OAuth2 refresh token string, or nil.")

(defvar wb--token-expiry nil
  "Emacs internal time at which the access token expires.")

(defconst wb--expiry-margin 60
  "Seconds before nominal expiry at which we proactively refresh.")

(defun wb--token-expired-p ()
  "Return non-nil if the access token is absent or about to expire."
  (or (null wb--access-token)
      (null wb--token-expiry)
      (time-less-p (time-subtract wb--token-expiry
                                  (seconds-to-time wb--expiry-margin))
                   (current-time))))

(defun wb--parse-token-response (data)
  "Store token fields from parsed JSON alist DATA and return the access token."
  (let ((access  (alist-get 'access_token  data))
        (refresh (alist-get 'refresh_token data))
        (expires (alist-get 'expires_in    data)))
    (unless access
      (error "Wallabag: token response missing access_token: %S" data))
    (setq wb--access-token  access
          wb--refresh-token refresh
          wb--token-expiry  (time-add (current-time)
                                      (seconds-to-time (or expires 3600))))
    access))

(defun wb--request-token-with-password ()
  "Fetch a fresh access + refresh token using the password grant."
  (let ((response
         (request (concat wallabag-host "/oauth/v2/token")
           :type   "POST"
           :data   (list (cons "grant_type"    "password")
                         (cons "client_id"     wallabag-clientid)
                         (cons "client_secret" wallabag-secret)
                         (cons "username"      wallabag-username)
                         (cons "password"      wallabag-password))
           :parser #'json-read
           :sync   t
           :error  (cl-function
                    (lambda (&key error-thrown response &allow-other-keys)
                      (error "Wallabag password grant failed (%s): %S"
                             (request-response-status-code response)
                             error-thrown))))))
    (wb--parse-token-response (request-response-data response))))

(defun wb--request-token-with-refresh ()
  "Refresh the access token using `wb--refresh-token'."
  (unless wb--refresh-token
    (error "Wallabag: no refresh token available"))
  (let ((response
         (request (concat wallabag-host "/oauth/v2/token")
           :type   "POST"
           :data   (list (cons "grant_type"    "refresh_token")
                         (cons "client_id"     wallabag-clientid)
                         (cons "client_secret" wallabag-secret)
                         (cons "refresh_token" wb--refresh-token))
           :parser #'json-read
           :sync   t
           :error  (cl-function
                    (lambda (&key error-thrown response &allow-other-keys)
                      (error "Wallabag refresh grant failed (%s): %S"
                             (request-response-status-code response)
                             error-thrown))))))
    (wb--parse-token-response (request-response-data response))))

(defun wb--ensure-token ()
  "Return a valid access token, refreshing or re-authenticating as needed."
  (when (wb--token-expired-p)
    (condition-case err
        (wb--request-token-with-refresh)
      (error
       (message "Wallabag: refresh failed (%s); retrying with password grant"
                (error-message-string err))
       (wb--request-token-with-password))))
  wb--access-token)

(defun wb--auth-header ()
  "Return an Authorization header cons for the current token."
  (cons "Authorization" (concat "Bearer " (wb--ensure-token))))

;;;; ----------------------------------------------------------------
;;;; Version / capability detection (synchronous, once per session)
;;;; ----------------------------------------------------------------

(defvar wb--annotations-filter-supported nil
  "Non-nil if the running wallabag instance supports ?annotations=1.")

(defvar wb--capabilities-detected nil
  "Non-nil once `wb--detect-capabilities' has run successfully.")

(defun wb--detect-capabilities ()
  "Query /api/version.json and set capability flags.  Runs only once per session."
  (unless wb--capabilities-detected
    (let ((response
           (request (concat wallabag-host "/api/version.json")
             :headers (list (wb--auth-header))
             :parser  #'json-read
             :sync    t
             :error   (cl-function
                       (lambda (&key error-thrown &allow-other-keys)
                         (error "Wallabag: version check failed: %S" error-thrown))))))
      (let* ((raw     (request-response-data response))
             (version (if (stringp raw) raw (format "%s" raw)))
             (parts   (mapcar #'string-to-number
                              (split-string (string-trim version "\"") "\\.")))
             (major   (nth 0 parts))
             (minor   (nth 1 parts)))
        (setq wb--annotations-filter-supported
              (or (> major 2) (and (= major 2) (>= minor 6)))
              wb--capabilities-detected t)
        (message "Wallabag: version %s -- annotations filter %s"
                 version
                 (if wb--annotations-filter-supported "supported" "NOT supported"))))))

;;;; ----------------------------------------------------------------
;;;; Data normalisation helpers
;;;; ----------------------------------------------------------------

(defun wb--fix-timestamp (ts)
  "Normalise wallabag timestamp TS to ISO 8601 with a colon in the TZ offset.
Wallabag emits e.g. \"2024-06-12T08:42:11+0100\" (no colon).  If a colon is
already present -- \"...+01:00\" -- the string is returned unchanged."
  (when (stringp ts)
    (if (string-match-p "[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\'" ts)
        ts
      (replace-regexp-in-string
       "\\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2" ts))))

(defun wb--coerce-int (x)
  "Return X as an integer, handling the case where it arrives as a string."
  (if (stringp x) (string-to-number x) x))

(defun wb--normalise-annotation (annot)
  "Convert raw annotation alist ANNOT into a plist for `annotation--update-entries'."
  (list :id         (wb--coerce-int (alist-get 'id annot))
        :source     "Wallabag"
        :quote      (or (alist-get 'quote annot) "")
        :text       (or (alist-get 'text  annot) "")
        :created-at (wb--fix-timestamp (alist-get 'created_at annot))
        :updated-at (wb--fix-timestamp (alist-get 'updated_at annot))
        :anki       t))

(defun wb--normalise-entry (entry)
  "Convert raw entry alist ENTRY into a plist for `annotation--update-entries'.
Returns nil when the entry has no annotations (client-side filter for < 2.6)."
  (let* ((raw-annots (alist-get 'annotations entry))
         (annots     (cond
                      ((vectorp raw-annots) (append raw-annots nil))
                      ((listp   raw-annots) raw-annots)
                      (t nil))))
    (when (and annots (> (length annots) 0))
      (let* ((pub    (alist-get 'published_by entry))
             (dom    (alist-get 'domain_name  entry))
             (author (cond
                      ((vectorp pub)
                       (and (> (length pub) 0) (aref pub 0)))
                      ((and (stringp pub) (not (string-empty-p pub))) pub)
                      ((and (stringp dom) (not (string-empty-p dom))) dom)
                      (t nil))))
        (list :version     1
              :id          (alist-get 'id         entry)
              :title       (alist-get 'title      entry)
              :url         (alist-get 'url        entry)
              :author      author
              :updated-at  (wb--fix-timestamp (alist-get 'updated_at entry))
              :annotations (mapcar #'wb--normalise-annotation annots))))))

;;;; ----------------------------------------------------------------
;;;; Async entry fetching -- recursive pagination via request.el callbacks
;;;; ----------------------------------------------------------------

(defconst wb--per-page 100
  "Page size for entry list requests.")

(defun wb--fetch-entries-page-async (page accumulator callback)
  "Fetch PAGE of annotated entries; on completion recurse or call CALLBACK.
ACCUMULATOR is the reversed list of normalised plists gathered so far.
When all pages are done, CALLBACK is called with the final forward-order list."
  (let ((params (append
                 (list (cons "detail"  "metadata")
                       (cons "perPage" (number-to-string wb--per-page))
                       (cons "page"    (number-to-string page)))
                 (when wb--annotations-filter-supported
                   (list (cons "annotations" "1"))))))
    (request (concat wallabag-host "/api/entries.json")
      :headers (list (wb--auth-header))
      :params  params
      :parser  #'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let* ((pages (wb--coerce-int (alist-get 'pages data)))
                (items (append (alist-get 'items (alist-get '_embedded data)) nil))
                (acc   accumulator))
           (dolist (entry items)
             (let ((norm (wb--normalise-entry entry)))
               (when norm (push norm acc))))
           (if (< page pages)
               (wb--fetch-entries-page-async (1+ page) acc callback)
             (message "Wallabag: fetched %d annotated entries" (length acc))
             (funcall callback (nreverse acc))))))
      :error
      (cl-function
       (lambda (&key error-thrown response &allow-other-keys)
         (let ((code (request-response-status-code response)))
           (if (eql code 404)
               (progn
                 (message "Wallabag: fetched %d annotated entries"
                          (length accumulator))
                 (funcall callback (nreverse accumulator)))
             (error "Wallabag: entries fetch failed (%s): %S"
                    code error-thrown))))))))

;;;; ----------------------------------------------------------------
;;;; Public interface
;;;; ----------------------------------------------------------------

(defun wallabag-synchronise-annotations ()
  "Pull all annotated entries from Wallabag and sync them to Org-roam.
Network I/O is async; `annotation--update-entries' runs in the final callback."
  (interactive)
  (message "Wallabag: starting async sync...")
  (wb--detect-capabilities)
  (wb--fetch-entries-page-async
   1 nil
   (lambda (entries)
     (annotation--update-entries entries))))

(defun wallabag-synchronise-annotation ()
  "Synchronise only the first annotated entry.  Useful for debugging."
  (interactive)
  (message "Wallabag: fetching first annotated entry...")
  (wb--detect-capabilities)
  (wb--fetch-entries-page-async
   1 nil
   (lambda (entries)
     (when entries
       (annotation--update-entries (list (car entries)))))))

(defun wallabag-reset-tokens ()
  "Clear cached OAuth tokens and capability flags, forcing re-authentication."
  (interactive)
  (setq wb--access-token          nil
        wb--refresh-token         nil
        wb--token-expiry          nil
        wb--capabilities-detected nil)
  (message "Wallabag: tokens and capabilities cleared"))

(provide 'wallabag-backend)
;;; wallabag-backend.el ends here
