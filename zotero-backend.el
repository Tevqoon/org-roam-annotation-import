;;; zotero-backend.el --- Sync Zotero PDF annotations with Org-roam -*- lexical-binding: t; -*-
;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/org-roam-annotation-import
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (org "9.4") (org-roam "2.0") (request "0.3"))

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

;; This backend extracts PDF annotations from a local Zotero 7 instance
;; via its built-in local HTTP API (http://localhost:23119/api) and
;; syncs them into Org-roam nodes.  No Python or pyzotero needed.
;;
;; Requirements:
;;   - Zotero 7, running, with Settings > Advanced >
;;     "Allow other applications on this computer to communicate with
;;     Zotero" enabled.
;;   - Better BibTeX installed (used via its JSON-RPC endpoint to map
;;     Zotero item keys to citekeys, so annotations land in the same
;;     org-roam note as your citar/citar-org-roam literature note).
;;
;; Data flow (all over localhost, synchronous):
;;   1. GET items?itemType=annotation        -> all annotations
;;   2. batch GET items?itemKey=...           -> their attachment parents
;;   3. batch GET items?itemKey=...           -> the attachments' parents
;;                                               (the actual papers) + metadata
;;   4. POST better-bibtex/json-rpc           -> {topKey: citekey}
;;   5. assemble entries, feed to zotero--process-entry
;;
;; Annotation hierarchy on the local API:
;;   paper (top item)  <-parentItem-  attachment (PDF)  <-parentItem-  annotation
;; Note: annotations are NOT returned by items/<attachment>/children on
;; the local API (numChildren is 0); they must be fetched via
;; itemType=annotation and walked UP via parentItem.
;;
;; Unlike the Wallabag backend, Zotero annotations use the Quote/Comment
;; structure (not Front/Hint) and are protected from overwrite once
;; :Manual: t is stamped (the core does this automatically on creation
;; when `annotation-auto-manual' is non-nil).  Anki export is controlled
;; by `zotero-anki-deck'.

;;; Code:

(require 'org-roam-annotation-import)
(require 'request)
(require 'json)
(require 'subr-x)
(require 'cl-lib)

;;;; ----------------------------------------------------------------
;;;; Customisation
;;;; ----------------------------------------------------------------

(defgroup zotero-backend nil
  "Zotero annotation backend for org-roam-annotation-import."
  :group 'annotation
  :prefix "zotero-")

(defcustom zotero-api-base "http://localhost:23119/api/users/0"
  "Base URL for the Zotero local API.
The user segment is 0 (the logged-in local library); the real
numeric user ID also works but 0 is portable."
  :group 'zotero-backend
  :type 'string)

(defcustom zotero-bbt-jsonrpc-url "http://localhost:23119/better-bibtex/json-rpc"
  "URL for the Better BibTeX JSON-RPC endpoint (citekey resolution)."
  :group 'zotero-backend
  :type 'string)

(defcustom zotero-page-size 100
  "Number of items to request per page from the Zotero local API."
  :group 'zotero-backend
  :type 'integer)

(defcustom zotero-annotation-types '("highlight" "underline")
  "Annotation types to import.  Others (image, ink, note) are skipped."
  :group 'zotero-backend
  :type '(repeat string))

(defcustom zotero-anki-deck "Zotero"
  "Anki deck for Zotero annotations, or a flag controlling export.
Value semantics mirror the :anki plist key:
- a string  -> ankify into that deck (default \"Zotero\")
- t         -> ankify into the global `annotation-anki-deck'
- nil       -> do not create Anki cards for Zotero annotations"
  :group 'zotero-backend
  :type '(choice (string :tag "Deck name")
                 (const :tag "Global default deck" t)
                 (const :tag "Disabled" nil)))

(defcustom zotero-literature-subdir "references"
  "Subdirectory of `org-roam-directory' for Zotero-created literature notes.
Match this to your citar-org-roam subdir so that notes created by a
Zotero-first import land in the same place as citar-created notes."
  :group 'zotero-backend
  :type 'string)

(defcustom zotero-color-tag-alist
  '(("#ffd400" . "yellow")
    ("#ff6666" . "red")
    ("#5fb236" . "green")
    ("#2ea8e5" . "blue")
    ("#a28ae5" . "purple")
    ("#e56eee" . "magenta")
    ("#f19837" . "orange")
    ("#aaaaaa" . "grey"))
  "Mapping from Zotero annotation color hex to a tag name."
  :group 'zotero-backend
  :type '(alist :key-type string :value-type string))

;;;; ----------------------------------------------------------------
;;;; Low-level HTTP
;;;; ----------------------------------------------------------------

(defun zotero--get (path &optional params)
  "GET PATH (relative to `zotero-api-base') with PARAMS; return parsed JSON.
Signals an error on non-2xx.  Result is parsed with `json-read'
\(objects as alists, arrays as vectors)."
  (let* ((url (concat zotero-api-base path))
         (resp (request url
                 :type "GET"
                 :params params
                 :parser #'json-read
                 :sync t
                 :error (cl-function
                         (lambda (&key error-thrown response &allow-other-keys)
                           (error "Zotero API GET %s failed (%s): %S"
                                  path
                                  (and response (request-response-status-code response))
                                  error-thrown))))))
    (request-response-data resp)))

(defun zotero--get-total-results (path &optional params)
  "Return the integer Total-Results header for PATH/PARAMS, or nil."
  (let* ((url (concat zotero-api-base path))
         (resp (request url
                 :type "GET"
                 :params (append params '(("limit" . "1")))
                 :parser #'ignore
                 :sync t))
         (hdr (and resp (request-response-header resp "Total-Results"))))
    (and hdr (string-to-number hdr))))

(defun zotero--get-all (path &optional params)
  "GET every page of PATH/PARAMS, concatenating the JSON arrays into a list.
Paginates with start/limit using `zotero-page-size'."
  (let ((acc '())
        (start 0)
        (limit zotero-page-size)
        (done nil))
    (while (not done)
      (let* ((page-params (append params
                                  (list (cons "start" (number-to-string start))
                                        (cons "limit" (number-to-string limit)))))
             (page (append (zotero--get path page-params) nil)))
        (setq acc (nconc acc page))
        (if (< (length page) limit)
            (setq done t)
          (setq start (+ start limit)))))
    acc))

(defun zotero--get-items-by-keys (keys)
  "Fetch item data for KEYS (a list of item-key strings).
Batches into groups of 50 (the API's itemKey limit).  Returns a
hash table mapping key -> data alist."
  (let ((table (make-hash-table :test 'equal))
        (batch-size 50))
    (cl-loop for i from 0 below (length keys) by batch-size do
             (let* ((batch (cl-subseq keys i (min (+ i batch-size) (length keys))))
                    (items (append
                            (zotero--get "/items"
                                         (list (cons "itemKey" (string-join batch ","))))
                            nil)))
               (dolist (item items)
                 (let* ((data (alist-get 'data item))
                        (k    (alist-get 'key data)))
                   (when k (puthash k data table))))))
    table))

;;;; ----------------------------------------------------------------
;;;; Better BibTeX citekey resolution
;;;; ----------------------------------------------------------------

(defun zotero--citekeys-for (top-keys)
  "Return a hash table mapping each key in TOP-KEYS to its BBT citekey.
Keys with no citekey are simply absent from the table.  Returns an
empty table (and warns) if the BBT endpoint is unreachable."
  (let ((table (make-hash-table :test 'equal)))
    (when top-keys
      (condition-case err
          (let* ((payload (json-encode
                           `(("jsonrpc" . "2.0")
                             ("method"  . "item.citationkey")
                             ("params"  . (,(vconcat top-keys)))
                             ("id"      . 1))))
                 (resp (request zotero-bbt-jsonrpc-url
                         :type "POST"
                         :headers '(("Content-Type" . "application/json"))
                         :data payload
                         :parser #'json-read
                         :sync t
                         :error (cl-function
                                 (lambda (&key error-thrown &allow-other-keys)
                                   (error "BBT JSON-RPC failed: %S" error-thrown)))))
                 (data   (request-response-data resp))
                 (result (alist-get 'result data)))
            ;; result is an alist (symbol-keys) of (TOPKEY . citekey)
            (dolist (pair result)
              (let ((k (symbol-name (car pair)))
                    (v (cdr pair)))
                (when (and v (stringp v) (not (string-empty-p v)))
                  (puthash k v table)))))
        (error
         (message "Zotero: citekey resolution failed (%s); will fall back to title/zotero-key matching"
                  (error-message-string err)))))
    table))

;;;; ----------------------------------------------------------------
;;;; Metadata helpers
;;;; ----------------------------------------------------------------

(defun zotero--creators-string (creators)
  "Build a readable author string from CREATORS (a vector of alists)."
  (let ((names '()))
    (cl-loop for c across (or creators [])
             for type = (alist-get 'creatorType c)
             when (member type '("author" "bookAuthor"))
             do (let ((last  (alist-get 'lastName c))
                      (first (alist-get 'firstName c))
                      (name  (alist-get 'name c)))
                  (cond
                   ((and last (not (string-empty-p last)))
                    (push (if (and first (not (string-empty-p first)))
                              (format "%s, %s" last first)
                            last)
                          names))
                   ((and name (not (string-empty-p name)))
                    (push name names)))))
    (setq names (nreverse names))
    (cond
     ((null names) nil)
     ((= (length names) 1) (car names))
     ((= (length names) 2) (format "%s & %s" (nth 0 names) (nth 1 names)))
     (t (format "%s et al." (car names))))))

(defun zotero--year (data)
  "Extract a 4-digit year from item DATA's date field, or empty string."
  (let ((date (or (alist-get 'date data) "")))
    (if (string-match "\\([0-9]\\{4\\}\\)" date)
        (match-string 1 date)
      "")))

(defun zotero--note-title (author year title)
  "Compose the note title from AUTHOR, YEAR, TITLE."
  (let ((title (or title "(untitled)")))
    (if (and year (not (string-empty-p year)))
        (format "%s (%s) - %s" (or author "Unknown") year title)
      title)))

(defun zotero--pdf-link (attach-key page-label annotation-key)
  "Build a zotero://open-pdf URI targeting the annotation."
  (let ((base (format "zotero://open-pdf/library/items/%s" attach-key))
        (params '()))
    (when (and page-label (string-match-p "\\`[0-9]+\\'" page-label))
      (push (format "page=%s" page-label) params))
    (when annotation-key
      (push (format "annotation=%s" annotation-key) params))
    (if params
        (concat base "?" (string-join (nreverse params) "&"))
      base)))

(defun zotero--select-link (top-key)
  "Build a zotero://select URI for the top-level item TOP-KEY."
  (format "zotero://select/library/items/%s" top-key))

(defun zotero--color-tag (color)
  "Return a tag string for COLOR hex, or nil."
  (cdr (assoc color zotero-color-tag-alist)))

;;;; ----------------------------------------------------------------
;;;; Content writer (Quote/Comment, Manual-aware, opt-in Anki)
;;;; ----------------------------------------------------------------

(defun zotero--write-annotation-content (annotation entry-title entry-url)
  "Write Zotero annotation content into the heading at point.
ANNOTATION is the plist, ENTRY-TITLE and ENTRY-URL are parent data.

- Sets Zotero-Link property for deep-linking into Zotero's PDF viewer.
- When the Manual property is t, the Quote body is left untouched
  (so hand-corrected LaTeX survives re-import).
- Writes Quote (highlighted text + Source link) and Comment (your
  Zotero note) subheadings -- no Front/Hint flashcard wrapper.
- Anki properties are set when :anki resolves to a deck (see
  `annotation--resolve-anki-deck'); the color is folded into the tags."
  (let* ((updated-at  (plist-get annotation :updated-at))
         (quote-text  (plist-get annotation :quote))
         (comment     (plist-get annotation :text))
         (page        (plist-get annotation :page))
         (source      (plist-get annotation :source))
         (zotero-link (plist-get annotation :zotero-link))
         (color       (plist-get annotation :color))
         (manual-p    (string= "t" (org-entry-get nil "Manual")))
         (color-tag   (zotero--color-tag color))
         (citekey     (plist-get annotation :citekey)))

    (when updated-at  (org-set-property "Updated-at"  updated-at))
    (when source      (org-set-property "Source"      source))
    (when page        (org-set-property "Page"        (format "%s" page)))
    (when zotero-link (org-set-property "Zotero-Link" zotero-link))
    (when color       (org-set-property "Color"       color))

    ;; Front: the highlighted text + source link. Protected when Manual
    ;; is set, so hand-corrected LaTeX survives re-import.  Named "Front"
    ;; to match the Basic note type's field (same as the Wallabag backend).
    (unless manual-p
      (annotation--upsert-child-heading
       "Front"
       (when (and quote-text (not (string-empty-p quote-text)))
         (concat quote-text
                 (when (and (or zotero-link entry-url) entry-title)
                   (format "\n\nSource: [[%s][%s, p.%s]]"
                           (or zotero-link entry-url)
                           entry-title
                           (or page "?")))))))

    ;; Hint: your Zotero comment.  Always synced.  Named "Hint" to match
    ;; the custom Hint field added to the Basic note type.
    (annotation--upsert-child-heading "Hint" comment)

    ;; Anki: deck resolved from :anki (nil disables).  Tags are the
    ;; citekey (@key) plus the highlight color — both space-free, so they
    ;; stay as distinct Anki tags with no %20 encoding.
    (let ((deck (annotation--resolve-anki-deck (plist-get annotation :anki))))
      (when deck
        (let* ((parts (delq nil (list (and citekey (concat "@" citekey))
                                      color-tag)))
               (full-tags (string-join parts " ")))
          (annotation--set-anki-properties full-tags deck))))))

;;;; ----------------------------------------------------------------
;;;; Normalisation
;;;; ----------------------------------------------------------------

(defun zotero--normalise-annotation (data attach-key citekey)
  "Build an annotation plist from raw annotation DATA under ATTACH-KEY.
CITEKEY is the parent paper's Better BibTeX citekey (or nil), stored
on the plist so the writer can use it as the Anki tag."
  (let* ((ann-key (alist-get 'key data))
         (page    (alist-get 'annotationPageLabel data))
         (quote   (or (alist-get 'annotationText data) ""))
         (comment (or (alist-get 'annotationComment data) ""))
         (color   (alist-get 'annotationColor data))
         (updated (alist-get 'dateModified data)))
    (list :id          ann-key
          :quote       quote
          :text        comment
          :page        page
          :color       color
          :citekey     citekey
          :zotero-link (zotero--pdf-link attach-key page ann-key)
          :updated-at  updated
          :source      "Zotero"
          :anki        zotero-anki-deck
          :write-fn    #'zotero--write-annotation-content)))

;;;; ----------------------------------------------------------------
;;;; Node lookup (citekey-aware) and entry processing
;;;; ----------------------------------------------------------------

(defun zotero--find-or-create-node (entry)
  "Return an org-roam node for ENTRY, preferring citekey lookup.
ENTRY carries :citekey, :ref (ROAM_REF to persist), :select-url, :title."
  (let* ((citekey    (plist-get entry :citekey))
         (ref        (plist-get entry :ref))
         (select-url (plist-get entry :select-url))
         (title      (plist-get entry :title)))
    (or
     ;; 1. Citekey via ROAM_REFS @key (the form citar-org-roam stores)
     (when citekey
       (org-roam-node-from-ref (concat "@" citekey)))
     ;; 2. zotero://select URL ref
     (when (and select-url (not (equal ref select-url)))
       (org-roam-node-from-ref select-url))
     ;; 3. Title (may throw on ambiguous match -- guard it)
     (when title
       (ignore-errors (org-roam-node-from-title-or-alias title)))
     ;; 4. Create new, seeded with the lookup ref
     (org-roam-node-create
      :title title
      :refs  (delq nil (list ref))
      :id    (org-id-new)))))

(defun zotero--literature-capture-templates (entry)
  "Build an `annotation-capture-templates' value for ENTRY.
Files the note under `zotero-literature-subdir' using the citekey as
the filename when available (matching citar-org-roam), with a
:literature: filetag and no startup line — matching a hand-made
citar literature note."
  (let* ((citekey (plist-get entry :citekey))
         (fname   (if (and citekey (not (string-empty-p citekey)))
                      (format "%s/%s.org" zotero-literature-subdir citekey)
                    (format "%s/%%<%%Y%%m%%d%%H%%M%%S>-${slug}.org"
                            zotero-literature-subdir))))
    `(("d" "literature" plain "%?"
       :if-new (file+head ,fname "#+title: ${title}\n#+filetags: :literature:")
       :unnarrowed t
       :immediate-finish t))))

(defun zotero--process-entry (entry)
  "Find or create a node for ENTRY and upsert its annotations.
Returns the modified file path, or nil if nothing changed."
  (save-window-excursion
    (let* ((ref         (plist-get entry :ref))
           (select-url  (plist-get entry :select-url))
           (title       (plist-get entry :title))
           (annotations (plist-get entry :annotations))
           (incoming-updated-at (annotation--entry-updated-at entry))
           (node   (zotero--find-or-create-node entry))
           ;; New nodes are created with a literature template matching
           ;; citar-org-roam's location/header.
           (annotation-capture-templates
            (zotero--literature-capture-templates entry))
           (buffer (annotation--org-roam-node-open-or-create node)))
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
                ;; Persist the lookup ref (check buffer ROAM_REFS, not the
                ;; struct slot, which may hold an unwritten ref).
                (when ref
                  (let ((existing (org-entry-get (point) "ROAM_REFS" t)))
                    (unless (and existing
                                 (string-match-p (regexp-quote ref) existing))
                      (org-roam-ref-add ref))))
                (org-roam-tag-add '("annotations" "zotero" "literature"))
                (when-let ((author (plist-get entry :author))
                           (slug   (annotation--slugify author)))
                  (org-roam-tag-add (list slug)))
                (annotation--goto-or-insert-child "Annotations")
                (dolist (annotation annotations)
                  (save-excursion
                    (annotation--process-annotation annotation title select-url)))
                (save-buffer)
                (widen)
                (if (buffer-modified-p)
                    (progn (save-buffer) (buffer-file-name))
                  nil)))))))))

;;;; ----------------------------------------------------------------
;;;; Assembly -- fetch, walk hierarchy, build entries
;;;; ----------------------------------------------------------------

(cl-defun zotero--collect-entries (&optional since)
  "Fetch annotations and assemble a list of entry plists.
With SINCE (ISO 8601 string), skip annotations not modified after it.

Returns entries grouped by top-level paper, each ready for
`zotero--process-entry'."
  ;; 1. All annotations.
  (let* ((raw-anns (zotero--get-all "/items"
                                    '(("itemType" . "annotation"))))
         ;; Keep only wanted types, and apply the since-filter.
         (anns (cl-remove-if-not
                (lambda (item)
                  (let* ((d (alist-get 'data item))
                         (type (alist-get 'annotationType d))
                         (upd  (alist-get 'dateModified d)))
                    (and (member type zotero-annotation-types)
                         (or (null since) (string> (or upd "") since)))))
                raw-anns)))
    (when (null anns)
      (cl-return-from zotero--collect-entries nil))

    ;; 2. Attachment keys (annotation :parentItem) -> fetch attachments.
    (let* ((attach-keys (delete-dups
                         (delq nil
                               (mapcar (lambda (item)
                                         (alist-get 'parentItem (alist-get 'data item)))
                                       anns))))
           (attach-table (zotero--get-items-by-keys attach-keys))
           ;; 3. Top-item keys (attachment :parentItem) -> fetch papers.
           (top-keys (delete-dups
                      (delq nil
                            (mapcar (lambda (ak)
                                      (alist-get 'parentItem (gethash ak attach-table)))
                                    attach-keys))))
           (top-table (zotero--get-items-by-keys top-keys))
           ;; 4. Citekeys for the top items.
           (citekey-table (zotero--citekeys-for top-keys))
           ;; Group annotations by top-item key.
           (by-top (make-hash-table :test 'equal)))

      ;; Bucket each annotation under its top-item key.
      (dolist (item anns)
        (let* ((d          (alist-get 'data item))
               (attach-key (alist-get 'parentItem d))
               (top-key    (and attach-key
                                (alist-get 'parentItem (gethash attach-key attach-table))))
               (citekey    (and top-key (gethash top-key citekey-table))))
          (when top-key
            (push (zotero--normalise-annotation d attach-key citekey)
                  (gethash top-key by-top)))))

      ;; Build one entry per top item.
      (let ((entries '()))
        (maphash
         (lambda (top-key annots)
           (let* ((data       (gethash top-key top-table))
                  (title      (alist-get 'title data))
                  (author     (zotero--creators-string (alist-get 'creators data)))
                  (year       (zotero--year data))
                  (note-title (zotero--note-title author year title))
                  (citekey    (gethash top-key citekey-table))
                  (have-key   (and citekey (not (string-empty-p citekey))))
                  (select-url (zotero--select-link top-key))
                  (ref        (if have-key (concat "@" citekey) select-url))
                  ;; Sort annotations by page then id for stable order.
                  (sorted     (sort (nreverse annots)
                                    (lambda (a b)
                                      (let ((pa (string-to-number (or (plist-get a :page) "0")))
                                            (pb (string-to-number (or (plist-get b :page) "0"))))
                                        (if (= pa pb)
                                            (string< (or (plist-get a :id) "")
                                                     (or (plist-get b :id) ""))
                                          (< pa pb))))))
                  (updated    (car (sort (delq nil (mapcar (lambda (a) (plist-get a :updated-at)) sorted))
                                         #'string>))))
             (push (list :title       note-title
                         :citekey     (and have-key citekey)
                         :ref         ref
                         :select-url  select-url
                         :author      author
                         :updated-at  updated
                         :annotations sorted)
                   entries)))
         by-top)
        (nreverse entries)))))

;;;; ----------------------------------------------------------------
;;;; Public interface
;;;; ----------------------------------------------------------------

;;;###autoload
(defun zotero-synchronise-annotations (&optional since)
  "Sync all Zotero PDF annotations into Org-roam.
With prefix arg, prompt for an ISO 8601 SINCE date to limit the sync."
  (interactive
   (list (when current-prefix-arg
           (read-string "Sync annotations modified since (ISO 8601, blank for all): "))))
  (message "Zotero: fetching annotations from local API...")
  (let* ((since-arg (and since (not (string-empty-p since)) since))
         (entries   (zotero--collect-entries since-arg)))
    (message "Zotero: syncing %d paper(s) with annotations..." (length entries))
    ;; Populate the shared `annotation--recently-modified-files' (not a
    ;; local var) so `my/anki-annotation-push-all' without a prefix arg
    ;; pushes exactly the files this sync touched.
    (setq annotation--recently-modified-files nil)
    (dolist (entry entries)
      (when-let ((file (zotero--process-entry entry)))
        (push file annotation--recently-modified-files)))
    (setq annotation--recently-modified-files
          (delete-dups (nreverse annotation--recently-modified-files)))
    (message "Zotero sync done: %d file(s) modified"
             (length annotation--recently-modified-files))))

;;;###autoload
(defun zotero-synchronise-annotation-at-point ()
  "Sync Zotero annotations for the citekey of the current org-roam node."
  (interactive)
  (require 'citar-org-roam nil t)
  (let* ((node (org-roam-node-at-point))
         (keys (and node
                    (fboundp 'citar-org-roam--node-cite-refs)
                    (citar-org-roam--node-cite-refs node)))
         (citekey (car keys)))
    (unless citekey
      (error "No citekey found for current node"))
    (message "Zotero: syncing annotations for @%s..." citekey)
    (let* ((entries (zotero--collect-entries))
           (matched (seq-filter
                     (lambda (e) (equal (plist-get e :citekey) citekey))
                     entries)))
      (if (null matched)
          (message "Zotero: no annotations found for @%s" citekey)
        (setq annotation--recently-modified-files nil)
        (dolist (entry matched)
          (when-let ((file (zotero--process-entry entry)))
            (push file annotation--recently-modified-files)))
        (setq annotation--recently-modified-files
              (delete-dups (nreverse annotation--recently-modified-files)))
        (message "Zotero: synced @%s (%d file(s) modified)"
                 citekey (length annotation--recently-modified-files))))))

;;;###autoload
(defun zotero-ping ()
  "Check that the Zotero local API is reachable.  Reports the item count."
  (interactive)
  (condition-case err
      (let ((n (zotero--get-total-results "/items" '(("itemType" . "annotation")))))
        (message "Zotero local API OK -- %s annotation(s) in library" (or n "?")))
    (error
     (message "Zotero local API unreachable: %s. Is Zotero running with the local API enabled?"
              (error-message-string err)))))

(provide 'zotero-backend)
;;; zotero-backend.el ends here.
