;;; js-anki-annotation-push.el --- Per-source Anki push helpers -*- lexical-binding: t; -*-

;; Auxiliary push commands for the org-roam-annotation-import backends.
;; Drop into your config (or load-file it) after anki-editor and the
;; backends are set up.  These complement `my/anki-annotation-push-all'.
;;
;; Each backend tags its nodes:
;;   Zotero    -> :annotations: :zotero: :literature:
;;   Wallabag  -> :annotations:            (+ author slug)
;;   KOReader  -> :annotations:            (+ author slug)
;;
;; Wallabag and KOReader don't currently carry a distinguishing tag, so
;; to split them you must add one in each backend (see the comment at the
;; bottom of this file).  Until then, `js/anki-push-wallabag' and
;; `js/anki-push-koreader' fall back to "everything tagged :annotations:
;; that ISN'T zotero", which lumps the two file-based sources together.

(require 'cl-lib)
(require 'anki-editor)

;;;; ----------------------------------------------------------------
;;;; Tag-based file selection
;;;; ----------------------------------------------------------------

(defun js/anki--files-with-tags (required &optional excluded)
  "Return org-roam files whose nodes carry all REQUIRED tags and no EXCLUDED tag.
REQUIRED and EXCLUDED are lists of tag strings."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (n)
            (let ((tags (org-roam-node-tags n)))
              (when (and (cl-every  (lambda (tg) (member tg tags)) required)
                         (cl-notany (lambda (tg) (member tg tags)) (or excluded '())))
                (org-roam-node-file n))))
          (org-roam-node-list)))))

;;;; ----------------------------------------------------------------
;;;; Generic pusher (shared error-reporting wrapper)
;;;; ----------------------------------------------------------------

(defun js/anki--push-files (files scope)
  "Push each file in FILES to Anki, reporting progress under SCOPE."
  (anki-flashcard-clear-error-buffer)
  (let ((total (length files))
        (success 0))
    (if (null files)
        (message "No %s annotation files to push." scope)
      (message "Pushing %d %s annotation file(s) to Anki..." total scope)
      (dolist (file files)
        (condition-case err
            (with-current-buffer (find-file-noselect file)
              (save-excursion (anki-editor-push-notes 'file))
              (cl-incf success))
          (error (anki-flashcard-report-error file (error-message-string err)))))
      (if (< success total)
          (progn
            (message "Pushed %d/%d %s, %d errors — see %s"
                     success total scope (- total success) anki-flashcard-error-buffer)
            (display-buffer anki-flashcard-error-buffer))
        (message "Pushed all %d %s annotation files" total scope)))))

;;;; ----------------------------------------------------------------
;;;; Per-source push commands
;;;; ----------------------------------------------------------------

;;;###autoload
(defun js/anki-push-zotero ()
  "Push all Zotero annotation files to Anki (tagged :zotero: :annotations:)."
  (interactive)
  (js/anki--push-files
   (js/anki--files-with-tags '("annotations" "zotero"))
   "Zotero"))

;;;###autoload
(defun js/anki-push-wallabag ()
  "Push all Wallabag annotation files to Anki.
Requires the Wallabag backend to tag nodes with :wallabag: (see note
in this file).  Falls back to non-Zotero annotation files otherwise."
  (interactive)
  (let ((tagged (js/anki--files-with-tags '("annotations" "wallabag"))))
    (js/anki--push-files
     (or tagged
         ;; Fallback: annotations that aren't Zotero or KOReader.
         (js/anki--files-with-tags '("annotations") '("zotero" "koreader")))
     "Wallabag")))

;;;###autoload
(defun js/anki-push-koreader ()
  "Push all KOReader annotation files to Anki.
Requires the KOReader backend to tag nodes with :koreader: (see note
in this file)."
  (interactive)
  (js/anki--push-files
   (js/anki--files-with-tags '("annotations" "koreader"))
   "KOReader"))

;;;###autoload
(defun js/anki-push-all-annotations ()
  "Push every annotation file (all sources) to Anki."
  (interactive)
  (js/anki--push-files
   (js/anki--files-with-tags '("annotations"))
   "all"))

;;;###autoload
(defun js/anki-push-recent-annotations ()
  "Push only files touched by the most recent sync, any source.
Reads `annotation--recently-modified-files'."
  (interactive)
  (js/anki--push-files annotation--recently-modified-files "recently modified"))

(provide 'js-anki-annotation-push)

;; ----------------------------------------------------------------
;; To make per-source splitting exact, add a source tag in each backend:
;;
;;   Wallabag (wallabag-backend.el, in the tag-add call of the entry
;;   processor):  (org-roam-tag-add '("annotations" "wallabag"))
;;
;;   KOReader (koreader-json-backend.el): the file-based backends route
;;   through the CORE `annotation--process-entry', which hardcodes
;;   (org-roam-tag-add '("annotations")).  To tag per source you'd add a
;;   :source-tag entry key and have the core add it, or wrap the call.
;;   Simplest: have each backend set the node's tag after import.
;; ----------------------------------------------------------------
;;; js-anki-annotation-push.el ends here
