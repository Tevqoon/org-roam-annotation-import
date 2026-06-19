(use-package org-roam-annotation-import
  :after org anki-editor
  :vc (:url "https://github.com/Tevqoon/org-roam-annotation-import" :rev :newest)
  :bind* ("C-c n p r a" . wallabag-synchronise-annotations)
  :config
  (require 'wallabag-backend))

(use-package js-anki-annotation-push
  :load-path "~/Documents/repos/org-roam-annotation-import/"  ; or wherever you put it
  :after (anki-editor org-roam-annotation-import)
  :bind
  (("C-c n p z" . js/anki-push-zotero)
   ("C-c n p w" . js/anki-push-wallabag)
   ("C-c n p k" . js/anki-push-koreader)
   ("C-c n p A" . js/anki-push-all-annotations)
   ("C-c n p a" . js/anki-push-recent-annotations)))

(use-package zotero-backend
  :load-path "~/Documents/repos/org-roam-annotation-import/"
  :after org-roam-annotation-import
  :bind* ("C-c n p r z" . zotero-synchronise-annotations)
  :custom
  (zotero-anki-deck "Zotero")
  :init
  (with-eval-after-load 'ol
    (org-link-set-parameters
     "zotero"
     :follow (lambda (path _)
               (let ((uri (concat "zotero:" path)))
                 (pcase system-type
                   ('darwin     (call-process "open" nil nil nil uri))
                   ('gnu/linux  (call-process "xdg-open" nil nil nil uri))
                   (_           (browse-url uri))))))))
