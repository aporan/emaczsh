(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)                                                    ;; org agenda
         ("C-c c" . org-capture)                                                   ;; org capture
         ("C-c i" . org-set-tags-command))                                         ;; org insert tags
  :hook ((org-mode . auto-fill-mode)
         (org-mode . darkroom-tentative-mode)))
