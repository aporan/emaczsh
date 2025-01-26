;; This file contains packages that are used for note-taking, writing, and
;; scheduling

(use-package darkroom
  :ensure t
  :config
  (setq darkroom-text-scale-increase 0.6))


(defun aporan/zk-blog ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/blog"
        zk-file-extension "org"))

(defun aporan/zk-poems ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/poems"
        zk-file-extension "org"))

(defun aporan/zk-books ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/books"
        zk-file-extension "org"))

(defun aporan/zk-notes ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/notes"
        zk-file-extension "org"))

(if (eq system-type 'darwin)
    (progn
      (defun aporan/zk-work ()
        (interactive)
        (setq zk-directory "~/bdance/workjournal/"
              zk-file-extension "md"))
      ))

(use-package zk
  :ensure t
  :config
  (zk-setup-embark)
  (setq zk-directory "~/Github/aporan.github.io/notes"
        zk-file-extension "org"
        zk-file-name-separator "-"
        zk-directory-recursive t))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode  (("\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . auto-fill-mode)
         (gfm-mode . auto-fill-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package ox-publish
  :config

  ;; publish without tracking file changes
  (setq org-publish-use-timestamps-flag nil)

  (setq org-publish-project-alist
      `(("poems"
         :base-directory "~/Github/aporan.github.io/poems/"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/Github/aporan.github.io/publish/poems/"
         :publishing-function org-html-publish-to-html)

       ("career"
         :base-directory "~/Github/aporan.github.io/career/"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/Github/aporan.github.io/publish/career/"
         :publishing-function org-html-publish-to-html)

       ("notes"
         :base-directory "~/Github/aporan.github.io/notes/"
         :base-extension "org"
         :publishing-directory "~/Github/aporan.github.io/publish/notes/"
         :publishing-function org-html-publish-to-html)

        ("static"
         :base-directory "~/Github/aporan.github.io/static/"
         :base-extension "css\\|txt\\|jpg\\|gif\\|png\\|ttf"
         :recursive t
         :publishing-directory "~/Github/aporan.github.io/publish/static/"
         :publishing-function org-publish-attachment)

        ("aporan.org" :components ("poems" "career" "notes" "static")))))

