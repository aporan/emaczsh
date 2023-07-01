;; This file contains packages that are used for note-taking, writing, and
;; scheduling

(use-package darkroom
  :ensure t
  :config
  (setq darkroom-text-scale-increase 0.6))


(defun aporan/zk-blog ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/blog"))

(defun aporan/zk-poems ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/poems"))

(defun aporan/zk-books ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/books"))

(defun aporan/zk-notes ()
  (interactive)
  (setq zk-directory "~/Github/aporan.github.io/notes"))

(if (eq system-type 'darwin)
    (progn
      (defun aporan/zk-meetings ()
        (interactive)
        (setq zk-directory "~/bdance/meetings/"))
      ))

(use-package zk
  :ensure t
  :config
  (zk-setup-embark)
  (setq zk-directory "~/Github/aporan.github.io/notes"
        zk-file-extension "org"
        zk-file-name-separator "-"
        zk-directory-recursive t))
