;; handle all package stuffs.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (org latex-preview-pane ripgrep nlinum multiple-cursors dumb-jump adaptive-wrap)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :width normal)))))

;; font size
(set-face-attribute 'default nil :height 180)

;; show line numbers by default during start up
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d ")

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)

;; set tab-width for C to default to 4
(setq-default c-basic-offset 4)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

;; highlight brackets
(setq show-paren-style 'parenthesis)

;; auto refresh buffer when file is changed
(global-auto-revert-mode t)

;; enable dump-jump
(dumb-jump-mode)
(global-set-key (kbd "M-s n") 'dumb-jump-go)
(global-set-key (kbd "M-s p") 'dumb-jump-back)
(global-set-key (kbd "M-s q") 'dumb-jump-quick-look)

;; show-trailing-whitespace
(setq-default show-trailing-whitespace t)

;; enable ido-mode vertically
(require 'ido)
(ido-mode 1)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "M-s M-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "M-s M-r") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "M-s d") 'mc/mark-all-like-this)
(global-set-key (kbd "M-s M-a") 'mc/edit-lines)

;; ORG
(require 'org-habit)

;; org agenda files
(setq org-agenda-files (quote ("~/Gitlab/org-anizer/tasks")))

;; global keybindings : explicitly needs to be set by user
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-ca" 'org-agenda)

(setq org-hide-leading-stars t)
(setq org-todo-keywords
       '((sequence "TODO(t)" "UPCOMING(u)" "WAITING(w@)" "IN-PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(c@)" "ASSIGNED(a@)")))


(defun calendar-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

   PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("w" "Custom Compact View"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "high-priority tasks:")))
          (agenda "" ((org-agenda-ndays 2)))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (calendar-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))
                          (org-agenda-overriding-header "everything else:")))))))


(setq org-enforce-todo-dependencies t)
(setq org-agenda-window-setup 'only-window)
(setq org-log-into-drawer t)
;; END ORG

;; Disable truncate lines
(set-default 'truncate-lines t)

;; wrap lines only in org mode
(setq visual-line-mode t)
(add-hook 'org-mode-hook 'visual-line-mode)

;; addition to wrap lines beautifully in org mode
;; https://emacs.stackexchange.com/questions/7432/make-visual-line-mode-more-compatible-with-org-mode
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)
