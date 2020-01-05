(require 'package)                                                                            ;; load package file

(add-to-list 'package-archives                                                                ;; add melpa to package archives for more packages
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)                                                                          ;; load packages explicitly in the init file

(setq                                                                             ;; SET back-ups
 backup-directory-alist '(("." . "~/.emacs-saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq                                                                             ;; SET default global variables
                                                                                        ;; visual
 package-enable-at-startup nil                                                                ;; disable package loading after the init file
 inhibit-startup-screen t                                                                     ;; disable emacs startup splash screen
 default-frame-alist '((fullscreen . fullboth))                                               ;; always open emacs in fullscreen

 ansi-color-faces-vector [default default default italic underline success warning error])

(setq-default                                                                     ;; SET default local variables set globally
 fill-column 78                                                                               ;; wrap columns higher than this
 indent-tabs-mode nil                                                                         ;; uses spaces instead of tabs
 tab-width 4                                                                                  ;; tab display character width in columns
 truncate-lines t                                                                             ;; prevent texts from bleeding over the edge
                                                                                        ;; prog
 c-basic-offset 4)                                                                            ;; uses 4 spaces for indentation in a c/cpp based file

                                                                                  ;; COMMANDS
                                                                                        ;; functs
(global-auto-revert-mode t)                                                                   ;; auto load buffer after file is changed
(put 'narrow-to-region 'disabled nil)                                                         ;; allow narrow to region
                                                                                        ;; visual
(load-theme 'tango-dark t)                                                                    ;; load built-in theme
(fringe-mode 3)                                                                               ;; fringe border is now half-width on either side
(menu-bar-mode -1)                                                                            ;; remove menu bar from display
(tool-bar-mode -1)                                                                            ;; remove tool bar from display
(scroll-bar-mode -1)                                                                          ;; remove scroll bar from display
(show-paren-mode t)                                                                           ;; highlight paranthesis pair
(set-face-attribute 'vertical-border nil                                                      ;; vertical border face is similar to background
                    :foreground "Gray2"
                    :background "Gray2")
(set-face-attribute 'default nil                                                              ;; font face and style
                    :family "Ubuntu Mono"
                    :foundry "DAMA"
                    :slant 'normal
                    :height 200
                    :weight 'normal
                    :width 'normal)


(unless (package-installed-p 'use-package)                                                    ;; install `use-package` if its not installed
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile                                                                ;; REQUIRES
  (require 'use-package))                                                                     ;; reduces load time

(use-package ibuffer
             :ensure t
             :bind (("C-x C-b" . ibuffer))
             :init
             (add-hook 'ibuffer-mode-hook
                       (lambda ()
                         (ibuffer-switch-to-saved-filter-groups "default")))
             :config
             (setq ibuffer-saved-filter-groups
                   (quote (("default"
                            ("dired"
                             (mode . dired-mode))
                            ("org"
                             (name . "^.*org$"))
                            ("magit"
                             (mode . magit-mode))
                            ("shell"
                             (or
                              (mode . eshell-mode) (mode . shell-mode)))
                            ("programming"
                             (or
                              (mode . clojure-mode)
                              (mode . clojurescript-mode)
                              (mode . python-mode)
                              (mode . c++-mode)))
                            ("emacs"
                             (or
                              (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$"))))))))

(use-package bind-key
             :ensure t)

(use-package org
             :ensure t
             :mode ("\\.org\\'" . org-mode)
             :bind (("C-c a" . org-agenda)                                                    ;; org agenda
                    ("C-c c" . org-capture)                                                   ;; org capture
                    ("C-c i" . org-set-tags-command))                                         ;; org insert tags
             :hook ((org-mode . auto-fill-mode)
                    (org-mode . darkroom-tentative-mode))
             :config
             (defun aporan/org-skip-subtree-if-priority (priority)                            ;; REFER: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
               "Skip an agenda subtree if it has a priority of PRIORITY.

                PRIORITY may be one of the characters ?A, ?B, ?C, or ?D."
               (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                     (pri-value (* 1000 (- org-lowest-priority priority)))
                     (pri-current (org-get-priority (thing-at-point 'line t))))
                 (if (= pri-value pri-current)
                     subtree-end
                   nil)))

             (defun aporan/org-agenda-skip-tag (tag &optional others)                         ;; REFER: https://stackoverflow.com/a/10091330
               "Skip all entries that correspond to TAG.

                If OTHERS is true, skip all entries that do not correspond to TAG."
               (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
                     (current-headline (or (and (org-at-heading-p)
                                                (point))
                                           (save-excursion (org-back-to-heading)))))
                 (if others
                     (if (not (member tag (org-get-tags-at current-headline)))
                         next-headline
                       nil)
                   (if (member tag (org-get-tags-at current-headline))
                       next-headline
                     nil))))

             (defun aporan/agenda-prefix ()                                                   ;; REFER: https://emacs.stackexchange.com/questions/9735/org-agenda-tags-todo-hierarchy-weirdness
               (format "%s" (aporan/agenda-indent-string (org-current-level) (org-get-priority (thing-at-point 'line t)))))

             (defun aporan/agenda-indent-string (level priority)
               (if (= level 1)
                   ""
                 (if (= priority 0)
                     (let ((str ""))
                       (while (> level 2)
                         (setq level (1- level)
                               str (concat str " ")))
                       (concat str "  ↯ "))
                   (let ((str ""))
                     (concat str "↬ ")))))


             (add-to-list 'org-structure-template-alist                                       ;; add blog easy template for .org files
                          '("B" "#+TITLE: ?\n#+PART: Nil\n#+DATE:\n#+UPDATE:\n\n"))


             (setq org-src-fontify-natively t)                                                ;; syntax highlighting for code block

             (setq org-capture-templates
                   '(
                     ("t"                                                                     ;; tasks which needs to be done eventually
                      "New Todo" entry (file "")
                      "* TODO %?%^g \n  :LOGBOOK: \n   - State \"TODO\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)
                     ("u"                                                                     ;; tasks with schedule / deadline
                      "Upcoming Tasks" entry (file "")
                      "* UPCOMING %?%^g \n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+5d\"))\n  :LOGBOOK: \n    - State \"UPCOMING\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)))

             (setq org-priority-faces
                   '((?A . (:foreground "DeepPink" :weight 'bold))
                     (?B . (:foreground "tomato"))
                     (?C . (:foreground "turquoise"))
                     (?D . (:foreground "pale turquoise"))))

             (setq org-todo-keywords
                   '((sequence
                      "TODO(t)"                                                               ;; tasks which needs to be done
                      "UPCOMING(u)"                                                           ;; tasks with schedule / deadlines
                      "WAITING(w@)"                                                           ;; waiting for someone / something
                      "IN-PROGRESS(p)" "|"                                                    ;; tasks which are under way
                      "DONE(d!)"                                                              ;; completed tasks
                      "CANCELLED(c@)"                                                         ;; tasks which are no longer relevant
                      "ASSIGNED(a@)")))                                                       ;; delegated items to someone

             (setq org-tag-alist '(("academia" . ?a)                                          ;; categorize header tag list
                                   ("blog" . ?b)                                              ;; related to blogging
                                   ("errands" . ?e)                                           ;; related repeated / one time chores / errands
                                   ("leisure" . ?l)                                           ;; related to casual / reading / enjoyment
                                   ("μtodays" . ?u)                                           ;; office work for each day
                                   ("vocation" . ?v)))                                        ;; career and life goal

             (setq org-hide-leading-stars t
                   org-enforce-todo-dependencies t                                            ;; enforce children dependencies on parents for todo's
                   org-lowest-priority 69                                                     ;; change lowest priority number to extend priority values
                   org-deadline-warning-days 7                                                ;; change early warning days
                   org-default-priority ?E                                                    ;; change default priority
                   org-refile-targets '((org-agenda-files :level . 2))                        ;; refile to all available agenda files
                   org-refile-use-outline-path 'file                                          ;; refile using filenames as top level
                   org-refile-allow-creating-parent-nodes 'confirm                            ;; prompt when creating top level refiles
                   org-outline-path-complete-in-steps nil                                     ;; use ivy to select names instead of stepping through
                   org-log-into-drawer t                                                      ;; log finished tasks into drawers
                   org-log-reschedule 'time                                                   ;; make drawer notes when scheduled time is updated
                   org-log-redeadline 'time)                                                  ;; make drawer notes when deadline is updated

             (setq org-agenda-files '("~/Gitlab/organizer/tasks" "~/Gitlab/organizer/tasks/office")
                   org-default-notes-file "~/Gitlab/organizer/tasks/orgnotes.org"
                   org-agenda-block-separator ?                                               ;; 'empty' separator between different org agenda sections
                   org-agenda-window-setup 'only-window                                       ;; open org-agenda in a new window
                   org-agenda-skip-scheduled-if-deadline-is-shown t                           ;; skip scheduled if deadline is present
                   org-agenda-hide-tags-regexp "errands\\|leisure\\|academia\\|vocation\\|μtodays" ;; hide these tags in agenda view
                   org-agenda-prefix-format '(                                                ;; agenda view display category and filename
                                              (agenda . " %i %?-12t % s")
                                              (todo . " %i %(aporan/agenda-prefix)")
                                              (tags . " %i ")
                                              (search . " %i ")))


             (setq org-agenda-custom-commands                                                 ;; custom org-agenda view with 3 sections filtered according to priorities
                   '(("p" "Personal View"
                      ((tags "PRIORITY=\"A\""
                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                              (org-agenda-overriding-header "⇈ High-Priority")))
                       (agenda "" ((org-agenda-span 3)))
                       (tags "academia"
                             ((org-agenda-skip-function
                               '(or (org-agenda-skip-entry-if 'todo 'done)
                                    (org-agenda-skip-if nil '(scheduled))
                                    (org-agenda-skip-entry-if 'nottodo 'todo)
                                    (aporan/org-skip-subtree-if-priority ?A)))
                              (org-agenda-prefix-format '((tags . " %b %(aporan/agenda-prefix)")))
                              (org-agenda-overriding-header "🐾 Learning Topics")))
                       (tags "vocation"
                             ((org-agenda-skip-function
                               '(or (org-agenda-skip-entry-if 'todo 'done)
                                    (org-agenda-skip-if nil '(scheduled))
                                    (aporan/org-skip-subtree-if-priority ?A)))
                              (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                              (org-agenda-overriding-header "♉  Vocation")))
                       (tags "leisure"
                             ((org-agenda-skip-function
                               '(or (org-agenda-skip-entry-if 'todo 'done)
                                    (org-agenda-skip-if nil '(scheduled))
                                    (aporan/org-skip-subtree-if-priority ?A)))
                              (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                              (org-agenda-overriding-header "♪~ ᕕ(ᐛ)ᕗ Leisure")))
                       (tags "errands"
                             ((org-agenda-skip-function
                               '(or (org-agenda-skip-entry-if 'todo 'done)
                                    (org-agenda-skip-if nil '(scheduled))
                                    (aporan/org-skip-subtree-if-priority ?A)))
                              (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                              (org-agenda-overriding-header "🐾 Errands")))
                       (alltodo ""
                                ((org-agenda-skip-function
                                  '(or (aporan/org-skip-subtree-if-priority ?A)
                                       (aporan/org-skip-subtree-if-priority ?B)
                                       (aporan/org-agenda-skip-tag "academia")
                                       (aporan/org-agenda-skip-tag "vocation")
                                       (aporan/org-agenda-skip-tag "leisure")
                                       (aporan/org-agenda-skip-tag "errands")
                                       (aporan/org-agenda-skip-tag "leisure")
                                       (org-agenda-skip-if nil '(scheduled deadline))))
                                 (org-agenda-overriding-header "🐾 Everything-Else"))))
                      ((org-agenda-files '("~/Gitlab/organizer/tasks/"))))))

             (require 'org-habit))

(use-package org-sidebar
            :ensure t
            :bind (:map org-mode-map
                        ("C-c b" . org-sidebar)
                        ("C-c C-b" . org-sidebar-tree-toggle)))

(use-package adaptive-wrap
             :ensure t
             :hook (org-mode . adaptive-wrap-prefix-mode))

(use-package multiple-cursors
             :ensure t
             :bind (("C-. C-n" . mc/mark-next-like-this-word)
                    ("C-. C-p" . mc/mark-previous-like-this-word)
                    ("C-. C-a" . mc/mark-all-like-this)
                    ("C-. C-e" . mc/edit-lines)))

(use-package ripgrep
             :ensure t)

(use-package which-key
             :ensure t
             :init
             (setq which-key-idle-delay 0.1)
             (which-key-mode)
             :config
             (which-key-show-major-mode))

(use-package ivy
             :ensure t
             :bind (("C-c C-s" . ivy-resume)
                    ("C-c C-o" . ivy-occur))
             :init
             (ivy-mode 1)
             :config
             (setq ivy-display-style 'fancy                                                   ;; highlight typed words in the selection
                   ivy-use-virtual-buffers t))

(use-package counsel
             :ensure t
             :bind (("M-x" . counsel-M-x)
                    ("C-c C-f" . counsel-find-file)
                    ("C-c g" . counsel-git)
                    ("C-c r" . counsel-git-grep)
                    ("C-c l" . counsel-locate)
                    ("C-'" . counsel-linux-app)))

(use-package swiper
             :ensure t
             :bind (("C-s" . swiper)))

(use-package projectile
             :ensure t
             :bind ("C-c p" . projectile-command-map)
             :config
             (projectile-mode t)
             (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
             :ensure t
             :init
             (counsel-projectile-mode))

(use-package editorconfig
             :ensure t
             :init
             (editorconfig-mode 1))

(use-package ng2-mode
             :ensure t)

(use-package dockerfile-mode
             :ensure t)

(use-package docker-compose-mode
             :ensure t)

(use-package restclient
             :ensure t)

(use-package dotenv-mode
             :ensure t)

(use-package vue-mode
             :ensure t)

(use-package magit
             :ensure t)

(use-package forge
             :after magit
             :ensure t
             :config
             (add-to-list 'forge-alist '("gitlab.usec.io" "gitlab.usec.io:8888/api/v4" "www.usec.io" forge-gitlab-repository)))

(use-package dart-mode
            :init
            (setq dart-sdk-path "~/Github/flutter/bin/cache/dart-sdk/"))

(use-package company
            :ensure t
            :defer t
            :diminish company-mode
            :hook (ledger-mode . company-mode))

(use-package flycheck
            :ensure t
            :hook ((python-mode . flycheck-mode)
                   (ledger-mode . flycheck-mode))
            :config
            (add-to-list 'flycheck-checkers 'python-pyflakes)
            (add-to-list 'flycheck-disabled-checkers 'python-flake8)
            (add-to-list 'flycheck-disabled-checkers 'python-pylint)

            (use-package flycheck-pyflakes
                        :after flycheck
                        :ensure t
                        :config
                        (setq flycheck-python-pyflakes-executable "pyflakes3"))

            (use-package flycheck-color-mode-line
                        :after flycheck
                        :ensure t
                        :hook (flycheck-mode . flycheck-color-mode-line-mode))

            (use-package flycheck-inline
                        :after flycheck
                        :ensure t
                        :hook (flycheck-mode . flycheck-inline-mode))

            (use-package flycheck-ledger
                        :after flycheck
                        :ensure t))

(use-package dired
            :ensure nil                                                                       ;; non-existent package in package.el (dired is default)
            :hook (dired-mode . dired-hide-details-mode)
            :config
            (use-package diredfl
                        :ensure t
                        :config
                        (diredfl-global-mode 1))

            (use-package dired-git-info
                        :ensure t
                        :bind (:map dired-mode-map
                                    (")" . dired-git-info-mode))))
(use-package ledger-mode
            :ensure t
            :mode "\\.journal\\'"
            :hook (ledger-mode . auto-revert-tail-mode)
            :config
            (setq ledger-binary-path "hledger"                                                ;; use hledger backend instead of ledger
                  ledger-report-links-in-register nil                                         ;; disables ledger checks
                  ledger-mode-should-check-version nil
                  ledger-accounts-file "~/Gitlab/ppocket/accounts.journal")

            (setq ledger-post-amount-alignment-column 64                                      ;; move default amount position right allowing longer account names
                  ledger-highlight-xact-under-point nil)                                      ;; disable highlighting of transactions

            (setq ledger-report-auto-width nil                                                ;; enables to use hledger reports
                  ledger-report-use-native-highlighting nil)                                  ;; https://github.com/simonmichael/hledger/issues/367#issuecomment-433678314

            (defun highlight-negative-amounts nil                                             ;; useful when running reports in a shell buffer
              (interactive)
              (highlight-regexp "\\(\\$-\\|-\\$\\)[.,0-9]+" (quote hi-red-b)))

            (defvar aporan/ledger-report-monthly-expenses
              (list "monthly-expenses"
                    (concat "%(binary)"                                                       ;; https://unconj.ca/blog/using-hledger-with-ledger-mode.html
                            "-f %(ledger-file) bal expenses "
                            "--tree --no-total --row-total --average --monthly --pretty-tables"))
              "Custom configuration to provide monthly expenses. The idea is originally taken
from https://unconj.ca/blog/using-hledger-with-ledger-mode.html"
              )

            (add-to-list 'ledger-reports aporan/ledger-report-monthly-expenses))

(use-package flymake
            :disabled)

;; start visual
(use-package eyebrowse
             :disabled)

(use-package darkroom
            :ensure t
            :config
            (setq darkroom-text-scale-increase 1))

(use-package telephone-line
             :ensure t
             :config
             (setq telephone-line-lhs
                     '((accent . (telephone-line-buffer-modified-segment))
                       (nil . (telephone-line-vc-segment))
                       (accent . (telephone-line-projectile-segment))
                       (nil . (telephone-line-buffer-name-segment
                               telephone-line-filesize-segment
                               telephone-line-atom-encoding-segment))))
             (setq telephone-line-rhs
                     '((nil    . (telephone-line-narrow-segment
                                  telephone-line-flycheck-segment
                                  telephone-line-process-segment))
                       (accent . (telephone-line-major-mode-segment))
                       (nil    . (telephone-line-position-segment
                                  telephone-line-hud-segment))))
             (telephone-line-mode t))

(use-package rainbow-delimiters
             :ensure t
             :hook ((prog-mode . rainbow-delimiters-mode)
                    (text-mode . rainbow-delimiters-mode)))

(use-package dimmer
             :ensure t
             :init
             (dimmer-mode t)
             :config
             (setq dimmer-fraction 0.4)
             (dimmer-configure-which-key))

(use-package whitespace
             :ensure t
             :hook ((prog-mode . whitespace-mode)
                    (org-mode . whitespace-mode))
             :init
             (setq whitespace-line-column 90
                   whitespace-style '(face lines-tail)
                   show-trailing-whitespace t)
             :config
             (set-face-attribute 'whitespace-line nil
                                 :foreground "SlateGray3"
                                 :background 'unspecified))
;; end visual

(use-package markdown-mode
             :ensure t
             :commands (markdown-mode gfm-mode)
             :mode  (("\\.md\\'" . gfm-mode))
             :init
             (setq markdown-command "multimarkdown"))

(use-package calfw
             :ensure t)

(use-package calfw-org
             :ensure t
             :after (calfw)
             :bind (("C-c t" . cfw:open-org-calendar))
             :init
             (setq cfw:render-line-breaker 'cfw:render-line-breaker-none                     ;; truncate long lines
                   cfw:face-item-separator-color 'unspecified)
             :config
             (set-face-attribute 'cfw:face-title nil                                         ;; year and month title
                                 :foreground "#f0dfaf"
                                 :height 1.6
                                 :inherit 'variable-pitch)

             (set-face-attribute 'cfw:face-header nil                                        ;; weekday header
                                 :foreground "##d0bf8f"
                                 :background nil
                                 :weight 'bold
                                 :height 1.0)

             (set-face-attribute 'cfw:face-sunday nil                                        ;; sunday header
                                 :foreground "#cc9393"
                                 :background "Gray10"
                                 :weight 'bold)

             (set-face-attribute 'cfw:face-saturday nil                                      ;; saturday header
                                 :foreground 'unspecified
                                 :background "Gray10")

             (set-face-attribute 'cfw:face-select nil                                        ;; today highlight
                                 :background "#fff7d7")

             (set-face-attribute 'cfw:face-today-title nil                                   ;; today title
                                 :foreground "red4"
                                 :background 'unspecified
                                 :height 1.0)

             (set-face-attribute 'cfw:face-today nil                                         ;; current days tasks 
                                 :foreground "#f9e07c"
                                 :height 1.0)

             (set-face-attribute 'cfw:face-toolbar nil                                       ;; entire toolbar
                                 :foreground 'unspecified
                                 :background 'unspecified
                                 :height 1.0)

             (set-face-attribute 'cfw:face-toolbar-button-on nil                             ;; active tool bar
                                 :foreground "#cc9393"
                                 :background "Gray10"
                                 :height 1.0)

             (set-face-attribute 'cfw:face-toolbar-button-off nil                            ;; in-active toolbar buttons
                                 :foreground "Gray60"
                                 :background "##d0bf8f"
                                 :height 1.0))

(use-package try
             :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" default)))
 '(package-selected-packages
   (quote
    (telephone-line dimmer rainbow-delimiters which-key company-ledger company flycheck-ledger ledger-mode darkroom vue-mode dotenv-mode restclient dockerfile-mode ng2-mode ripgrep dart-mode olivetti diredfl dired-git-info dired org-sidebar flycheck-inline flycheck-color-mode-line flycheck-pyflakes flycheck python gnu-elpa-keyring-update forge counsel-projectile try calfw-org calfw magit docker-compose-mode editorconfig counsel multiple-cursors adaptive-wrap use-package)))
 '(paradox-github-token t))

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
