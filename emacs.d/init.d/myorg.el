(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)                                                    ;; org agenda
         ("C-c c" . org-capture)                                                   ;; org capture
         ("C-c i" . org-set-tags-command))                                         ;; org insert tags
  :hook ((org-mode . auto-fill-mode)
         (org-mode . darkroom-tentative-mode)
         (org-agenda-finalize-hook . #'aporan/org-agenda-adjust-text-size))
  :config
  (add-to-list 'org-structure-template-alist                                       ;; add blog easy template for .org files
               '("B" . "#+TITLE: ?\n#+PART: Nil\n#+DATE:\n#+UPDATE:\n\n"))

  (setq org-src-fontify-natively t
        org-image-actual-width nil
        org-hide-leading-stars t
        org-hide-emphasis-markers nil                                              ;; show text in it's formatted style along with the shortcuts
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

  (setq org-priority-faces
        '((?A . (:foreground "DeepPink" :weight 'bold))
          (?B . (:foreground "tomato"))
          (?C . (:foreground "turquoise"))
          (?D . (:foreground "pale turquoise"))))
        
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                                                               ;; tasks which needs to be done
           "NEXT(n)"                                                               ;; tasks which are next in Q
           "WAITING(w@)"                                                           ;; waiting for someone / something
           "ALLOT(a@)"                                                             ;; work which is delegated
           "EPIC(e)"                                                               ;; tasks which are under way
           "IN-PROGRESS(p)" "|"                                                    ;; tasks which are under way
           "DONE(d!)"                                                              ;; completed tasks
           "CANCELLED(c@)")))                                                      ;; tasks which are no longer relevant


  (setq org-tag-alist '(("daily" . ?d)                                             ;; tasks for today
                        ("traverse" . ?t)                                          ;; career and life goal
                        ("unplanned" . ?u)))                                       ;; unplanned items in work / life

  (setq org-agenda-files '("~/Gitlab/organizer/tasks" "~/Gitlab/organizer/tasks/office")
        org-default-notes-file "~/Gitlab/organizer/tasks/orgnotes.org"
        org-agenda-breadcrumbs-separator " > "                                     ;; custom breadcrumbs separator in agenda view
        org-agenda-block-separator 12714                                           ;; 'empty' separator between different org agenda sections
        org-agenda-window-setup 'only-window                                       ;; open org-agenda in a new window
        org-agenda-skip-scheduled-if-deadline-is-shown t                           ;; skip scheduled if deadline is present
        org-agenda-hide-tags-regexp "errands\\|leisure\\|unplanned\\|academia\\|cuppa\\|traverse\\|daily" ;; hide these tags in agenda view
        org-agenda-prefix-format '(                                                ;; agenda view display category and filename
                                   (agenda . " %i %?-12t % s")
                                   (todo . " %i %(aporan/agenda-prefix)")
                                   (tags . " %i ")
                                   (search . " %i ")))

  (setq org-agenda-custom-commands                                                 ;; custom org-agenda view with 3 sections filtered according to priorities
        '(("h" "Home Overview"
           ((tags "daily"
                  ((org-agenda-skip-function
                    '(or (aporan/org-agenda-skip-tag "unplanned")
                         (org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "WAITING" "TODO" "CANCELLED"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "Progressing ⇈")))
            (tags "unplanned"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'done 'todo '("DONE" "CANCELLED"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "Unplanned 🦅")))
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-use-time-grid nil)
                        (org-agenda-skip-function
                         '(org-agenda-skip-subtree-if 'notregexp "habit"))
                        (org-agenda-overriding-header "Repeated Tasks/Habits ...?\n")))
            (tags "daily"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'nottodo '("NEXT"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "On-Platform 🛬")))
            (tags "daily"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'nottodo '("WAITING"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "Held 🐚")))
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-skip-function
                         '(org-agenda-skip-subtree-if 'regexp "habit"))
                        (org-agenda-overriding-header "What's happening?\n"))))

           ((org-agenda-files '("~/Gitlab/organizer/tasks/"))))


          ("b" "Backlog"
           ((tags "traverse"
                  ((org-agenda-skip-function
                    '(or (aporan/org-agenda-skip-tag "unplanned")
                         (aporan/org-agenda-skip-tag "daily")
                         (org-agenda-skip-entry-if 'todo 'done)
                         (org-agenda-skip-if nil '(scheduled))
                         (org-agenda-skip-entry-if 'nottodo 'todo)))
                   (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                   (org-agenda-overriding-header "⊶ Route53A"))))

           ((org-agenda-files '("~/Gitlab/organizer/tasks/"))))

          ("o" "Office View"
           ((tags "daily"
                  ((org-agenda-skip-function
                    '(or (aporan/org-agenda-skip-tag "unplanned")
                         (org-agenda-skip-entry-if 'todo '("DONE" "NEXT" "WAITING" "TODO" "ALLOT"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b")))
                   (org-agenda-overriding-header "Daily 📜 ┐")))
            (tags "unplanned"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'done 'todo '("DONE" "CANCELLED"))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "⇈ Unplanned")))
            (tags "daily"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'nottodo '("WAITING" "ALLOT"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "⇈ Held")))
            (tags "daily"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'nottodo '("NEXT"))
                         (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-prefix-format '((tags . "  %b ")))
                   (org-agenda-overriding-header "On-Platform 🛬")))
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-use-time-grid nil)
                        (org-agenda-skip-function
                         '(org-agenda-skip-subtree-if 'notregexp "habit"))
                        (org-agenda-overriding-header "Professional Habits\n")))
            (tags "traverse"
                  ((org-agenda-skip-function
                    '(or (aporan/org-agenda-skip-tag "unplanned")
                         (aporan/org-agenda-skip-tag "daily")
                         (org-agenda-skip-entry-if 'todo 'done)
                         (org-agenda-skip-if nil '(scheduled))
                         (org-agenda-skip-entry-if 'nottodo 'todo)))
                   (org-agenda-prefix-format '((tags . " %i %(aporan/agenda-prefix)")))
                   (org-agenda-overriding-header "Backlog 🧟‍♀️ ┐"))))

           ((org-agenda-files
             '("~/Gitlab/organizer/tasks/office/daily.org"))))
          ))

  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))

  (setq org-capture-templates
        '(
          ("o"                                                                     ;; tasks which needs to be done eventually
           "New Todo" entry (file+headline "~/Gitlab/organizer/tasks/office/unplanned.org" "Daily")
           "* TODO %? \n  <CREATED>: %(org-insert-time-stamp (org-read-date nil t \"+5d\")) DEADLINE: %^t\n  :LOGBOOK: \n    - State \"TODO\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)
          ("t"                                                                     ;; tasks which needs to be done eventually
           "New Todo" entry (file "")
           "* TODO %?%^g \n  :LOGBOOK: \n   - State \"TODO\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)
          ("u"                                                                     ;; tasks with schedule / deadline
           "Upcoming Tasks" entry (file "")
           "* UPCOMING %?%^g \n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+5d\"))\n  :LOGBOOK: \n    - State \"UPCOMING\" set at %U \n  :END: \n\n  %i\n" :empty-lines 1)))
  
  (require 'org-habit)
  (setq org-habit-graph-column 60)
  (require 'org-tempo))


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
          (concat str "↯ "))
      (let ((str ""))
        (concat str "↬ ")))))

(defun aporan/org-agenda-adjust-text-size ()
  (if (= text-scale-mode-amount 0)
      (text-scale-adjust 0.5)))
