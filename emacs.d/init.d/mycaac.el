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
        ledger-report-use-native-highlighting nil)                                  ;; REFER: https://github.com/simonmichael/hledger/issues/367#issuecomment-433678314

  (defun highlight-negative-amounts nil                                             ;; useful when running reports in a shell buffer
    (interactive)
    (highlight-regexp "\\(\\$-\\|-\\$\\)[.,0-9]+" (quote hi-red-b)))

  (defvar aporan/ledger-report-liquid-assets
    (list "liquid-assets"
          (concat "%(binary) "                                                       ;; REFER: https://unconj.ca/blog/using-hledger-with-ledger-mode.html
                  "-f %(ledger-file) bal -B --flat "
                  "assets:pa assets:s assets:f assets:g assets:d assets:bank:ch")))

  (defvar aporan/ledger-report-netincome
    (list "report-netincome"
          (concat "%(binary) " 
                  "-f %(ledger-file) bal -M -A --flat --transpose --pretty-tables "
                  "netincome retained earnings")))

  (defvar aporan/ledger-report-blackhole
    (list "report-blackhole"
          (concat "%(binary) " 
                  "-f %(ledger-file) bal -M -A --flat --transpose --pretty-tables "
                  "blackhole")))

  (defvar aporan/ledger-report-daily-expenses
    (list "day-expenses"
          (concat "%(binary) "
                  "-f %(ledger-file) bal expenses "
                  "--tree --average --row-total -ED --pretty-tables -p 'this week'")))

  (defvar aporan/ledger-report-month-weekly-expenses
    (list "mnth-wk-expenses"
          (concat "%(binary) "
                  "-f %(ledger-file) bal expenses "
                  "--tree --average --row-total -EW --pretty-tables -p 'this month'")))

  (defvar aporan/ledger-report-year-weekly-expenses
    (list "year-wk-expenses"
          (concat "%(binary) "
                  "-f %(ledger-file) bal expenses "
                  "--tree --average --row-total -EW --pretty-tables -p 'this year to today'")))

  (defvar aporan/ledger-report-monthly-expenses
    (list "mnth-expenses"
          (concat "%(binary) "
                  "-f %(ledger-file) bal expenses "
                  "--tree --no-total --row-total --average -M --pretty-tables -p 'this year to today'")))

  (defvar aporan/ledger-report-balance-sheet-cost
    (list "balance-sheet-cost"
          (concat "%(binary) "
                  "-f %(ledger-file) bse -B -p 'this year to today' "
                  "--flat -M --pretty-tables")))

  (defvar aporan/ledger-report-balance-sheet-curr-month-cost
    (list "balance-sheet--curr-mnth-cost"
          (concat "%(binary) "
                  "-f %(ledger-file) bse -B -p 'this month' "
                  "--flat --pretty-tables")))

  (defvar aporan/ledger-report-net-account-balance
    (list "net-account-balance"
          (concat "%(binary) "
                  "-f %(ledger-file) bal -BAE "
                  "--tree -M --pretty-tables")))

  (defvar aporan/ledger-report-exp-budget                                        ;; budget functions
    (list "exp-budget"
          (concat "%(binary) "
                  "-f %(ledger-file) -f ~/Gitlab/ppocket/budget.journal bal -BE "
                  "--budget cur:SGD --no-total --pretty-tables -p 'this month' "
                  "--tree expenses")))

  (defvar aporan/ledger-report-exp-cum-budget
    (list "exp-cum-budget"
          (concat "%(binary) "
                  "-f %(ledger-file) -f ~/Gitlab/ppocket/budget.journal bal -B -M "
                  "--budget --cumulative cur:SGD --no-total --pretty-tables -p 'from last month to next month' "
                  "--tree expenses")))

  (defvar aporan/ledger-report-savings-forecast
    (list "savings-forecast"
          (concat "%(binary) "
                  "-f %(ledger-file) -f ~/Gitlab/ppocket/budget.journal bal -B -M "
                  "--historical --cumulative --forecast cur:SGD --no-total --pretty-tables -p 'this year' "
                  "--tree assets:d assets:subscriptions assets:f asset:g assets:savings:r "
                  "assets:savings:h assets:savings:e assets:savings:i")))

  (add-to-list 'ledger-reports aporan/ledger-report-liquid-assets)
  (add-to-list 'ledger-reports aporan/ledger-report-netincome)
  (add-to-list 'ledger-reports aporan/ledger-report-blackhole)
  (add-to-list 'ledger-reports aporan/ledger-report-daily-expenses)
  (add-to-list 'ledger-reports aporan/ledger-report-year-weekly-expenses)
  (add-to-list 'ledger-reports aporan/ledger-report-month-weekly-expenses)
  (add-to-list 'ledger-reports aporan/ledger-report-monthly-expenses)
  (add-to-list 'ledger-reports aporan/ledger-report-net-account-balance)
  (add-to-list 'ledger-reports aporan/ledger-report-balance-sheet-cost)
  (add-to-list 'ledger-reports aporan/ledger-report-balance-sheet-curr-month-cost)
  (add-to-list 'ledger-reports aporan/ledger-report-exp-budget)
  (add-to-list 'ledger-reports aporan/ledger-report-exp-cum-budget)
  (add-to-list 'ledger-reports aporan/ledger-report-savings-forecast))
