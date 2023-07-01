(require 'package)                                             ;; load GNU ELPA packages

(add-to-list 'package-archives                                 ;; add melpa to package archives for more packages
             '("melpa" . "https://melpa.org/packages/"))

(setq package-enable-at-startup nil                            ;; disable package loading after the init file
      backup-directory-alist '(("." . "~/.emacs-saves"))       ;; SET back-ups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(package-initialize)                                           ;; load packages explicitly in the init file

(unless (package-installed-p 'use-package)                     ;; install `use-package` if its not installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile                                             ;; REQUIRES
  (require 'use-package))                                         ;; ensure-use package is available during the compilation step
                                                                  ;; as some variables are necessary; this also reduces the overall load time

(use-package color-theme-modern                                ;; my default theme, till i write my own theme (if ever)
  :ensure t
  :init
  (load-theme 'cobalt t t)
  (enable-theme 'cobalt))


(let ((directory "~/.emacs.d/init.d/"))                        ;; Load the rest of the files as specified here
  (dolist (filename '(sane                                     ;; global settings
                      gui                                      ;; packages related to visual changes [except theme]
                      mynav                                    ;; packages for navigating around emacs: includes searching, jumping
                      myorg                                    ;; configuration related to organizing my life
                      writing                                  ;; packages dedicated for making writing simple
                      coding                                   ;; packages for simple built in coding type 
                      ide                                      ;; packaegs for allowing emacs to behave like an ide
                      mycaac                                   ;; modes for configuration like filetypes
                      myvim                                    ;; machine/os specific configurations
                      unsorted))                               ;; packages that hasn't stood the test of time
    (load (format "%s%s" directory filename)))) ;; load each packages declared above
