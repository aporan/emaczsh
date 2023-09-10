(setq custom-file "~/.emacs.d/init.d/custom-file.el")       ;; Press `C-h v custom-file` for info
(load custom-file)

(global-auto-revert-mode t)                                  ;; auto load buffer after file is changed
(put 'narrow-to-region 'disabled nil)                        ;; allow narrow to region

(fringe-mode 3)                                              ;; fringe border is now half-width on either side
(menu-bar-mode -1)                                           ;; remove menu bar from display
(tool-bar-mode -1)                                           ;; remove tool bar from display
(scroll-bar-mode -1)                                         ;; remove scroll bar from display
(show-paren-mode t)                                          ;; highlight paranthesis pair
(set-face-attribute 'vertical-border nil                     ;; vertical border face is similar to background
                    :foreground "Gray2"
                    :background "Gray2")
(set-face-attribute 'default nil                             ;; font face and style
                    :family "Ubuntu Mono"
                    :foundry "DAMA"
                    :height 200)


(setq inhibit-startup-screen t                               ;; disable emacs startup splash screen
      ;; default-frame-alist '((fullscreen . fullboth))      ;; always open emacs in fullscreen
      ansi-color-faces-vector [default default default italic underline success warning error])

(setq-default fill-column 78                                 ;; wrap columns higher than this
              indent-tabs-mode nil                           ;; uses spaces instead of tabs
              tab-width 4                                    ;; tab display character width in columns
              truncate-lines t)                              ;; prevent texts from bleeding over the edge

;; move to coding
              ;; c-basic-offset 4)                           ;; uses 4 spaces for indentation in a c/cpp based file

; move these to magit
      ;; vc-handled-backends nil                                    ;; don't use emacs builtin versioning system
      ;; version-control t)
