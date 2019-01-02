;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'init-const))

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Menu/Tool/Scroll bars
(when (and (fboundp 'menu-bar-mode) menu-bar-mode)
    (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode)
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode)
  (scroll-bar-mode -1))
(set-fringe-mode 0)

;; Theme
;; (use-package doom-themes
;;  :init (load-theme 'doom-one t))

;; Modeline
(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (display-time-mode)
  )

;; Fonts
(use-package cnfonts
  :init
  (add-hook 'after-init-hook #'cnfonts-enable)
  :config
  (setq cnfonts-keep-frame-size nil)
  (setq cnfonts-profiles
        '("program1" "program2" "program3" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program1" . 4)
                                  ("program2" . 5)
                                  ("program3" . 3)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(use-package linum-off
  :demand
  :init (add-hook 'after-init-hook #'global-linum-mode)
  :config ;; (setq linum-format "%4d "))
  (use-package hlinum
    :init
    (hlinum-activate))
  )

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(size-indication-mode 1)
(blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; (use-package window-numbering
;;   :init
;;   (window-numbering-mode))

(defun set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(set-transparency 87)
(toggle-frame-fullscreen)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
