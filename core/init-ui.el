;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-
;;; Code:

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
;;(set-fringe-mode 0)

;; Theme
(use-package srcery-theme
  :config (load-theme 'srcery t))

;; Modeline
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )

;; Fonts
(use-package cnfonts
  :config
  ;; (add-hook 'after-init-hook #'cnfonts-enable)
  (cnfonts-enable)
  (setq cnfonts-keep-frame-size nil)
  (cnfonts-set-spacemacs-fallback-fonts)
  )

(use-package linum-relative
  :config
  (global-linum-mode)
  ;; (linum-relative-on)
  (linum-on)
  )

;; Misc
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(size-indication-mode 1)
(blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 93 93))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
