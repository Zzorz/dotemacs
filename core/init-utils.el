;; init-utils.el --- Initialize ultilities.
;;; Code:


;; Display available keybindings in popup
;; (use-package which-key
;;   :diminish which-key-mode
;;   :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
;;   :init (add-hook 'after-init-hook #'which-key-mode))


;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "~/.pyenv/shims")
  )

(use-package emojify
  :ensure t
  :init
  (global-emojify-mode)
  :config
  (progn
    (add-hook 'after-init-hook 'global-emojify-mode-line-mode)))

;; (require 'fira-code-mode)
;; Misc
;; (use-package fontawesome)
;; (use-package copyit) ;; copy path, url, etc.
;; (use-package diffview)               ; side-by-side diff view
;; (use-package esup)
;; (use-package htmlize)

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
