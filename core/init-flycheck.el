;; init-flycheck.el --- Initialize flycheck configurations.
;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Display Flycheck errors in GUI tooltips
  (use-package flycheck-pos-tip
    :init (flycheck-pos-tip-mode 1)
    :config (setq flycheck-pos-tip-timeout 15))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :init (avy-flycheck-setup)))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
