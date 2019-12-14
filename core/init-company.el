;; init-company.el --- Initialize company configurations.
;;; Code:

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind(
        :map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next)
        :map company-active-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next)
        )
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (use-package company-lsp
    :config
    (push 'company-lsp company-backends)
    )
  (use-package yasnippet
    :diminish yas-minor-mode
    :init (add-hook 'after-init-hook #'yas-global-mode)
    :config (use-package yasnippet-snippets)
    (push 'company-yasnippet company-backends)
    (yas-global-mode)
    )
  )


(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
