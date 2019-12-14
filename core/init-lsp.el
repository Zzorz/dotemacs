(use-package lsp-mode
  :hook (python-mode . lsp)
  :commands lsp
  :config
  (setq lsp-log-io t)
  )

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(provide 'init-lsp)
