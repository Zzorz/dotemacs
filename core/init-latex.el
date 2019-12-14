(use-package auctex
  :defer t)
(use-package company-auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))
(use-package latex-math-preview)
(provide 'init-latex)
