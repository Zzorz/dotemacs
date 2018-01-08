;; init-ruby.el --- Initialize ruby configurations.
;;; Code:

(use-package ruby-mode
  :ensure nil
  :mode "\\.\\(rb\\|rake\\|\\gemspec\\|ru\\|\\(Rake\\|Gem\\|Guard\\|Cap\\|Vagrant\\)file\\)$"
  :interpreter "ruby"
  :config
  ;; Code navigation, documentation lookup and completion for Ruby
  (use-package robe
    :diminish robe-mode
    :init
    (add-hook 'ruby-mode-hook #'robe-mode)

    (with-eval-after-load 'company
      (cl-pushnew (company-backend-with-yas 'company-robe) company-backends)))

  ;; Ruby refactoring helpers
  (use-package ruby-refactor
    :diminish ruby-refactor-mode
    :init (add-hook 'ruby-mode-hook #'ruby-refactor-mode-launch))

  ;; Run a Ruby process in a buffer
  (use-package inf-ruby
    :init
    (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
    (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

  ;; Rubocop
  (use-package rubocop
    :diminish rubocop-mode
    :init (add-hook 'ruby-mode-hook #'rubocop-mode))

  ;; RSpec
  (use-package rspec-mode
    :diminish rspec-mode
    :commands rspec-install-snippets
    :init (add-hook 'dired-mode-hook #'rspec-dired-mode)
    :config (with-eval-after-load 'yasnippet
              (rspec-install-snippets)))

  ;; Coverage for SimpleCov
  (use-package coverage)

  ;; Yet Another RI interface for Emacs
  (use-package yari
    :bind (:map ruby-mode-map ([f1] . yari)))

  ;; Ruby YARD comments
  (use-package yard-mode
    :diminish yard-mode
    :init (add-hook 'ruby-mode-hook #'yard-mode)))

;; YAML mode
(use-package yaml-mode)

(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here