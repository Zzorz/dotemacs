;; init-utils.el --- Initialize ultilities.
;;; Code:

(eval-when-compile (require 'init-const))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook #'which-key-mode))

;; A tree layout file explorer
(use-package treemacs
  :bind (("C-c t"        . treemacs-toggle))
  :config
  (setq treemacs-follow-after-init          t
        treemacs-width                      30
        treemacs-indentation                2
        treemacs-collapse-dirs              0
        treemacs-silent-refresh             t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil
        treemacs-is-never-other-window      nil
        treemacs-goto-tag-strategy          'refetch-index)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))

;; Projectile integration for treemacs
(use-package treemacs-projectile
  :bind (("C-c p t" . treemacs-projectile-toggle))
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))



;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))





;; Search utils: `ag', `rg', `pt'
(use-package ag
  :init
  (with-eval-after-load 'projectile
    (bind-key "s s" 'ag-project projectile-command-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package pt
  :init
  (with-eval-after-load 'projectile
    (bind-key "s p" 'projectile-pt projectile-command-map)))

(use-package rg
  :init
  (add-hook 'after-init-hook #'rg-enable-default-bindings)
  (if (fboundp 'wgrep-ag-setup)
      (add-hook 'rg-mode-hook #'wgrep-ag-setup))
  :config
  (setq rg-custom-type-aliases nil)
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (with-eval-after-load 'projectile
    (bind-key "s r" 'rg-project projectile-command-map))

  (when (fboundp 'ag)
    (bind-key "a" 'ag rg-global-map))
  (when (fboundp 'pt-regexp)
    (bind-key "P" 'pt-regexp rg-global-map))

  (with-eval-after-load 'counsel
    (bind-key "c r" 'counsel-rg rg-global-map)
    (bind-key "c s" 'counsel-ag rg-global-map)
    (bind-key "c p" 'counsel-pt rg-global-map)
    (bind-key "c f" 'counsel-fzf rg-global-map))

  (with-eval-after-load 'counsel-projectile
    (bind-key "s r" 'rg-project counsel-projectile-command-map)))

;; ;; Tramp
;; (use-package docker-tramp)

;; Emoji
(when my-emoji-enabled
  (use-package emojify
    :init (add-hook 'after-init-hook #'global-emojify-mode)
    :config
    (with-eval-after-load 'company
      (use-package company-emoji
        :init (add-to-list 'company-backends 'company-emoji)))))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; Log keyboard commands to buffer
(use-package command-log-mode
  :diminish (command-log-mode . "¢")
  :init (setq command-log-mode-auto-show t))

(setq make-backup-files nil)

(setq initial-scratch-message nil)


;; Misc
(use-package fontawesome)
(use-package copyit)                    ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package htmlize)                   ; covert to html
(use-package list-environment)
(use-package memory-usage)
(use-package open-junk-file)
(use-package try)
(use-package ztree)                     ; text mode directory tree. Similar with beyond compare

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
