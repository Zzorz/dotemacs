;; init-edit.el --- Initialize edit configurations.
;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq make-backup-files nil)               ; Forbide to make backup files
(setq-default kill-whole-line t)           ; Kill line including '\n'

(setq-default major-mode 'python-mode)



(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :init (add-hook 'after-init-hook #'delete-selection-mode))


;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook #'global-auto-revert-mode))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :init
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-c ;" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
         ("C-c w" . avy-goto-word-0))
  :init (add-hook 'after-init-hook #'avy-setup-default)
  :config (setq avy-background t))

;; Quickly follow links
(use-package ace-link
  :bind (("M-o" . ace-link-addr))
  :init (add-hook 'after-init-hook #'ace-link-setup-default))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'after-init-hook #'global-aggressive-indent-mode)

  ;; FIXME: Disable in big files due to the performance issues
  ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
  (add-hook 'find-file-hook
            (lambda ()
              (if (> (buffer-size) (* 3000 80))
                  (aggressive-indent-mode -1))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> ")))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (add-hook 'after-init-hook #'drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :init (setq flyspell-issue-message-flag nil))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind (("C-'" . iedit-mode)
         :map isearch-mode-map ("C-'" . iedit-mode-from-isearch)
         :map esc-map ("C-'" . iedit-execute-last-modification)
         :map help-map ("C-'" . iedit-mode-toggle-on-function))
  :init
  ;; Avoid to restore Iedit mode when restoring desktop
  (add-to-list 'desktop-minor-mode-handlers
               '(iedit-mode . nil)))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-c ." . mc/mark-next-like-this)
         ("C-c ,". mc/mark-previous-like-this)
         ("C-c /". mc/mark-all-like-this)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind
  (("C-c i" . er/expand-region))
  )

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish subword-mode
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  (add-hook 'minibuffer-setup-hook #'subword-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
