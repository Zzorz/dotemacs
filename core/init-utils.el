;; init-utils.el --- Initialize ultilities.
;;; Code:

(eval-when-compile (require 'init-const))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook #'which-key-mode))


;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c y" . youdao-dictionary-search-at-point+))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))





;; Emoji
(when my-emoji-enabled
  (use-package emojify
    :init (add-hook 'after-init-hook #'global-emojify-mode)
    :config
    (with-eval-after-load 'company
      (use-package company-emoji
        :init (cl-pushnew (company-backend-with-yas 'company-emoji ) company-backends)))))

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
;; (use-package fontawesome)
;; (use-package copyit)
                                        ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
;; (use-package esup)
                                        ; Emacs startup profiler
(use-package htmlize)                   ; covert to html

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
