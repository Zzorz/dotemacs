;; init-shell.el --- Initialize shell configurations.
;;; Code:

;; Shell

(eval-when-compile (require 'init-const))

(use-package shell
  :ensure nil
  :config
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

  (defun n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input)
    (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
    (setq comint-input-sender 'n-shell-simple-send))

  (defun n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer)
      )
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command))))

  (add-hook 'shell-mode-hook #'n-shell-mode-hook))

;; Company mode backend for shell functions
(with-eval-after-load 'company
  (use-package company-shell
    :init (cl-pushnew '(company-shell company-shell-env company-fish-shell) company-backends)))

;; Multi termp
(use-package multi-term)

;; Shell Pop
(use-package shell-pop
  :bind ([C-M-return] . shell-pop)
  :init
  (let ((val
         '("ansi-term" "*ansi-term*"
           (lambda () (ansi-term shell-pop-term-shell)))))
    (setq shell-pop-shell-type val))
  (setq shell-pop-window-position 'bottom)
  (setq shell-pop-window-size 35)
  (setq shell-pop-full-span t)
  )


(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
