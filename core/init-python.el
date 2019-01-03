;; init-python.el --- Initialize python configurations.
;;; Code:

;; Python Mode
(use-package python
  :bind (
         :map python-mode-map
         ("C-x C-e" . 'python-shell-send-region)
         )
  :config
  (use-package anaconda-mode
    :hook
    (python-mode . anaconda-mode)
    (python-mode . anaconda-eldoc-mode)
    :config
    (use-package company-anaconda
      :init
      (cl-pushnew (company-backend-with-yas 'company-anaconda) company-backends)
      )
    )
  )

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
