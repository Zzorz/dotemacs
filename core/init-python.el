;; init-python.el --- Initialize python configurations.
;;; Code:

;; Python Mode
;; Autopep8

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :config
  (use-package py-autopep8)
  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi)
    )
  )


(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
