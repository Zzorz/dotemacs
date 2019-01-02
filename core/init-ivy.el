;;; init-ivy.el --- Initialize ivy configurations.
;;; Code:

(use-package ivy
  :config (ivy-mode 1)
  :bind (
         ("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h k" . counsel-descbinds)
         ("M-." . counsel-imenu)
         )
  )

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
