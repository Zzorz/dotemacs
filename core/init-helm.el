;;; init-exwm.el --- Initialize c configurations.
;;; Code:

(use-package helm
  :defer t
  :diminish helm-mode
  :init
  ;; Make Helm look nice.
  (setq-default helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t
                ;; open Helm window in the current window where point is in
                helm-split-window-in-side-p t
                helm-ff-ido-style-backspace 'always)

  ;;(set-face-attribute 'helm-source-header nil :height 0.75)

  ;; Replace common selectors with Helm versions.
  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-r") #'helm-recentf)
  (global-set-key (kbd "C-x C-o") #'helm-occur)
  (global-set-key (kbd "C-c C-s") #'helm-do-grep-ag)

  :config
  ;;(require 'helm-config)
  ;;(require 'helm)

  ;; Activate Helm.
  (helm-mode 1)
  (helm-autoresize-mode -1))

(provide 'init-helm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-exwm.el ends here
