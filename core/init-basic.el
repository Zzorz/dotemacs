;; Environment
;;;Code:

(eval-when-compile (require 'init-const))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))


;; History
;; (use-package saveplace
;;   :ensure nil
;;   :init
;;   ;; Emacs 25 has a proper mode for `save-place'
;;   (if (fboundp 'save-place-mode)
;;       (add-hook 'after-init-hook #'save-place-mode)
;;     (setq save-place t)))

;; (use-package recentf
;;   :ensure nil
;;   :init
;;   (setq recentf-max-saved-items 200)

;;   ;; lazy load recentf
;;   ;; (add-hook 'after-init-hook #'recentf-mode)
;;   (add-hook 'find-file-hook (lambda () (unless recentf-mode
;;                                          (recentf-mode)
;;                                          (recentf-track-opened-file))))
;;   :config
;;   (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
;;   (add-to-list 'recentf-exclude "bookmarks"))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
