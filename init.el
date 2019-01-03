;; ;; Added by Package.el.  This must come before configurations of
;; ;; installed packages.  Don't delete this line.  If you don't want it,
;; ;; just comment it out by adding a semicolon to the start of the line.
;; ;; You may delete these explanatory comments.
;; (package-initialize)

;; Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

;; Prefers the newest version of a file
;; (setq load-prefer-newer t)

;; Load path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;; Packages
(require 'init-package)

;; Preferences
(require 'init-basic)
(require 'init-utils)
(require 'init-ui)

;;EXWM
;; (require 'init-exwm)

(require 'init-company)
(require 'init-edit)
(require 'init-ivy)
;; (require 'init-helm)

(require 'init-yasnippet)
(require 'init-highlight)

(require 'init-shell)
;; (require 'init-calendar)
;; (require 'init-dired)
;; (require 'init-org)


;; (require 'init-eshell)
;; (require 'init-ibuffer)
;; (require 'init-kill-ring)
;; (require 'init-window)


;; Programming
(require 'init-flycheck)

(require 'init-emacs-lisp)
(require 'init-c)
;; (require 'init-go)
(require 'init-python)
(require 'init-ruby)
(require 'init-web)
;; (require 'init-prog)

;; Restore
;; (require 'init-restore)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
