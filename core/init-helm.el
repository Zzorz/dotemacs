;;; init-exwm.el --- Initialize c configurations.
;;; Code:

(use-package helm
  :ensure t
  :defer t
  :config
  (define-minor-mode chunyang-helm-window-hack-mode
    "Hack helm window display."
    :global t
    (let ((action '("\\`\\*helm"
                    (display-buffer-in-side-window)
                    (window-height . 0.4))))
      (if chunyang-helm-window-hack-mode
          (progn
            (add-to-list 'display-buffer-alist action)
            (setq helm-display-function #'display-buffer))
        (setq display-buffer-alist
              (delete action display-buffer-alist))
        (let ((standard-value (eval (car (get 'helm-display-function 'standard-value)))))
          (setq helm-display-function standard-value)))))
  (chunyang-helm-window-hack-mode))

;;; Setup of Helm's Sub-packages

(use-package helm-mode                ; Use helm completing everywhere
  :diminish helm-mode
  :after helm
  :preface
  (defun helm-completing-read-el-search-history (&rest _)
    (helm
     :sources (helm-build-sync-source "Resum El-search"
                :candidates (let (result)
                              (dotimes (i (ring-length el-search-history) (nreverse result))
                                (push
                                 (cons (el-search--get-search-description-string
                                        (ring-ref el-search-history i)
                                        t)
                                       (prin1-to-string i))
                                 result)))
                :multiline t)
     :buffer "*helm resum El-search*"))
  :config
  (add-to-list 'helm-completing-read-handlers-alist
               '(where-is . helm-completing-read-symbols))
  (add-to-list 'helm-completing-read-handlers-alist
               '(org-insert-link . nil))
  (add-to-list 'helm-completing-read-handlers-alist
               '(el-search-jump-to-search-head . helm-completing-read-el-search-history))
  (helm-mode))

;;; key bindings, M-0 to M-9
;; (progn
;;   (dolist (nth (number-sequence 1 9))
;;     (define-key helm-map (kbd (format "M-%d" nth))
;;       ;; In case `lexical-binding' is off
;;       `(lambda () (interactive) (helm-execute-candidate-action ,nth))))

;;   ;; Use M-0 for the tenth candidate
;;   (define-key helm-map (kbd "M-0")
;;     (lambda () (interactive) (helm-execute-candidate-action 10))))

(defun helm-execute-candidate-action (nth)
  "Move selection to Nth candidate and execute default action."
  (interactive)
  (when (<= nth (helm-get-candidate-number 'in-current-source))
    (let* ((count (- nth (helm-candidate-number-at-point)))
           (direction (if (> count 0) 'next 'previous)))
      (dotimes (_i (abs count))
        (helm-move-selection-common :where 'line
                                    :direction direction))
      (helm-maybe-exit-minibuffer))))

;; (setq helm-candidate-number-limit 100)

;; Defaults to 15, not working correctly in helm-occur, so disable it :(
(setq helm-highlight-matches-around-point-max-lines 0)

(use-package helm-adaptive
  :disabled t
  :init (helm-adaptive-mode))

(use-package helm-command               ; helm-M-x
  :defer t
  :config (setq helm-M-x-always-save-history t))

(use-package helm-buffers
  :defer t
  :config
  (setq helm-boring-buffer-regexp-list
        (append helm-boring-buffer-regexp-list
                (list
                 ;; C-h e
                 (rx string-start "*Messages*" string-end)
                 ;; C-h t
                 (rx string-start "*scratch*" string-end)
                 ;; C-h h
                 (rx string-start "*Help*" string-end))))

  (define-key helm-buffer-map [?\M-o] #'helm-buffer-switch-other-window)

  ;; "other window" action
  (defun chunyang-helm-create-buffer-other-window (buffer-name)
    (cl-letf (((symbol-function 'switch-to-buffer)
               #'switch-to-buffer-other-window))
      (let ((default-action
              (cdr (car (helm-attr 'action helm-source-buffer-not-found 'ignorefn)))))
        (funcall default-action buffer-name))))

  (let ((source helm-source-buffer-not-found)
        (action-name "Create buffer other window `M-o'"))
    ;; Avoid duplicates of the same action
    (helm-delete-action-from-source action-name source)
    (helm-add-action-to-source action-name
                               #'chunyang-helm-create-buffer-other-window
                               source
                               2))

  ;; key binding for the new action
  (defvar chunyang-helm-source-buffer-not-found-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map "\M-o" #'chunyang-helm-create-buffer-other-window-cmd)
      map))

  (helm-attrset 'keymap chunyang-helm-source-buffer-not-found-map
                helm-source-buffer-not-found)

  (defun chunyang-helm-create-buffer-other-window-cmd ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'chunyang-helm-create-buffer-other-window)))

  (defvar helm-source-ivy-view
    (helm-build-sync-source "Ivy Views"
      :candidates
      (lambda ()
        (require 'ivy)
        (mapcar (lambda (view)
                  (cons (car view) view))
                ivy-views))
      :nomark t
      :action
      (helm-make-actions
       "Open"
       (lambda (view)
         (delete-other-windows)
         (let (
               ;; silence "Directory has changed on disk"
               (inhibit-message t))
           (ivy-set-view-recur (cadr view))))
       "Delete"
       (lambda (view)
         (setq ivy-views (delete view ivy-views))))))

  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          ;; helm-source-ivy-view
          helm-source-recentf
          helm-source-buffer-not-found))

  ;; It can be very slow by checking remote files (Tramp)
  (setq helm-buffer-skip-remote-checking t))


;;; Use Ivy's view with Helm
;; https://oremacs.com/2016/06/27/ivy-push-view/

(use-package ivy
  :ensure t
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)))



(use-package helm-files
  :defer t
  :config
  (add-to-list 'helm-boring-file-regexp-list ".DS_Store")

  (define-key helm-find-files-map [?\M-o] #'helm-ff-run-switch-other-window)
  (define-key helm-generic-files-map [?\M-o] #'helm-ff-run-switch-other-window)

  (use-package helm-ls-git
    :ensure t
    :defer t
    :config
    (setq helm-ls-git-default-sources
          (delq 'helm-source-ls-git-status helm-ls-git-default-sources)))

  (use-package helm-ls-svn
    :disabled t
    :ensure t
    :bind ("M-8" . helm-ls-svn-ls))

  (use-package helm-fuzzy-find
    :disabled t
    :ensure t
    :commands helm-fuzzy-find))

(use-package helm-color                 ; Input colors with Helm
  :bind ("C-c i C" . helm-colors))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c i 8" . helm-unicode))

(use-package helm-grep
  ;; Must make sure `wgrep-helm' is available first and do NOT load it
  ;; since it is soft loaded in `helm-grep'
  :preface
  (use-package wgrep-helm :ensure t :defer t)
  (defun chunyang-helm-rg (type)
    "Search the current project with ripgrep."
    (interactive "P")
    (let ((default-directory
            (or (chunyang-project-root)
                default-directory))
          (helm-grep-ag-command
           "rg --color=always --smart-case --no-heading --line-number %s %s %s"))
      (helm-do-grep-ag type)))
  :defer t
  :bind ("M-I" . chunyang-helm-rg)
  :config
  (setq helm-grep-ag-command
        "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag))

(use-package helm-regexp
  :defer t
  :init (bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map)
  :config
  (defun isearch-from-helm-occur ()
    (interactive)
    (helm-run-after-exit
     (lambda (initial)
       (isearch-forward nil t)
       (isearch-yank-string initial))
     helm-pattern))
  (define-key helm-moccur-map "\C-s" #'isearch-from-helm-occur))

(use-package helm-ring
  :defer t
  :config
  (add-to-list 'helm-kill-ring-actions
               '("Yank(s)" .
                 (lambda (_candidate)
                   (helm-kill-ring-action
                    (mapconcat #'identity (helm-marked-candidates) "\n"))))))

(use-package helm-man
  :defer t
  :config
  ;; helm needs a relatively new man version, which is not provided on even
  ;; latest OS X (10.10) and also not available on MacPorts
  (setq helm-man-format-switches "%s"))

(use-package helm-elisp                 ; Helm commands for Emacs Lisp
  :bind ("C-c f l" . helm-locate-library))

;; Set up shorter key bindings
(bind-keys ("M-x"     . helm-M-x)
           ("C-c M-x" . execute-extended-command)
           ("C-x C-f" . helm-find-files)
           ;; ("C-x f"   . helm-recentf)
           ("C-x C-d" . helm-browse-project)
           ("M-l"     . helm-mini)
           ("M-y"     . helm-show-kill-ring)
           ("C-z"     . helm-resume)
           ("C-x r j" . helm-register)
           ("C-h a"   . helm-apropos)
           ("M-i"     . helm-occur)
           ("C-o"     . helm-semantic-or-imenu))

(use-package helm-ag
  :disabled t
  :ensure t
  :bind (("C-c S" . helm-do-ag)    ; C-u chooses file type, C-- enter own option
         ("C-c s" . helm-do-ag-project-root)
         ("C-c C-s" . helm-do-ag-project-root)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :commands helm-descbinds
  :init
  (setq helm-descbinds-window-style 'split-window)
  (advice-add 'describe-bindings :override #'helm-descbinds))

(use-package gh
  ;; Disable autoloads to reduce Emacs startup time, see
  ;; https://github.com/sigma/gh.el/issues/95
  :ensure (gh :type git :host github :repo "sigma/gh.el" :no-autoloads t)
  :defer t)

(use-package helm-open-github
  :ensure t
  :commands (helm-open-github-from-file ; Use the region for selecting specfic lines
             helm-open-github-from-issues
             helm-open-github-from-commit
             helm-open-github-from-pull-requests))

(use-package helm-github-stars
  :ensure t
  :defer t
  :config
  (add-hook 'helm-github-stars-clone-done-hook #'dired)
  (setq helm-github-stars-refetch-time (/ 6.0 24)
        helm-github-stars-full-frame t
        helm-github-stars-default-sources '(hgs/helm-c-source-stars
                                            hgs/helm-c-source-repos)))

(use-package helm-mu
  :ensure t
  :defer t
  :config
  (setq helm-mu-gnu-sed-program
        ;; Note that BSD Sed doesn't work
        (or (executable-find "gsed")
            (executable-find "sed"))
        helm-mu-skip-duplicates t))

(use-package helm-zhihu-daily    :ensure t :defer t)

(use-package helm-org
  :defer t
  :config (setq helm-org-headings-fontify t))


;;; mdfind(1)

(defun helm-mdfind ()
  "mdfind(1) within helm."
  (interactive)
  (require 'helm)
  (helm :sources
        (helm-build-async-source "mdfind"
          :candidates-process
          (lambda ()
            (let ((proc
                   (start-process-shell-command
                    "mdfind"
                    helm-buffer
                    (concat "mdfind " helm-pattern))))
              (prog1 proc
                (set-process-sentinel
                 proc
                 (lambda (_process _event))))))
          :nohighlight t
          :requires-pattern 2
          :action helm-find-files-actions)
        :buffer "*helm-mdfind*"))


;;; Manage advises
(defun advice--members (symbol)
  (let ((definition (advice--symbol-function symbol))
        (fns '()))
    (while (advice--p definition)
      (push (advice--car definition) fns)
      (setq definition (advice--cdr definition)))
    (nreverse fns)))

(defun helm-manage-nadvice ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Advices"
          :candidates
          (cl-remove-if-not
           (lambda (func-name)
             (and (featurep 'nadvice)
                  (advice--p (advice--symbol-function (intern func-name)))))
           (all-completions "" obarray #'fboundp))
          :coerce #'intern
          :action
          (lambda (candidate)
            (run-at-time
             0.01 nil
             (lambda (symbol)
               (message "`%s' is advised by `%s'"
                        symbol
                        (advice--members symbol))
               (helm :sources
                     (helm-build-sync-source (format "Remove advice(s) from %s" symbol)
                       :candidates (mapcar #'symbol-name (advice--members symbol))
                       :coerce #'intern
                       :action (lambda (ad) (advice-remove symbol ad)))))
             candidate)))))


;;; Manage Emacs's hook
(defun helm-manage-hooks ()
  ;; Note: It's much better to add a custom action to `helm-apropos', however,
  ;; its action is not customizable and I'm not sure this function is useful for
  ;; other helm users.
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (helm-build-sync-source "Choose hook"
            :candidates
            (all-completions "" obarray
                             (lambda (x)
                               (and (boundp x) (not (keywordp x))
                                    (string-match "-hook$" (symbol-name x)))))
            ;; TODO: This is needed to make sure :preselect is working (which
            ;; might be a bug of helm)
            :candidate-number-limit 9999
            :action
            (lambda (candidate)
              (run-at-time 0.01 nil
                           (lambda (hook)
                             (helm :sources
                                   (helm-build-sync-source (format "Manage function(s) from %s" hook)
                                     :candidates (mapcar #'symbol-name (symbol-value hook))
                                     :action (helm-make-actions
                                              "Remove this function from hook"
                                              (lambda (candidate)
                                                ;; Warn: I'm not handling the LOCAL argument
                                                (remove-hook hook (intern-soft candidate)))))))
                           (intern-soft candidate))))
          :preselect default)))


;; Utilities
(defmacro helm-string (string)
  `(helm :sources (helm-build-in-buffer-source "Helm String"
                    :data ,string)))

(defmacro helm-list (list)
  (let ((tempvar (make-symbol "stringify-list")))
    `(let ((,tempvar (mapcar (lambda (obj) (format "%s" obj)) ,list)))
       (helm :sources (helm-build-sync-source "Helm List"
                        :candidates ,tempvar)))))

(defun helm-shell-command (command)
  (interactive (list (read-shell-command "Shell command: ")))
  (helm-string (shell-command-to-string command)))


(provide 'init-helm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-exwm.el ends here
