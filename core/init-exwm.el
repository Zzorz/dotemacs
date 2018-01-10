;;; init-exwm.el --- Initialize c configurations.
;;; Code:
(use-package exwm)
(require 'exwm)
(require 'exwm-systemtray)
(require 'exwm-randr)
(require 'exwm-cm)
(setq exwm-workspace-number 4)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
;; 's-r': Reset
(exwm-input-set-key (kbd "s-k") #'exwm-reset)
;; ;; 's-N': Switch to certain workspace
;; (exwm-input-set-key (kbd "s-<tab>") #'exwm-workspace-switch-to-buffer)

(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;; Launch application
(exwm-input-set-key (kbd "s-i")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "rofi" nil "rofi -show window")))
(exwm-input-set-key (kbd "s-z")
                    (lambda (command)
                      (interactive (list (read-shell-command "Command:  ")))
                      (start-process-shell-command command nil command)))

(exwm-input-set-key (kbd "s-q") (exwm-layout--exit))

;; Line-editing shortcuts
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)
   ([?\C-d] . delete)
   ([?\C-k] . (S-end delete))))
;; Enable EXWM


;; Make all Emacs frames opaque.
(setq window-system-default-frame-alist '((x . ((alpha . 100)))))
;; Assign everything else a 80% opacity.
(setq exwm-cm-opacity 90)

(setq exwm-randr-workspace-output-plist '(0 "DP1" 1 "HDMI2"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI2 --pos 0x0 --auto --output eDP1 --primary --mode 1920x1080 --pos 0x1080")))

(setq exwm-workspace-minibuffer-position 'bottom)
(setq exwm-workspace-display-echo-area-timeout '5)

(exwm-randr-enable)
(exwm-systemtray-enable)
(exwm-cm-enable)
(exwm-enable)

(provide 'init-exwm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-exwm.el ends here
