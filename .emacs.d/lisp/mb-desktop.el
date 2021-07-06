(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; This function takes the name of the wallpaper, defined in Systems.org, and sets the
;; correct wallpaper. You could also just write a fixed path here.
(defun efs/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil
   (format "feh --bg-scale ~/.dotfiles/backgrounds/samuel-ferrara-uOi3lg8fGl4-unsplash.jpg")))

(defun efs/bind-function (key invocation &rest bindings)
  "Bind KEYs to FUNCTIONs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (funcall ',invocation)))
    (setq key (pop bindings)
          invocation (pop bindings))))

(defun efs/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (efs/run-in-background ,command)))
    (setq key (pop bindings)
          command (pop bindings))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  (eshell)

  ;; NOTE: The next two are disabled because we now use Polybar!

  ;; Show battery status in the mode line
  ;;(display-battery-mode 1)

  ;; Show the time and date in modeline
  ;;(setq display-time-day-and-date t)
  ;;(display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Start the Polybar panel
  (efs/start-panel)

  ;; Launch apps that will run in the background
  (efs/run-in-background "dunst")
  (when (string= (system-name) "wheeler")
    (efs/run-in-background "nm-applet"))
  (efs/run-in-background "udiskie -t"))
;; (efs/run-in-background "syncthing-gtk --minimized"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Qutebrowser" (exwm-workspace-rename-buffer (format "QB: %s" exwm-title)))))

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun efs/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Qutebrowser" (exwm-workspace-move-window 2))
    ("Spotify"     (exwm-workspace-move-window 4))
    ("mpv"         (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (efs/configure-window-by-class)

              ;; Hide the modeline on all X windows
              (exwm-layout-hide-mode-line)))

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; NOTE: Uncomment the following two options if you want window buffers
  ;;       to be available on all workspaces!

  ;; Automatically move EXWM buffer to current workspace when selected
  ;; (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  ;; (setq exwm-workspace-show-all-buffers t)

  ;; NOTE: Uncomment this option if you want to detach the minibuffer!
  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;;(setq exwm-workspace-minibuffer-position 'top)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  ;; (require 'exwm-randr)
  ;; (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  ;; (setq exwm-randr-workspace-monitor-plist '(2 "Virtual-2" 3 "Virtual-2"))

  ;; NOTE: Uncomment these lines after setting up autorandr!
  ;; React to display connectivity changes, do initial display update
  ;; (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  ;; (efs/update-displays)

  ;; Set the wallpaper after changing the resolution
  (efs/set-wallpaper)

  ;; NOTE: This is disabled because we now use Polybar!
  ;; Load the system tray before exwm-init
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  ;; (exwm-systemtray-enable)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Run Qutebrowser on window 2
  (defun efs/run-qute ()
    (efs/run-in-background "qutebrowser")
    (exwm-workspace-switch-create 2))

  ;; Run Qutebrowser with META-O
  ;; Kill buffer with META-q
  (efs/bind-function
   "s-o" 'efs/run-qute
   "s-q" 'kill-buffer)

  (efs/bind-command
   "s-p" "playerctl play-pause"
   "s-[" "playerctl previous"
   "s-]" "playerctl next")

  ;;  ;; Use hydra for some nice window resizing
  ;;  (defhydra hydra-exwm-move-resize (:timeout 4)
  ;;  "Move/Resize Window (Shift is bigger steps, Ctrl moves window)"
  ;;  ("j" (lambda () (interactive) (exwm-layout-enlarge-window 10)) "V 10")
  ;;  ("J" (lambda () (interactive) (exwm-layout-enlarge-window 30)) "V 30")
  ;;  ("k" (lambda () (interactive) (exwm-layout-shrink-window 10)) "^ 10")
  ;;  ("K" (lambda () (interactive) (exwm-layout-shrink-window 30)) "^ 30")
  ;;  ("h" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 10)) "< 10")
  ;;  ("H" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 30)) "< 30")
  ;;  ("l" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 10)) "> 10")
  ;;  ("L" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 30)) "> 30")
  ;;  ("C-j" (lambda () (interactive) (exwm-floating-move 0 10)) "V 10")
  ;;  ("C-S-j" (lambda () (interactive) (exwm-floating-move 0 30)) "V 30")
  ;;  ("C-k" (lambda () (interactive) (exwm-floating-move 0 -10)) "^ 10")
  ;;  ("C-S-k" (lambda () (interactive) (exwm-floating-move 0 -30)) "^ 30")
  ;;  ("C-h" (lambda () (interactive) (exwm-floating-move -10 0)) "< 10")
  ;;  ("C-S-h" (lambda () (interactive) (exwm-floating-move -30 0)) "< 30")
  ;;  ("C-l" (lambda () (interactive) (exwm-floating-move 10 0)) "> 10")
  ;;  ("C-S-l" (lambda () (interactive) (exwm-floating-move 30 0)) "> 30")
  ;;  ("f" nil "finished" :exit t))

  ;; ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-\C-r] . exwm-reset)
          ([?\s-r] . hydra-exwm-move-resize/body)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app)
  (exwm-input-set-key (kbd "<s-return>") 'vterm)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)


  ;; This next function integrates slock, a suckless screen locker utility, into =EXWM=.
  ;; I copied this snippet from Rasendubi's configuration
  ;; (https://github.com/rasendubi/dotfiles#slock).

  (defun mb/blank-screen ()
    "Blank screen after 1 second. The delay is introduced so the user
    could get their hands away from the keyboard. Otherwise, the screen
    would lit again immediately."
    (interactive)
    (run-at-time "1 sec" nil
                 (lambda ()
                   (efs/run-in-background "xset dpms force off"))))

  (defun mb/lock-screen ()
    "Lock and blank screen."
    (interactive)
    (efs/run-in-background "slock")
    (mb/blank-screen))

  ;; Super + l locks screen
  (exwm-input-set-key (kbd "s-M-l") #'mb/lock-screen)

  (exwm-enable))

(use-package desktop-environment
  :straight t
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

(defun dw/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun dw/update-polybar-exwm ()
  (efs/send-polybar-hook "exwm" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'dw/update-polybar-exwm)
;; (add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

(defun mb/bluetooth-connect-Edifier ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 40:EF:4C:94:F6:B9"))

(defun mb/bluetooth-connect-Buds ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 18:54:CF:10:DB:85"))

(defun mb/bluetooth-disconnect ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- disconnect"))

(provide 'mb-desktop)
