(setq org-directory "~/Documents/org"

      ;; Used for org roam and deft
      org-roam-directory "~/Documents/org/notes"

      ;; Needed fot my GTD workflow
      org-default-notes-file "~/Documents/org/notes/main.org"

      ;; Personal/Family get synced wih org-caldav
      ;; inbox is my GTD inbox file
      org-agenda-files '( "~/Documents/org/calendar/personal.org"
                          "~/Documents/org/calendar/family.org"
                          "~/Documents/org/gtd/inbox.org"
                          "~/Documents/org/gtd/agenda.org"
                          "~/Documents/org/gtd/projects.org"
                          "~/Documents/org/gtd/notes.org"))

(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/Documents/org/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("m" "Meeting" entry  (file+headline "~/Documents/org/gtd/agenda.org" "Future")
         ,(concat "* %? :meeting:\n"
                  "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry  (file "gtd/notes.org")
         ,(concat "* Note (%a)\n"
                  "/Entered on/ %U\n" "\n" "%?"))
        ("@" "Inbox [mu4e]" entry (file "gtd/inbox.org")
         ,(concat "* TODO Reply to \"%a\" %?\n"
                  "/Entered on/ %U"))

        ;; This is a first menu --> Sports
        ("m" "Metrics Capture / Sports")
        ("mc" "Fietsen" table-line
         (file+headline "~/Documents/org/gtd/metrics.org" "Cycling")
         "| %U | %^{Distance} | %^{Avg speed} | %^{Duration} | %^{Avg heartrate} | %^{Notes} |" :kill-buffer t)
        ("mr" "Hardlopen" table-line
         (file+headline "~/Documents/org/gtd/metrics.org" "Running"))
        ("mw" "Weight" table-line
         (file+headline "~/Documents/org/gtd/metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)

        ;; And a second menu for my hobbies
        ("h" "Hobbies")
        ("hb" "Book entry" entry (file+headline "~/Documents/org/gtd/books.org" "2021")
         "* %^{prompt|TODO|READING|DONE} %^{Title}\n:PROPERTIES:\n:author: %?\n:END:\n" :prepend t :empty-lines-after 1)))

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun org-capture-mail ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Key bindings
(define-key global-map            (kbd "C-c a") 'org-agenda)
(define-key global-map            (kbd "C-c c") 'org-capture)
(define-key global-map            (kbd "C-c i") 'org-capture-inbox)

(require 'mu4e)
(define-key mu4e-headers-mode-map (kbd "C-c i") 'org-capture-mail)
(define-key mu4e-view-mode-map    (kbd "C-c i") 'org-capture-mail)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
    '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
       (when (member (buffer-file-name) org-agenda-files)
         t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
      (lambda (&rest _)
        (gtd-save-org-buffers)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")
        (sequence "PLANNED(p)" "READING(r)" "|" "DONE(d!)" "CANC(k@")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(setq org-log-done 'time)

(setq org-agenda-custom-commands
    '(("g" "Get Things Done (GTD)"
       ((agenda ""
                ((org-agenda-span 'day)
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-deadline-warning-days 0)))
        (todo "NEXT"
              ((org-agenda-skip-function
                '(org-agenda-skip-entry-if 'deadline))
               (org-agenda-prefix-format "  %i %-12:c [%e] ")
               (org-agenda-overriding-header "\nTasks\n")))
        (agenda nil
                ((org-agenda-entry-types '(:deadline))
                 (org-agenda-format-date "")
                 (org-deadline-warning-days 7)
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                 (org-agenda-overriding-header "\nDeadlines")))
        (tags-todo "inbox"
                   ((org-agenda-prefix-format "  %?-12t% s")
                    (org-agenda-overriding-header "\nInbox\n")))
        (tags "CLOSED>=\"<today>\""
              ((org-agenda-overriding-header "\nCompleted today\n")))))))

(provide 'mb-workflow)
