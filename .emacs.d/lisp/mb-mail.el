(use-package mu4e
  :straight (:type built-in)
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ido-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Personal"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Mark Broeders")
                    (user-mail-address . "mail@markbroeders.nl")
                    (mu4e-sent-folder . "/Personal/mail/Sent")
                    (mu4e-trash-folder . "/Personal/mail/Trash")
                    (mu4e-drafts-folder . "/Personal/mail/drafts")
                    (mu4e-refile-folder . "/Personal/mail/Archive")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ;; ,(make-mu4e-context
          ;;   :name "Work"
          ;;   :match-func (lambda (msg) (when msg
          ;;                               (string-prefix-p "/Work" (mu4e-message-field msg :maildir))))

             ))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Use msmtp for sending e-mail
  (setq sendmail-program "msmtp"
        sendmail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/Personal/mail/inbox"        . ?i)
          ("/Personal/mail/Sent"         . ?s)
          ("/Personal/inbox/inbox"       . ?c)
          ("/Personal/lorentzlaan/inbox" . ?l)
          ("/Personal/mail/Trash"        . ?t)))

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "maildir:/Personal/mail/inbox OR maildir:/Personal/inbox/inbox OR maildir:/Personal/lorentzlaan/inbox"
                :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq mb/mu4e-inbox-query
        "(maildir:/Personal/mail/inbox OR maildir:/Personal/inbox/inbox OR maildir:/Personal/lorentzlaan/inbox) AND flag:unread")

  (defun mb/go-to-inbox ()
    (interactive)
    (mu4e-headers-search mb/mu4e-inbox-query))

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query mb/mu4e-inbox-query)

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))

(provide 'mb-mail)
