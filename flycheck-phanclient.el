;;; flycheck-phanclient.el --- A flycheck checker for phan, using the phan daemon.

;;; Commentary:
;;
;; Based on emacs-phpphan, but using phan_client to quickly get a daemon to analyze the current file instead.
;; This code is experimental.

;;; Code:
(require 'flycheck)
(require 'php-project)

;; TODO: Use phan's severity level to choose between warning and info. Include that in the lines printed by phan_client.
;; TODO: Allow users to override the information included in message?

(defun flycheck-phanclient-start-daemon ()
  "Start the phan daemon"
  (interactive)
  (let ((default-directory (php-project-get-root-dir))
        (cmd '("phan" "--daemonize-tcp-port" "4846" "--quick")))
    (apply #'start-process "PhanDaemon" "*phan daemon*" cmd)))

(flycheck-define-checker php-phanclient
  "A PHP static analyzer using phan. Analyzes the file on buffer save.

See URL `https://github.com/etsy/phan'."
  :command ("phan_client" "-l" source-original "-f" source)
;; Alternately, use the below :command with the commented out :predicate to only run the check after file save.
;; :command ("phan_client" "-l" source-original)

  :error-patterns
  ((warning line-start (or "Parse" "Fatal" "syntax" "Phan") " error" (any ":" ",") " " (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode)
; We would work around this by passing the contents of the temporary file and the path to the original file to the daemon through phan_client
; However, if the "-f" option isn't used (and only source-original was used), we would have to limit this to when the buffer was saved(flycheck-buffer-saved-p)
;  :predicate flycheck-buffer-saved-p
  )
(flycheck-add-next-checker 'php '(warning . php-phanclient))
(add-to-list 'flycheck-checkers 'php-phanclient)

(provide 'flycheck-phanclient)
