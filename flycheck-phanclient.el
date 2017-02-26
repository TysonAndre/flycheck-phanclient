;;; flycheck-phanclient.el --- A flycheck checker for phan, using the phan daemon.

;;; Commentary:
;;
;; Based on emacs-phpphan, but using phan_client to quickly get a daemon to analyze the current file instead.
;; This code is experimental.

;;; Code:
(require 'flycheck)

(flycheck-define-checker php-phanclient
  "A PHP static analyzer using phan. Analyzes the file on buffer save.

See URL `https://github.com/etsy/phan'."
  :command ("phan_client" "-l" source-original)
;; TODO: Use phan's severity level to choose between warning and info. Include that in the lines printed by phan_client.
  :error-patterns
  ((warning line-start (or "Parse" "Fatal" "syntax" "Phan") " error" (any ":" ",") " " (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode)
;; Since we check the original file, we can only use this phan checker if
;; the buffer is actually linked to a file, and if it is not modified.
;; TODO: Work around this by passing the contents of the temporary file and the path to the original file to the daemon through phan_client
;; TODO: This predicate doesn't work if the previous doesn't have the predicate, or if there is a previous checker.
  :predicate flycheck-buffer-saved-p
  )
(flycheck-add-next-checker 'php '(warning . php-phanclient))
(add-to-list 'flycheck-checkers 'php-phanclient)

(provide 'flycheck-phanclient)
