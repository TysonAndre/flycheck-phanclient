;;; flycheck-phan.el --- A flycheck checker for phan

;;; Commentary:
;;
;; This code is experimental.  This may not be a good thing to run
;; heavy programs like phan with with flycheck. Prefer `phan-run-on-directory'.
;;

;;; Code:
(require 'flycheck)
(require 'phan)

(defvar phan-flycheck-directory ""
  "The direcotry to use to run PHAN in flycheck.")

(flycheck-define-checker php-phan
  "A PHP statical analyzer using phan.

See URL `https://github.com/etsy/phan'."
  :command ("php" (eval (phan-get-phan-program)) "--directory" (eval phan-flycheck-directory))
  :error-patterns
  ((info line-start (file-name) ":" line " " (message) line-end))
  :modes (php-mode))

(flycheck-add-next-checker 'php '(info . php-phan))
(add-to-list 'flycheck-checker 'php-phan)

(provide 'flycheck-phan)

;;; flycheck-phan.el ends here
