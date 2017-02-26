;;; phan.el --- Base support for phan in Emacs

;;; Commentary:
;;
;;; Code:

;; Variables and constants

(defgroup phan nil
  "Support for the PHP static analyzer phan in Emacs.

See URL `https://github.com/etsy/phan'."
  :group 'tools)

(defcustom phan-program-location 'local
  "The location of the phan program to use.

Can be a string, or the symbol `local' to use a version local to
the Emacs package.

If it is `local', it is necessary to have `git' installed on the
computer to clone the phan repository and update it."
  :type '(choice (const :tag "Use local phan installation" local)
                 (string :tag "Specify a custom phan location"))
  :group 'phan)

(defcustom phan-php-command "php"
  "PHP interpreter to use to run commands for phan."
  :type '(string)
  :group 'phan)

(defcustom phan-program-repository "https://github.com/etsy/phan"
  "Git repository for the phan program."
  :type '(string)
  :group 'phan)

(defcustom phan-composer-program-url "https://getcomposer.org/installer"
  "URL to the composer installer."
  :type '(string)
  :group 'phan)

(defconst phan-package-dir (file-name-directory (or load-file-name buffer-file-name))
  "The directory in which the `phan' package is installed.")

(defconst phan-local-phan-dir (expand-file-name "phan-php/" phan-package-dir)
  "The directory in which the phan program is installed.")

(defconst phan-composer-file (expand-file-name "composer.phar" phan-local-phan-dir)
  "Absolute path to the composer program.")

;; Local phan installation

(defun phan-clone-local-phan ()
  "Clone the phan repository to get the program files.

Return t when the clone succeeds."
  (unless (and (y-or-n-p "Install a local version of PHAN ?") (= (call-process "git" nil " *PHAN: clone*" nil "clone" phan-program-repository phan-local-phan-dir) 0))
    (display-buffer " *PHAN: clone*")
    (error "Error when cloning phan repository")))

(defun phan-ensure-local-phan-exists ()
  "Clone the phan local repository if it does not exist.

Does not totally install phan, just check if the repository has
been cloned."
  (unless (file-exists-p phan-local-phan-dir)
    (phan-clone-local-phan)))

(defun phan-install-composer ()
  "Install composer in the phan program directory."
  (let ((default-directory phan-local-phan-dir))
       (url-copy-file phan-composer-program-url (expand-file-name "installer" phan-local-phan-dir) t)
       (call-process phan-php-command nil " *PHAN: install composer*" nil "installer")
       (delete-file "installer")))

(defun phan-ensure-composer-exists ()
  "Install composer if it does not exists."
  (phan-ensure-local-phan-exists)
  (unless (file-exists-p phan-composer-file)
    (phan-install-composer)))

(defun phan-composer-install ()
  "Run composer install on phan."
  (phan-ensure-composer-exists)
  (let ((default-directory phan-local-phan-dir)
        (output-buffer " *PHAN: composer install*"))
    (unless (= (call-process phan-php-command nil output-buffer nil phan-composer-file "install") 0)
      (display-buffer output-buffer)
      (error "Error running 'composer install'"))))

(defun phan-install-local-phan ()
  "Install a local version of phan program."
  (phan-composer-install))

(defun phan-ensure-local-phan-is-installed ()
  "Install a local copy of phan when it does not exist."
  (phan-ensure-local-phan-exists)
  (unless (file-exists-p (expand-file-name "vendor" phan-local-phan-dir))
    (phan-install-local-phan)))

;; Generic variable accessing

(defun phan-get-phan-program ()
  "Return the phan program to use when running phan.

If `phan-program-location' is a string, return it.  It it is
`local', make sure phan is installed and return the path to the
local phan program."
  (if (stringp phan-program-location)
      phan-program-location
    (phan-ensure-local-phan-is-installed)
    (expand-file-name "phan" phan-local-phan-dir)))

;; Compilation

(add-to-list 'compilation-error-regexp-alist 'php-phan)
(add-to-list 'compilation-error-regexp-alist-alist
             `(php-phan . (,(rx bol (group (group (+ (not (any ":")))) ":"
                                           (group (+ digit))) " "
                                (group (+ (not (any " ")))) " "
                                (group (+ (not (any "\n")))) eol)
                           2 3 nil nil 1)))

;;;###autoload
(defun phan-run-on-directory (directory)
  "Run phan analysis on the specified directory."
  (interactive "Directory to scan: ")
  (let ((directory (expand-file-name directory))
        (program (phan-get-phan-program))
        (default-directory (or (locate-dominating-file (expand-file-name "false-file" directory) ".phan")
                               directory)))
    (compile (concat phan-php-command " "
                     (shell-quote-argument (phan-get-phan-program))
                     " --directory " (shell-quote-argument directory)))))

(provide 'phan)

;;; phan.el ends here
