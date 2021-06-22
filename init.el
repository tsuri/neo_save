;;; remove path to the builtin org mode
(require 'cl-seq)
;; (setq load-path
;;       (cl-remove-if
;;        (lambda (x)
;;          (string-match-p "org$" x))
;;        load-path))

(require 'subr-x) ; for string-remove-suffix
(defun mav/litter-directory (leaf-dir &optional version)
  (let* ((dir (directory-file-name
               (file-name-directory user-emacs-directory)))
         (distribution (string-remove-suffix ".d" dir))
         (version-dir (if version version "")))
    (file-name-as-directory (format "%s-save.d/%s/%s" distribution leaf-dir version-dir))))

(setq package-user-dir (mav/litter-directory "packages" emacs-version))
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(setq package-archive-priorities
      '(("org" . 50)
        ("melpa-stable" . 40)
        ("gnu" . 30)
        ("melpa" . 10)))
(setq package-menu-hide-low-priority t)
(require 'package)
(package-initialize)
(package-refresh-contents)

(defun package-from-archive (f &rest args)
  (message (symbol-name (car args)))
  (if (car args) 'org
    (apply f rgs)
    (and (apply f args)
	 (assq (car args) package-alist))))

;;; org is a builtin package, hence `package-install` wouldn't
;;; install it. We advise package-installed-p to answer false
;;; for builtin packages.
;(advice-add 'package-installed-p :around #'package-from-archive)
(package-install 'org)
(require 'org)
;(advice-remove 'package-installed-p #'package-from-archive)

;;; NOTE: maybe this is enough:
;;; (assq-delete-all 'org package--builtins)
;;; (assq-delete-all 'org package--builtin-versions)
;;; instead of removing from loadpath + the advice above

(let ((mav-org
       (concat (file-name-as-directory user-emacs-directory) "mav.org")))
  (org-babel-load-file mav-org))
