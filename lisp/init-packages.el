(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
;; cl - Common Lisp Extension
(require 'cl)
;; Add Packages
(defvar lisatiy/packages '(
			   ;; --- Auto-completion ---
			   company
			   ;; --- Better Editor ---
			   hungry-delete
			   swiper
			   counsel
			   smartparens
			   ;; --- Major Mode ---
			   js2-mode
			   ;; --- Minor Mode ---
			   nodejs-repl
			   ;; exec-path-from-shell
			   popwin
			   ;; --- Themes ---
			   monokai-theme
			   ;; solarized-theme
			   ) "Default packages")
(setq package-selected-packages lisatiy/packages)
(defun lisatiy/packages-installed-p ()
  (loop for pkg in lisatiy/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))
(unless (lisatiy/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg lisatiy/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(global-hungry-delete-mode)

(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'js-mode-hook 'smartparens-mode)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; cofig js2-mode for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))
(require 'nodejs-repl)

(global-company-mode t)

(load-theme 'monokai t)

(require 'popwin)
(popwin-mode 1)

;; provide feature
(provide 'init-packages)
