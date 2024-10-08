;; -*- lexical-binding: t; -*-
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")))
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq ox/packages-directory (expand-file-name "packages/" real-emacs-directory)
      ox/features-directory (expand-file-name "features/" real-emacs-directory)
      ox/profiles-directory (expand-file-name "profiles/" real-emacs-directory))

(defun ox/load (directory file-name)
  (load (expand-file-name (format "%s.el" file-name) directory)))

(defun ox/load-package (package)
  (ox/load ox/packages-directory package))

(defun ox/load-feature (feature)
  (ox/load ox/features-directory feature))

(defun ox/load-profile (profile)
  (ox/load ox/profiles-directory profiles))

(ox/load-package "sane-defaults")
(ox/load-package "use-package")
(ox/load-package "gcmh")
(ox/load-package "adaptive-wrap")

(use-package emacs
  :custom
  custom-file (expand-file-name "custome.el" user-emacs-directory)
  :config
  (load-theme 'wombat)
  (defun ox/set-default-font (&rest fonts)
    (unless (null fonts)
      (if (find-font (font-spec :name (car fonts)))
	  (set-face-attribute 'default nil
			      :font (car fonts)
			      :height 90
			      :weight 'light)
	(apply #'ox/set-default-font (cdr fonts)))))
  (ox/set-default-font "ioZevka Code"
		       "ioZevka Mono"
		       "JetBrains Mono"
		       "monospace")
  (defun ox/set-font (script &rest fonts)
    (set-fontset-font t script (car fonts))
    (dolist (font (cdr fonts))
      (set-fontset-font t script font nil 'append))
    (set-fontset-font t script (font-spec :script script) nil 'append))
  (ox/set-font 'arabic
	       "Vazir Code Extra Height WOL"
	       "Vazir Code WOL"
	       "Vazir Code Extra Height"
	       "Vazir Code"
	       "Noto Sans Arabic"
	       "Noto Naskh Arabic"))
