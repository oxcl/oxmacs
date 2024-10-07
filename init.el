;; -*- lexical-binding: t; -*-   
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")))
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-compute-statistics t)

(use-package use-package
  :custom
  (use-package-verbose init-file-debug)
  (use-package-always-ensure t)
  (use-package-always-defer  t))

(use-package emacs
  :custom
  custom-file (expand-file-name "custome.el" user-emacs-directory)
  :config
  (load-theme 'wombat))

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  gcmh-verbose t
  gcmh-auto-idle-delay-factor 10
  gcmh-high-cons-threshold (* 100 1024 1024) ; 100MB
  :config
  (remove-hook 'after-init-hook #'ox/revert-gc))
