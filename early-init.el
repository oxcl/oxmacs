;; -*- lexical-binding: t; -*-

(setq ox/loaded t
      ;; temporarily disable garbage collection for the startup.
      ;; this must be reset after startup is over or it might cause stutters
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; Don't use precious startup time checking mtime on elisp bytecode.
      ;; Ensuring correctness is 'doom sync's job, not the interactive session's
      ;; Still, stale byte-code will cause *heavy* losses in startup efficiency
      ;; but performance is unimportant when Emacs is in an error state.
      load-prefer-newer noninteractive
      ;; `file-name-handler-alist' is consulted often. Unsetting it offers a
      ;; notable saving in startup time. This is just a stopgap though;
      file-name-handler-alist-og file-name-handler-alist
      file-name-handler-alist nil
      ;; disabling auto-save and backup files during startup to hopefully
      ;; improve startup time
      auto-save-default nil
      make-backup-files nil
      ;; package.el initialization loads every package autoloads file and
      ;; populates load-path. That makes (package-initialize) a great target
      ;; for premature optimizers like me!
      package-enable-at-startup nil ; don' auto-initialize!
      ;; this tells package.el not to add those pesky customized variable
      ;; settinsg at the end of your init.el
      package--init-file-ensured t
      ;; disable unnecessary ui elements
      default-frame-alist '((vertical-scroll-bars)
			    (tool-bar-lines . 0))
      tool-bar-mode nil
      scroll-bar-mode nil
      ;; don't show the (in)famous emacs startup buffer
      inhibit-startup-screen t
      ;; prevent littering of emacs config directory by moving it entirely to
      ;; a cache directory and explicitly configure the path for files you need
      ;; to the 'real-emacs-directory'
      real-emacs-directory user-emacs-directory
      user-emacs-directory (expand-file-name "emacs/"
					     (or (getenv "XDG_CACHE_HOME")
						 (expand-file-name ".cache" "~"))))

;; disable menu bar unless in MacOS
(unless (eq system-type 'darwin)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (setq menu-bar-mode nil))

(defun ox/revert-gc ()
  (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
	gc-cons-percentage 0.1))
(add-hook 'after-init-hook #'ox/revert-gc)

(defun ox/revert-file-handler ()
  (setq file-name-handler-alist file-name-handler-alist-og))
(add-hook 'after-init-hook #'ox/revert-file-handler)


