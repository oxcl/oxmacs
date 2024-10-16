(setq initial-sctratch-message "" ; Make *sctratch* buffer empty
      ring-bell-function #'ignore ; disable visual and audible bell
      vc-follow-symlinks       t) ; follow a symlink without asking wtf?

;; why should i type the whole world "yes" instead of just "y" richard stallman? why??
(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)   ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t) ; Auto-update buffer if file has changed on disk

;; teach emacs to keep your folders clean
(setq create-lockfiles nil
      backup-directory-alist `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-default t
      auto-save-list-file-prefix (expand-file-name "auto-saves/" user-emacs-directory))
(make-directory auto-save-list-file-prefix t)

;; save the cursor position for files across emacs sessions
(save-place-mode)

;; save mini buffer history across emacs sessions
(savehist-mode)
