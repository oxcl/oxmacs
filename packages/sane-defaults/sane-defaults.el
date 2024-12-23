(use-package sane-defaults
:ensure nil
:demand t
:no-require
:config

(setq initial-sctratch-message "" ; Make *sctratch* buffer empty
      ring-bell-function #'ignore ; disable visual and audible bell
      vc-follow-symlinks       t) ; follow a symlink without asking. wtf?

;; why should i type the whole world "yes" instead of just "y" richard stallman? why??
(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode 1)   ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t) ; Auto-update buffer if file has changed on disk

;; teach emacs to keep your folders clean
(setq create-lockfiles nil
      backup-directory-alist `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-file-name-transforms `((".*"  ,(expand-file-name "auto-saves/" user-emacs-directory) t))
      auto-save-default t
      auto-save-no-message t)
(make-directory auto-save-list-file-prefix t)

;; save the cursor position for files across emacs sessions
(save-place-mode)

;; save mini buffer history across emacs sessions
(savehist-mode)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)
;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5


;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: there's a timeout that adds latency to frame operations,
;; like `make-frame-invisible', which Emacs frequently calls without a guard
;; because it's inexpensive in non-PGTK builds. Lowering the timeout from the
;; default 0.1 should make childframes and packages that manipulate them (like
;; `lsp-ui', `company-box', and `posframe') feel much snappier. See
;; emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes redisplay faster, but might produce incorrect
;; reordering of bidirectional text with embedded parentheses (and other
;; bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
	w32-pipe-read-delay 0               ; faster IPC
	w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; no blinking cursor please
(blink-cursor-mode 0)

) ; use-package end
