;; the following code snippet is used to enable the <escape> key to be recognized by terminal environments
;; taken from: https://github.com/emacsorphanage/god-mode/issues/43#issuecomment-67193877

(defvar oxcl/fast-keyseq-timeout 50) ; this timeout determines if a ESC is a Alt combination or a single <escape>
(defun oxcl/-tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
	   (sit-for (/ oxcl/fast-keyseq-timeout 1000.0)))
      [?\C-g] map)) ; i replaced [escape] with [?\C-g] because i want to swap them

(defun oxcl/-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun oxcl/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
    (let ((esc-binding (oxcl/-lookup-key input-decode-map ?\e)))
      (define-key input-decode-map
		  [?\e] `(menu-item "" ,esc-binding :filter oxcl/-tty-ESC-filter)))))  
(oxcl/catch-tty-ESC)

;; swap <escape> and Ctrl-g because Ctrl-g is just too good for just quit and ESC is just too good to not be used at all
;; so in key bindings <escape> will actually become C-g
;; meaning if you want to bound someting to C-g you have to use <escape>
(define-key input-decode-map [?\C-g] [escape] )
(define-key input-decode-map [escape] [?\C-g] )
