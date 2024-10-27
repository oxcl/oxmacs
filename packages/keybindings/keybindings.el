(use-package keybindings
  :ensure nil
  :no-require
  :demand t
  :config

(defvar ox/fast-keyseq-timeout 50) ; this timeout determines if a ESC is a Alt combination or a single <escape>
(defun ox/-tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
	   (sit-for (/ ox/fast-keyseq-timeout 1000.0)))
      [?\C-g] map)) ; i replaced [escape] with [?\C-g] because i want to swap them

(defun ox/-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun ox/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
    (let ((esc-binding (ox/-lookup-key input-decode-map ?\e)))
      (define-key input-decode-map
		  [?\e] `(menu-item "" ,esc-binding :filter ox/-tty-ESC-filter)))))  
(ox/catch-tty-ESC)

;; swap <escape> and Ctrl-g because Ctrl-g is just too good for just quit and ESC is just too good to not be used at all
;; so in key bindings <escape> will actually become C-g
;; meaning if you want to bound someting to C-g you have to use <escape>
(define-key input-decode-map [?\C-g] [escape] )
(define-key input-decode-map [escape] [?\C-g] )

)
