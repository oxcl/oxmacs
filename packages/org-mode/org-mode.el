(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :commands (org-mode)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)

  (defun ox/async-tangle-and-compile ()
    (message "Compiling...")
    (make-process
     :name "async-tangle-and-compile"
     :buffer nil
     :noquery t
     :sentinel (lambda (process event)
		 (when (equal event "finished\n")
		   (load (concat (file-name-sans-extension (buffer-file-name)) ".el"))))
     :command `("emacs" "-Q"
		"--batch"
		"--file" ,(buffer-file-name)
		"--eval" "(dolist (file (org-babel-tangle)) (byte-compile-file file))")))
  (defun ox/add-tangle-and-compile-to-config-buffers ()
    (let ((config-dir (if (boundp 'real-emacs-directory) real-emacs-directory user-emacs-directory)))
      (when (string-prefix-p (expand-file-name config-dir) (buffer-file-name))
			     (add-hook 'after-save-hook #'ox/async-tangle-and-compile t t))))
  (add-hook 'org-mode-hook #'ox/add-tangle-and-compile-to-config-buffers))
