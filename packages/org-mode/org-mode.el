(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :commands (org-mode)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode))
