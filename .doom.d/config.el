;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Treemacs
(global-set-key (kbd "s-SPC") 'treemacs)

(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )

(defun my-office-code-style ()
  (interactive)
  (message "Office code style!")
  ;; use tab instead of space
  (setq-local indent-tabs-mode t)
  ;; indent 4 spaces width
  (my-setup-indent 2))

(defun my-personal-code-style ()
  (interactive)
  (message "My personal code style!")
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (my-setup-indent 4))

(defun my-setup-develop-environment ()
  (interactive)
  (let ((proj-dir (file-name-directory (buffer-file-name))))
    ;; if hobby project path contains string "hobby-proj1"
    (if (string-match-p "hobby-proj1" proj-dir)
        (my-personal-code-style))

    ;; if commericial project path contains string "commerical-proj"
    (if (string-match-p "commerical-proj" proj-dir)
        (my-office-code-style))))

(defun default-environment ()
  (interactive)
  (my-personal-code-style)

  )

;; prog-mode-hook requires emacs24+
(add-hook 'prog-mode-hook 'default-environment)
;; a few major-modes does NOT inherited from prog-mode
(add-hook 'lua-mode-hook 'default-environment)
(add-hook 'web-mode-hook 'default-environment)



( use-package evil-window
  :bind
  (
   ("C-j" . evil-window-down)
   ("C-k" . evil-window-up)
   ("C-h" . evil-window-left)
   ("C-l" . evil-window-right)
  )
)
