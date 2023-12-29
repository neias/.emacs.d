;;; init-typescript.el --- Support for typescript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'typescript-mode)
(maybe-require-package 'tide)
(maybe-require-package 'company)
(maybe-require-package 'flycheck)
(maybe-require-package 'prettier-js)
(maybe-require-package 'react-snippets)

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; TypeScript setup.
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(setq typescript-indent-level 2)

(defun check-directory-and-create ()
  "Check if the directory of the current file exists, if not, create it."
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (if (not (file-exists-p dir))
          (if (y-or-n-p (format "Directory %s does not exist. Create it?" dir))
              (make-directory dir t))))))

;; Tide setup.
(defun setup-tide-mode ()
  (interactive)
  (check-directory-and-create)
  (tide-setup)
  (tide-hl-identifier-mode t))

(add-hook 'typescript-mode-hook 'setup-tide-mode)

;; Company setup.
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck setup.
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Prettier setup.
(require 'prettier-js)
(add-hook 'typescript-mode-hook 'prettier-js-mode 'tslint-mode)

(cond
 ((string-equal system-type "windows-nt") ; for Windows
  (let ((my-paths '("C:/Program Files/Git/usr/bin")))
    (setenv "PATH" (concat (getenv "PATH") ";" (mapconcat 'identity my-paths ";")))
    (setq exec-path (append exec-path my-paths))))

 ((string-equal system-type "darwin") ; 
  ;; for MacOS
  ))

(provide 'init-typescript)
;;; init-javascript.el ends here
