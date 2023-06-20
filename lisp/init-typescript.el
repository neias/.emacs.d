;;; init-typescript.el --- Support for typescript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'typescript-mode)
(maybe-require-package 'tide)
(maybe-require-package 'company)
(maybe-require-package 'flycheck)
(maybe-require-package 'prettier-js)
(maybe-require-package 'react-snippets)

;; TypeScript setup.
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(setq typescript-indent-level 2)


;; Tide setup.
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode t))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Company setup.
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck setup.
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Prettier setup.
(require 'prettier-js)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)


(provide 'init-typescript)
;;; init-javascript.el ends here
