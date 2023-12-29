;;; init-kotlin.el --- Kotlin support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :config
  (setq kotlin-tab-width 4))

(use-package lsp-mode
  :bind
  ("M-RET" . lsp-execute-code-action))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-position 'at-point))

;; Additional helpers using treemacs
;; (symbols view, errors, dependencies for Java etc.)
(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

;; debugger component (for the few times I need it)
(use-package dap-mode
  :after lsp-mode
  :init
  (dap-auto-configure-mode))


(provide 'init-kotlin)
