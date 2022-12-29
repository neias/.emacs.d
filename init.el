;;; package --- Summary:

;;; Commentary:
;;; init.el --- neias Emacs Config

;;; Author: Emre Acar <emreacar@hotmail.com>
;;; Version: 0.0.3
;;; Keywords: Emacs, configuration
;;; URL: https://github.com/neias/.emacs.d

;;; Code:
(set-language-environment "English")
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

(setq inhibit-startup-message t) ;; Disable startup message
(tool-bar-mode -1)               ;; Disable the toolbar
(menu-bar-mode -1)               ;; Disable the menu bar
(scroll-bar-mode -1)             ;; Disable visible scrollbar
(global-hl-line-mode +1)         ;; Highlight line
(delete-selection-mode 1)        ;; Then inserting text while the mark is active causes the selected text to be deleted first.
(tooltip-mode -1)                ;; Disable tooltips
(set-default-coding-systems 'utf-8)
(setq backup-directory-alist '(("." . "~/.saves")))   ; Different backup director
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; Display line numbers when programming

;; for mac os
(if (eq system-type 'darwin)
    (setq mac-command-modifier      'meta
          mac-option-modifier       'alt
          mac-right-option-modifier 'super
					ns-alternate-modifier 'meta
					ns-right-alternate-modifier 'none))

;; starting resize emacs
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
				(if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 130))
          (add-to-list 'default-frame-alist (cons 'width 80)))
				(add-to-list 'default-frame-alist 
										 (cons 'height (/ (- (x-display-pixel-height) 200)
																			(frame-char-height)))))))
(set-frame-size-according-to-resolution)

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 )
  )

;; Better Completions with Ivy
(use-package ivy
  :ensure t
  :diminish
  :init
  (ivy-mode 1)
  )

(use-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ;; ("C-x b" . counsel-ibuffer)
	 ;; ("C-x C-f" . counsel-find-file)
	 ("C-M-j" . counsel-switch-buffer)
	 ("C-M-l" . counsel-imenu)
	 )
  )

;;; Sorting and filtering
(use-package prescient :ensure t
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient :ensure t
  :after prescient
  :config
  (ivy-prescient-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  :after counsel
  )

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))


;; Highlight Matching Braces
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package expand-region
  :ensure t
  :bind
  ("C-*" . er/expand-region)
  ("C--" . er/contract-region))

;; Window tabs
(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-set-bar 'over
	centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-height 24
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "o")
  (centaur-tabs-mode t))



;; Window Selection with ace-window
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package gruvbox-theme
  :ensure t)

;; dark/light theme selecting
(defun dark-mode ()
  "Activate dark mode color theme"
  (interactive)
  (load-theme 'gruvbox-dark-medium t))
(global-set-key (kbd "C-c d") 'dark-mode)
(defun light-mode ()
  "Activate light mode color theme."
  (interactive)
  (load-theme 'gruvbox-light-hard t))
(global-set-key (kbd "C-c l") 'light-mode)

(use-package all-the-icons ;; M-x all-the-icons-install-fonts RET
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

;; Selected font 
(defun set-font (font-name)
  "Set font for current frame."
  (interactive "sFont name: ")
  (set-frame-font font-name))

(cond
 ((string-equal system-type "darwin") ; macOS
  (set-font "Monaco-14"))
 ((string-equal system-type "windows-nt") ; Windows
  (set-font "Consolas-12"))
 ((string-equal system-type "gnu/linux") ; Linux
  (set-font "DejaVu Sans Mono-12")))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

;; Emacs dashboard configure
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 3)
			    (projects . 3)
			    (bookmarks . 3)))
    ;; (setq dashboard-show-shortcuts nil)
    (setq dashboard-center-content nil)
    (setq dashboard-banner-logo-title "neias")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-startup-banner "~/.emacs.d/images/emacs-logo.png")
    (setq dashboard-set-navigator t)
    (setq dashboard-navigator-buttons
	  `(;; linel
	    ((, nil
		"init file"
		"Open init file"
		(lambda (&rest _) (find-file "~/.emacs.d/init.el"))
		)))))
  :config
  (dashboard-setup-startup-hook))


;;; Autocomplate
(use-package company
  :ensure t)

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

;;; Tide mode and company mode configuration for TypeScript, JavaScript, and React
(use-package rjsx-mode
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :hook (rjsx-mode . setup-tide-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . setup-tide-mode))

(use-package js2-mode
  :hook (js2-mode . setup-tide-mode))

(use-package tide
	:ensure t
  :hook ((rjsx-mode . setup-tide-mode)
         (typescript-mode . setup-tide-mode)
         (js2-mode . setup-tide-mode)))

(defun setup-tide-mode()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
	(setq js-indent-level 2)
  (company-mode +1))

(add-hook 'tide-mode-hook #'company-mode)






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tide typescript-mode rjsx-mode flycheck company dashboard projectile doom-modeline all-the-icons gruvbox-theme ace-window centaur-tabs expand-region flx ivy-rich ivy-prescient prescient counsel swiper which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
