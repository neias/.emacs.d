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
(set-default-coding-systems 'utf-8)

(setq inhibit-startup-message t) ;; Disable startup message
(tool-bar-mode -1)               ;; Disable the toolbar
(menu-bar-mode -1)               ;; Disable the menu bar
(scroll-bar-mode +1)             ;; Disable visible scrollbar
(global-hl-line-mode +1)         ;; Highlight line
(delete-selection-mode 1)        ;; Then inserting text while the mark is active causes the selected text to be deleted first.
(tooltip-mode -1)                ;; Disable tooltips

(setq backup-directory-alist '(("." . "~/.saves")))   ; Different backup director
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; Display line numbers when programming

;; for mac os
(if (eq system-type 'darwin)
    (setq mac-command-modifier      'meta
          mac-option-modifier       'alt
          mac-right-option-modifier 'super
	  ns-alternate-modifier 'meta
	  ns-right-alternate-modifier 'none))

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

;; Instant help about keyboard shortcuts.
(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

;; Emacs package for fast searching within files/buffers.
(use-package swiper
  :ensure t
  :after ivy)

;; It is a package that allows searching options and making quick selections.
(use-package ivy
  :ensure t
  :diminish
  :bind
  ("C-s" . swiper)
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)
  )

(use-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ;; ("C-x C-f" . counsel-find-file)
	 ("C-M-j" . counsel-switch-buffer)
	 ("C-M-l" . counsel-imenu))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  ;; :config
  ;; (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
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
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
	(plist-put ivy-rich-display-transformers-list
		   'ivy-switch-buffer
		   '(:columns
		     ((ivy-rich-candidate (:width 40))
		      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
		      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
		      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
		      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
		     :predicate
		     (lambda (cand)
		       (if-let ((buffer (get-buffer cand)))
			   ;; Don't mess with EXWM buffers
			   (with-current-buffer buffer
			     (not (derived-mode-p 'exwm-mode)))))))))

;; It provides smart sorting and filtering of frequently used items.
(use-package prescient :ensure t
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient :ensure t
  :after prescient
  :config
  (ivy-prescient-mode 1))

;; Snippets
 (use-package yasnippet
   :ensure t
   :init
   (yas-global-mode 1)
   :config
   (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

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

(use-package beacon
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#d65d0e")
  (beacon-mode t))

;; Window Selection with ace-window
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

;; installed theme
(add-to-list 'load-path "~/.emacs.d/themes/ef-themes")
(require 'ef-themes)
(load-theme 'ef-light t)

;; dark/light theme selecting
(defun dark-mode ()
  "Activate dark mode color theme"
  (interactive)
  (load-theme 'ef-autumn t))
(global-set-key (kbd "C-c d") 'dark-mode)
(defun light-mode ()
  "Activate light mode color theme."
  (interactive)
  (load-theme 'ef-light t))
(global-set-key (kbd "C-c l") 'light-mode)


(use-package all-the-icons ;; M-x all-the-icons-install-fonts RET
  :if (display-graphic-p))

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
 (set-font "Fira Code Retina-12"))
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


;; Org mode
(use-package org
  :config
  (setq org-ellipsis " ▼")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)"))))
	
  (setq org-hide-emphasis-markers t)

  (setq org-agenda-files
	'("~/OrgFiles/Tasks.org"))
  )

;; todo list TODO, DONE, DOING
(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "DOING")))))
(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "DOING")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "DOING")))
                (org-todo-if-needed "DOING"))))))))
(add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)
;; end TODO


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org-mode Fontification
(set-face-attribute 'org-document-title nil :height 150)
(set-face-attribute 'org-level-1 nil :weight 'bold)
(set-face-attribute 'org-level-2 nil :weight 'bold)

(defun ne/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ne/org-mode-visual-fill))




;;; Mevzu
;;; Autocomplate
(use-package company
  :ensure t
  :config (global-company-mode t))
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package json-mode
  :ensure t)

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)


(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (when (or (string-equal "jsx" (file-name-extension buffer-file-name))
                                       (string-equal "tsx" (file-name-extension buffer-file-name)))
                               (prettier-js-mode))))


(use-package tide
  :ensure t
  :after (company flycheck)
  :hook (
	 (web-mode . setup-tide-mode)))


(defun setup-tide-mode()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq company-tooltip-align-annotations t))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))


(add-to-list 'exec-path "C:/tools/emacs/bin")

;;; End Mevzu



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tide prettier-js web-mode json-mode flycheck company visual-fill-column org-bullets dashboard projectile doom-modeline all-the-icons ace-window beacon expand-region flx yasnippet ivy-rich ivy-prescient prescient counsel swiper which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
