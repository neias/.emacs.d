;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.9)
  :config
  (which-key-mode))


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

  ;; :after eshell     ;; Make sure it gets hooked after eshell
  ;; :custom-face
  ;; (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))
  ;; :custom
  ;; (doom-modeline-height 15)
  ;; (doom-modeline-bar-width 6)
  ;; (doom-modeline-lsp t)
  ;; (doom-modeline-github nil)
  ;; (doom-modeline-mu4e nil)
  ;; (doom-modeline-irc nil)
  ;; (doom-modeline-minor-modes t)
  ;; (doom-modeline-persp-name nil)
  ;; (doom-modeline-buffer-file-name-style 'truncate-except-project)
;; (doom-modeline-major-mode-icon nil))

(use-package all-the-icons ;; M-x all-the-icons-install-fonts RET
  :ensure t)

;; Thanks, but no thanks
(setq inhibit-startup-message t)

(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)	    ; Disable the menu bar
(scroll-bar-mode -1)        ; Disable visible scrollbar
(global-hl-line-mode +1)    ; Highlight line
(delete-selection-mode 1)   ; Delete selection
;; (tooltip-mode -1)           ; Disable tooltips
(set-default-coding-systems 'utf-8)
(setq backup-directory-alist '(("." . "~/.saves")))   ; Different backup directory
(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; Display line numbers when programming
;; disable line number for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight Matching Braces
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; Set up the visible bell
(setq visible-bell t)

;; (use-package helm
;;   :ensure t
;;   :config (helm-mode 1))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

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

(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	([f8] . treemacs)
	("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-*" . er/expand-region)
  ("C--" . er/contract-region))

(defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (height (/ (window-total-height) 3))
             (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))

        (insert (concat "ls"))
        (eshell-send-input))
      :bind ("C-c C-c" . eshell-quit-process))

(global-set-key (kbd "C-x t") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-support-shift-select t)

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

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-book 'global-company-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.js\\'")

(defun setup-tide-mode()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  ;; editing
  (setq typescript-indent-level 2)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook (typescript-mode . setup-tide-mode))

(use-package prettier-js
  :ensure t
  :after (typescript-mode)
  :hook (typescript-mdoe . prettier-js-mode))

;; Better Completions with Ivy
(use-package ivy :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
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
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich :ensure t
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

(use-package counsel :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

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



;; ;; The default is 800 kilobytes.  Measured in bytes.
;; (setq gc-cons-threshold (* 50 1000 1000))

;; ;; Profile emacs startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "*** Emacs loaded in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; ;; ;; ESC Cancels All
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ;; Silence compiler warnings as they can be pretty disruptive
;; (setq comp-async-report-warnings-errors nil)

;; (load-file "~/.emacs.d/lisp/dw-settings.el")

;; ;; Load settings for the first time
;; (dw/load-system-settings)

;; (require 'subr-x)
;; (setq dw/is-termux
;;       (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

;; (setq dw/is-guix-system (and (eq system-type 'gnu/linux)
;;                              (require 'f)
;;                              (string-equal (f-read "/etc/issue")
;;                                            "\nThis is the GNU system.  Welcome.\n")))


;; ;; Fix an issue accessing the ELPA archive in Termux
;; ;; (when dw/is-termux
;; ;;   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;; ;; Uncomment this to get a reading on packages that get loaded at startup
;; (setq use-package-verbose t)

;; ;; On non-Guix systems, "ensure" packages by default
;; (setq use-package-always-ensure (not dw/is-guix-system))

;; ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

;; ;; Bootstrap straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;; Always use straight to install on systems other than Linux
;; (setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; ;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; ;; Load the helper package for commands like `straight-x-clean-unused-repos'
;; (require 'straight-x)


;; ;; Use no-littering to automatically set common paths to the new user-emacs-directory
;; (use-package no-littering)

;; ;; Keep customization settings in a temporary file (thanks Ambrevar!)
;; (setq custom-file
;;       (if (boundp 'server-socket-dir)
;;           (expand-file-name "custom.el" server-socket-dir)
;;         (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; (load custom-file t)


;; (use-package use-package-chords
;;   :disabled
;;   :config (key-chord-mode 1))


;; (unless dw/is-termux
;;   (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;   (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;   (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;   (setq scroll-step 1) ;; keyboard scroll one line at a time
;;   (setq use-dialog-box nil)) ;; Disable dialog boxes since they weren't working in Mac OSX


;; (unless dw/is-termux
;;   (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;   (add-to-list 'default-frame-alist '(fullscreen . maximized)))


;; ;; Don’t warn for large files (shows up when launching videos)
;; (setq large-file-warning-threshold nil)

;; ;; Don’t warn for following symlinked files
;; (setq vc-follow-symlinks t)

;; ;; Don’t warn when advice is added for functions
;; (setq ad-redefinition-action 'accept)


;; (doom-themes-visual-bell-config)

;; (defun dw/replace-unicode-font-mapping (block-name old-font new-font)
;;   (let* ((block-idx (cl-position-if
;;                      (lambda (i) (string-equal (car i) block-name))
;;                      unicode-fonts-block-font-mapping))
;;          (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
;;          (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
;;     (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
;;           `(,updated-block))))

;; (use-package unicode-fonts
;;   :disabled
;;   :if (not dw/is-termux)
;;   :custom
;;   (unicode-fonts-skip-font-groups '(low-quality-glyphs))
;;   :config
;;   ;; Fix the font mappings to use the right emoji font
;;   (mapcar
;;    (lambda (block-name)
;;      (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
;;    '("Dingbats"
;;      "Emoticons"
;;      "Miscellaneous Symbols and Pictographs"
;;      "Transport and Map Symbols"))
;;   (unicode-fonts-setup))

;; ;; Emojis in buffers
;; (use-package emojify
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)


;; (use-package diminish)

;; ;; Smart Mode Line
;; (use-package smart-mode-line
;;   :disabled
;;   :if dw/is-termux
;;   :config
;;   (setq sml/no-confirm-load-theme t)
;;   (sml/setup)
;;   (sml/apply-theme 'respectful)  ; Respect the theme colors
;;   (setq sml/mode-width 'right
;;         sml/name-width 60)

;;   (setq-default mode-line-format
;;                 `("%e"
;;                   ,(when dw/exwm-enabled
;;                      '(:eval (format "[%d] " exwm-workspace-current-index)))
;;                   mode-line-front-space
;;                   evil-mode-line-tag
;;                   mode-line-mule-info
;;                   mode-line-client
;;                   mode-line-modified
;;                   mode-line-remote
;;                   mode-line-frame-identification
;;                   mode-line-buffer-identification
;;                   sml/pos-id-separator
;;                   (vc-mode vc-mode)
;;                   " "
;;                                         ;mode-line-position
;;                   sml/pre-modes-separator
;;                   mode-line-modes
;;                   " "
;;                   mode-line-misc-info))

;;   (setq rm-excluded-modes
;;         (mapconcat
;;          'identity
;;                                         ; These names must start with a space!
;;          '(" GitGutter" " MRev" " company"
;;            " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
;;            " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
;;          "\\|")))


;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))


;; ;; Notification
;; (use-package alert
;;   :commands alert
;;   :config
;;   (setq alert-default-style 'notifications))


;; ;; Auto-Saving Changed Files
;; (use-package super-save
;;   :defer 1
;;   :diminish super-save-mode
;;   :config
;;   (super-save-mode +1)
;;   (setq super-save-auto-save-when-idle t))


;; ;; Revert Dired and other buffers
;; (setq global-auto-revert-non-file-buffers t)
;; ;; Revert buffers when the underlying file has changed
;; (global-auto-revert-mode 1)



;; ;; Set default connection mode to SSH
;; (setq tramp-default-method "ssh")

;; ;; editing
;; (setq-default tab-width 2)
;; (setq-default indent-tabs-mode nil)


;; ;; Automatically clean whitespace
;; (use-package ws-butler
;;   :hook ((text-mode . ws-butler-mode)
;;          (prog-mode . ws-butler-mode)))

;; ;; Origami.el for Folding
;; (use-package origami
;;   :hook (yaml-mode . origami-mode))

;; ;; (use-package command-log-mode)

;; ;; Stateful Keymaps with Hydra
;; (use-package hydra
;;   :defer 1)










;; (use-package wgrep)

;; (use-package ivy-posframe
;;   :disabled
;;   :custom
;;   (ivy-posframe-width      115)
;;   (ivy-posframe-min-width  115)
;;   (ivy-posframe-height     10)
;;   (ivy-posframe-min-height 10)
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (setq ivy-posframe-parameters '((parent-frame . nil)
;;                                   (left-fringe . 8)
;;                                   (right-fringe . 8)))
;;   (ivy-posframe-mode 1))




;; ;; Completion Annotations with Marginalia
;; (use-package marginalia
;;   :disabled
;;   :custom
;;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
;;   :init
;;   ;; Ensure that Selectrum is refreshed when cycling annotations.
;;   (marginalia-mode)
;;   (advice-add #'marginalia-cycle :after (lambda () (selectrum-exhibit))))

;; ;; Completion Actions with Embark
;; (use-package embark
;;   :disabled
;;   :bind (("C-S-a" . embark-act)
;;          :map minibuffer-local-map
;;          ("C-d" . embark-act))
;;   :config
;;   ;; ---- Selectrum only ----
;;   (defun current-candidate+category ()
;;     (when selectrum-active-p
;;       (cons (selectrum--get-meta 'category)
;;             (selectrum-get-current-candidate))))

;;   ;; (add-hook 'embark-target-finders #'current-candidate+category)

;;   (defun current-candidates+category ()
;;     (when selectrum-active-p
;;       (cons (selectrum--get-meta 'category)
;;             (selectrum-get-current-candidates
;;              ;; Pass relative file names for dired.
;;              minibuffer-completing-file-name))))

;;   ;; No unnecessary computation delay after injection.
;;   ;; (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

;;   ;; (add-hook 'embark-candidate-collectors #'current-candidates+category))

;;   ;; Show Embark actions via which-key
;;   (setq embark-action-indicator
;;         (lambda (map)
;;           (which-key--show-keymap "Embark" map nil nil 'no-paging)
;;           #'which-key--hide-popup-ignore-command)
;;         embark-become-indicator embark-action-indicator)

;;   ;; Use this for icomplete
;;   (add-hook 'embark-pre-action-hook #'completion--flush-all-sorted-completions))

;; ;; Launching apps
;; ;; (use-package app-launcher)
;; ;; :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

;; ;; Selectrum
;; (use-package selectrum
;;   :disabled
;;   :bind (("C-M-r" . selectrum-repeat)
;;          :map selectrum-minibuffer-map
;;          ("C-r" . selectrum-select-from-history)
;;          ("C-j" . selectrum-next-candidate)
;;          ("C-k" . selectrum-previous-candidate)
;;          :map minibuffer-local-map
;;          ("M-h" . backward-kill-word))
;;   :custom
;;   (selectrum-fix-minibuffer-height t)
;;   (selectrum-num-candidates-displayed 7)
;;   (selectrum-refine-candidates-function #'orderless-filter)
;;   (selectrum-highlight-candidates-function #'orderless-highlight-matches)
;;   :custom-face
;;   (selectrum-current-candidate ((t (:background "#3a3f5a"))))
;;   :init
;;   (selectrum-mode 1))

;; ;; Buffer Management with Bufler
;; (use-package bufler
;;   :disabled
;;   :straight t
;;   :bind (("C-M-j" . bufler-switch-buffer)
;;          ("C-M-k" . bufler-workspace-frame-set))
;;   :config
;;   (evil-collection-define-key 'normal 'bufler-list-mode-map
;;                               (kbd "RET")   'bufler-list-buffer-switch
;;                               (kbd "M-RET") 'bufler-list-buffer-peek
;;                               "D"           'bufler-list-buffer-kill)

;;   (setf bufler-groups
;;         (bufler-defgroups
;;          ;; Subgroup collecting all named workspaces.
;;          (group (auto-workspace))
;;          ;; Subgroup collecting buffers in a projectile project.
;;          (group (auto-projectile))
;;          ;; Grouping browser windows
;;          (group
;;           (group-or "Browsers"
;;                     (name-match "Vimb" (rx bos "vimb"))
;;                     (name-match "Qutebrowser" (rx bos "Qutebrowser"))
;;                     (name-match "Chromium" (rx bos "Chromium"))))
;;          (group
;;           (group-or "Chat"
;;                     (mode-match "Telega" (rx bos "telega-"))))
;;          (group
;;           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
;;           (group-or "Help/Info"
;;                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
;;                     ;; (mode-match "*Helpful*" (rx bos "helpful-"))
;;                     (mode-match "*Info*" (rx bos "info-"))))
;;          (group
;;           ;; Subgroup collecting all special buffers (i.e. ones that are not
;;           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
;;           ;; through to other groups, so they end up grouped with their project buffers).
;;           (group-and "*Special*"
;;                      (name-match "**Special**"
;;                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
;;                      (lambda (buffer)
;;                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
;;                                             buffer)
;;                                    (funcall (mode-match "Dired" (rx bos "dired"))
;;                                             buffer)
;;                                    (funcall (auto-file) buffer))
;;                          "*Special*"))))
;;          ;; Group remaining buffers by major mode.
;;          (auto-mode))))

;; ;; Frame Scaling / Zooming
;; (use-package default-text-scale
;;   :defer 1
;;   :config
;;   (default-text-scale-mode))

;; ;; Window Selection with ace-window
;; (use-package ace-window
;;   :bind (("M-o" . ace-window))
;;   :custom
;;   (aw-scope 'frame)
;;   (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;   (aw-minibuffer-flag t)
;;   :config
;;   (ace-window-display-mode 1))

;; ;; Window History with winner-mode
;; ;; (use-package winner
;; ;;   :after evil
;; ;;   :config
;; ;;   (winner-mode)
;; ;;   (define-key evil-window-map "u" 'winner-undo)
;; ;;   (define-key evil-window-map "U" 'winner-redo))


;; (defun dw/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 110
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :defer t
;;   :hook (org-mode . dw/org-mode-visual-fill))


;; ;; Control Buffer Placement
;; (setq display-buffer-base-action
;;       '(display-buffer-reuse-mode-window
;;         display-buffer-reuse-window
;;         display-buffer-same-window))

;; ;; If a popup does happen, don't resize windows to be equal-sized
;; (setq even-window-sizes nil)

;; ;; Expand Region
;; (use-package expand-region
;;   :if (not dw/is-termux)
;;   :bind (("M-[" . er/expand-region)
;;          ("C-(" . er/mark-outside-pairs)))

;; ;; File Browsing

;; (use-package dired
;;   :ensure nil
;;   :straight nil
;;   :defer 1
;;   :commands (dired dired-jump)
;;   :config
;;   (setq dired-listing-switches "-agho --group-directories-first"
;;         dired-omit-files "^\\.[^.].*"
;;         dired-omit-verbose nil
;;         dired-hide-details-hide-symlink-targets nil)

;;   (autoload 'dired-omit-mode "dired-x")

;;   (add-hook 'dired-load-hook
;;             (lambda ()
;;               (interactive)
;;               (dired-collapse)))

;;   (add-hook 'dired-mode-hook
;;             (lambda ()
;;               (interactive)
;;               (dired-omit-mode 1)
;;               (dired-hide-details-mode 1)
;;               (unless (or dw/is-termux
;;                           (s-equals? "/gnu/store/" (expand-file-name default-directory)))
;;                 (all-the-icons-dired-mode 1))
;;               (hl-line-mode 1)))

;;   (use-package dired-rainbow
;;     :defer 2
;;     :config
;;     (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;;     (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;;     (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;;     (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;;     (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;;     (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;;     (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;;     (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;;     (dired-rainbow-define log "#c17d11" ("log"))
;;     (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;;     (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;;     (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;;     (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;     (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;     (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;;     (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;;     (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;;     (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;;     (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;;     (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

;;   (use-package dired-single
;;     :defer t)

;;   (use-package dired-ranger
;;     :defer t)

;;   (use-package dired-collapse
;;     :defer t))

;; ;; Opening Files Externally
;; (use-package openwith
;;   :if (not dw/is-termux)
;;   :config
;;   (setq openwith-associations
;;         (list
;;          (list (openwith-make-extension-regexp
;;                 '("mpg" "mpeg" "mp3" "mp4"
;;                   "avi" "wmv" "wav" "mov" "flv"
;;                   "ogm" "ogg" "mkv"))
;;                "mpv"
;;                '(file))
;;          (list (openwith-make-extension-regexp
;;                 '("xbm" "pbm" "pgm" "ppm" "pnm"
;;                   "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
;;                ;; causing feh to be opened...
;;                "feh"
;;                '(file))
;;          (list (openwith-make-extension-regexp
;;                 '("pdf"))
;;                "zathura"
;;                '(file)))))

;; ;; Projectile
;; (defun dw/switch-project-action ()
;;   "Switch to a workspace with the project name and start `magit-status'."
;;   ;; TODO: Switch to EXWM workspace 1?
;;   (persp-switch (projectile-project-name))
;;   (magit-status))

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :demand t
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/Projects/Code")
;;     (setq projectile-project-search-path '("~/Projects/Code")))
;;   (setq projectile-switch-project-action #'dw/switch-project-action))

;; (use-package counsel-projectile
;;   :after projectile
;;   :bind (("C-M-p" . counsel-projectile-find-file))
;;   :config
;;   (counsel-projectile-mode))


;; (use-package prettier-js
;;   :ensure t
;;   :config
;;   (setq prettier-js-show-errors nil))

;; (use-package company
;;   :ensure t
;;   :init
;;   (add-hook 'after-init-hook 'global-company-mode))

;; (add-to-list 'exec-path "~/Users/emrea/AppData/Roaming/npm")

;; ;; js and typescript
;; (use-package rjsx-mode
;;   :unsure t
;;   :mode "\\.js\\'")

;; (defun setup-tide-mode()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (setq-default typescript-indent-level 2)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; (use-package tide
;;   :ensure t
;;   :after (rjsx-mode company flycheck)
;;   :hook (rjsx-mode . setup-tide-mode))

;; (use-package prettier-js
;;   :ensure t
;;   :after (rjsx-mode)
;;   :hook (rjsx-mode . prettier-js-mode))

;; ;;   (eldoc-mode +1)

;; ;;   (prettier-js-mode t)
;; ;;   ;; company is an optional dependency. You have to
;; ;;   ;; install it separately via package-install
;; ;;   ;; `M-x package-install [ret] company`


;; ;; ;; aligns annotation to the right hand side
;; ;; (setq company-tooltip-align-annotations t)

;; ;; ;; formats the buffer before saving
;; ;; (add-hook 'before-save-hook 'tide-format-before-save)

;; ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)




;; (use-package apheleia
;;   :config
;;   (apheleia-global-mode +1))


;; ;; Scheme
;; ;; TODO: This causes issues for some reason.
;; ;; :bind (:map geiser-mode-map
;; ;;        ("TAB" . completion-at-point))

;; (use-package geiser
;;   :straight t
;;   :config
;;   ;; (setq geiser-default-implementation 'gambit)
;;   (setq geiser-default-implementation 'guile)
;;   (setq geiser-active-implementations '(gambit guile))
;;   (setq geiser-repl-default-port 44555) ; For Gambit Scheme
;;   (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
;;                                        ((regexp "\\.sld") gambit))))
;; ;; Markdown
;; (use-package markdown-mode
;;   :straight t
;;   :mode "\\.md\\'"
;;   :config
;;   (setq markdown-command "marked")
;;   (defun dw/set-markdown-header-font-sizes ()
;;     (dolist (face '((markdown-header-face-1 . 1.2)
;;                     (markdown-header-face-2 . 1.1)
;;                     (markdown-header-face-3 . 1.0)
;;                     (markdown-header-face-4 . 1.0)
;;                     (markdown-header-face-5 . 1.0)))
;;       (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

;;   (defun dw/markdown-mode-hook ()
;;     (dw/set-markdown-header-font-sizes))

;;   (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

;; ;; HTML
;; ;; (use-package web-mode
;; ;;   :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
;; ;;   :config
;; ;;   (setq-default web-mode-code-indent-offset 2)
;; ;;   (setq-default web-mode-markup-indent-offset 2)
;; ;;   (setq-default web-mode-attribute-indent-offset 2))

;; ;; 1. Start the server with `httpd-start'
;; ;; 2. Use `impatient-mode' on any buffer
;; (use-package impatient-mode
;;   :straight t)

;; (use-package skewer-mode
;;   :straight t)

;; ;; YAML
;; (use-package yaml-mode
;;   :mode "\\.ya?ml\\'")

;; ;; Syntax checking with Flycheck
;; (use-package flycheck
;;   :defer t
;;   :hook (lsp-mode . flycheck-mode))



;; (use-package smartparens
;;   :hook (prog-mode . smartparens-mode))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package rainbow-mode
;;   :defer t
;;   :hook (org-mode
;;          emacs-lisp-mode
;;          web-mode
;;          ;; js2-mode
;;          typescript-mode))

;; ;; TODO: Figure out how to query for 'done' bugs
;; (defun dw/debbugs-guix-patches ()
;;   (interactive)
;;   (debbugs-gnu '("serious" "important" "normal") "guix-patches" nil t))

;; (use-package know-your-http-well
;;   :defer t)

;; (use-package darkroom
;;   :commands darkroom-mode
;;   :config
;;   (setq darkroom-text-scale-increase 0))

;; (defun dw/enter-focus-mode ()
;;   (interactive)
;;   (darkroom-mode 1)
;;   (display-line-numbers-mode 0))

;; (defun dw/leave-focus-mode ()
;;   (interactive)
;;   (darkroom-mode 0)
;;   (display-line-numbers-mode 1))

;; (defun dw/toggle-focus-mode ()
;;   (interactive)
;;   (if (symbol-value darkroom-mode)
;;       (dw/leave-focus-mode)
;;     (dw/enter-focus-mode)))

;; (use-package vterm
;;   :commands vterm
;;   :config
;;   (setq vterm-max-scrollback 10000))

;; (use-package multi-term
;;   :commands multi-term-next
;;   :config
;;   (setq term-buffer-maximum-size 10000)
;;   (setq term-scroll-to-bottom-on-output t)
;;   (add-hook 'term-mode-hook
;;             (lambda ()
;;               (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
;;               (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next)))))


;; ;; Don't let ediff break EXWM, keep it in one frame
;; (setq ediff-diff-options "-w"
;;       ediff-split-window-function 'split-window-horizontally
;;       ediff-window-setup-function 'ediff-setup-windows-plain)

;; (use-package docker
;;   :commands docker)

;; (use-package docker-tramp
;;   :defer t
;;   :after docker)

;; ;; (server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet ivy-prescient prescient ivy-rich counsel ivy which-key use-package treemacs-projectile tide prettier-js org-bullets helm-projectile expand-region doom-themes doom-modeline dashboard company centaur-tabs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
