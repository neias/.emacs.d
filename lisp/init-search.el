;;; init-search.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'swiper)

;; Ivy settings
(require-package 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-wrap t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)


;; (require-package 'prescient)
;; (prescient-persist-mode 1)

;; Ivy-prescient settings
(require-package 'ivy-prescient)
(ivy-prescient-mode 1)

;; Key bindings
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(global-set-key (kbd "C-M-l") 'counsel-imenu)


;; Regex strategies
(setq ivy-re-builders-alist
      '((completion-at-point . ivy--regex-fuzzy)
        (swiper . ivy--regex-ignore-order)
        (counsel-M-x . ivy--regex-ignore-order)))

;; Minibuffer height
(setq ivy-height-alist '((counsel-projectile-ag . 15)
                         (counsel-projectile-rg . 15)
                         (swiper . 15)
                         (counsel-switch-buffer . 7)))

;; Ivy-rich settings
(require-package 'ivy-rich)
(ivy-rich-mode 1)
(setq ivy-format-function #'ivy-format-function-line)
(setq ivy-rich-display-transformers-list
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer
                 '(:columns
                   ((ivy-rich-candidate (:width 40))
                    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                    (ivy-rich-switch-buffer-project (:width 15 :face success))
                    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
                   :predicate
                   (lambda (cand)
                     (if-let ((buffer (get-buffer cand)))
                         (with-current-buffer buffer
                           (not (derived-mode-p 'exwm-mode)))))))))

;; Counsel setting
(setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)

(require-package 'flx)  ;; Improves sorting for fuzzy-matched results
(eval-after-load 'ivy
  '(progn
     (setq ivy-flx-limit 10000)))


(provide 'init-search)
;;; init-search.el ends here
