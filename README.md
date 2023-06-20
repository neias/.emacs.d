
theme
(global-set-key (kbd "C-c l") 'light)
(global-set-key (kbd "C-c d") 'dark)

mac os 
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-_") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h


Bu, M-ƒ (Mac'te Command-Option-f) kısayolunu tam ekran moduna geçiş yapmak için kullanır.
Bu, M-C-8 (Mac'te Command-Control-8) kısayolunu, mevcut pencerenin opaklığını azaltmak için kullanır.
Bu, M-C-9 (Mac'te Command-Control-9) kısayolunu, mevcut pencerenin opaklığını artırmak için kullanır.
Bu, M-C-7` (Mac'te Command-Control-7) kısayolunu, mevcut pencerenin opaklığını %100'e çıkarmak için kullanır.


Bu, C-s kısayolunu swiper işlevine atar. Swiper, mevcut tamponu (buffer) aramanıza izin verir.
Bu, M-x kısayolunu counsel-M-x işlevine atar. Bu işlev, Emacs komutlarını aramanıza ve çalıştırmanıza olanak sağlar.
Bu, C-x b kısayolunu counsel-ibuffer işlevine atar. Bu işlev, mevcut tamponları (buffer) gösterir ve aramanıza olanak sağlar.
Bu, C-M-j kısayolunu counsel-switch-buffer işlevine atar. Bu işlev, mevcut tamponlar (buffer) arasında geçiş yapmanızı sağlar.

counsel-imenu ??? 
Bu, C-M-l kısayolunu counsel-imenu işlevine atar. Bu işlev, imenu (örneğin bir kod dosyasındaki fonksiyonlar veya sınıflar gibi) aramanızı ve seçmenizi sağlar.

M-o kısayolunu switch-window işlevine atar. Bu işlev, mevcut pencereler arasında geçiş yapmanızı sağlar.
C-x 2 kısayolunu dikey olarak pencereyi bölen ve yeni pencerede diğer tamponu gösteren bir işleve atar.
C-x 3 kısayolunu yatay olarak pencereyi bölen ve yeni pencerede diğer tamponu gösteren bir işleve atar.
C-x 1 kısayolunu sanityinc/toggle-delete-other-windows işlevine atar. Bu işlev, diğer tüm pencereleri siler veya önceki pencere konfigürasyonunu geri yükler.
C-x | kısayolunu split-window-horizontally-instead işlevine atar. Bu işlev, diğer tüm pencereleri siler ve mevcut pencereyi ekranın üst yarısında olacak şekilde yatay olarak böler.
C-x _ kısayolunu split-window-vertically-instead işlevine atar. Bu işlev, diğer tüm pencereleri siler ve mevcut pencereyi ekranın sol yarısında olacak şekilde dikey olarak böler.
<f7> kısayolunu sanityinc/split-window işlevine atar. Bu işlev, pencereyi böler ve en son tamponu diğer pencerede görüntüler.
C-c <down> kısayolunu sanityinc/toggle-current-window-dedication işlevine atar. Bu işlev, mevcut pencerenin mevcut tampona adanmış olup olmadığını değiştirir.

 Shift + Enter tuşlarına basıldığında 'sanityinc/newline-at-end-of-line' işlevini çalıştırır.
 Alt + i", "Alt + Shift + i", "Alt + n", "Alt + p" tuş kombinasyonlarına belirli işlevleri atar.
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))

Alt + Shift + z" tuş kombinasyonuna 'zap-up-to-char' işlevini atar.
Alt + Shift + y" tuş kombinasyonuna 'browse-kill-ring' işlevini atar
Ctrl + ;" tuş kombinasyonuna 'avy-goto-char-timer' işlevini atar.


(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines


(require-package 'multiple-cursors)
(kbd "C-<"), (kbd "C->"), (kbd "C-+"), (kbd "C-c C-<") - "Ctrl + <", "Ctrl + >", "Ctrl + +", "Ctrl + c, Ctrl + <" tuş kombinasyonlarına belirli işlevleri atar.
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-;") 'avy-goto-char-timer))


(global-unset-key [M-left])
(global-unset-key [M-right])

(require-package 'move-dup)
(global-set-key [M-up] 'move-dup-move-lines-up)
(global-set-key [M-down] 'move-dup-move-lines-down)
(global-set-key [M-S-up] 'move-dup-move-lines-up)
(global-set-key [M-S-down] 'move-dup-move-lines-down)

(global-set-key (kbd "C-c C-d") 'move-dup-duplicate-down)
(global-set-key (kbd "C-c C-u") 'move-dup-duplicate-up)


(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-j") 'join-line)



git
Git zaman makinesini açıp kapatmak için C-x v t kısayolunu ayarlar.
magit-status fonksiyonuna meta + F12 kısayolunu atar.

(global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)   magit-status fonksiyonuna C-x g kısayolunu atar.
  (global-set-key (kbd "C-x M-g") 'magit-dispatch) magit-dispatch fonksiyonuna C-x M-g kısayolunu atar.
  (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file): sanityinc/magit-or-vc-log-file fonksiyonuna l (vc-prefix-map üzerinden) kısayolunu atar.

(define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up): magit-section-up fonksiyonuna C-M-<up> kısayolunu atar.
(fullframe magit-status magit-mode-quit-window): magit-status çalıştırıldığında, çıkış yaptığında magit-mode-quit-window fonksiyonunu çağırmak için fullframe'i kullanır.
(add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))): magit-mode'da meta + h kısayolunun etkisini kaldırır.
(define-key vc-prefix-map (kbd "f") 'vc-git-grep): vc-git-grep fonksiyonuna f (vc-prefix-map üzerinden) kısayolunu atar.
(compile (concat "git svn " command))): Git'in svn komutlarını çalıştırmak için compile fonksiyonunu kullanır.


projectile
C-c p
