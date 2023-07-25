
# Emacs Kısayolları

Bu belge, Emacs metin düzenleyicisinde kullanılacak bazı kullanışlı kısayolları içerir.

## Temalar

- `C-c l`: Light tema
- `C-c d`: Dark tema

## Tam Ekran ve Opaklık Ayarları (Mac)

- `M-ƒ` (Command-Option-f): Tam ekran moduna geçiş
- `M-C-8` (Command-Control-8): Pencerenin opaklığını azaltır
- `M-C-9` (Command-Control-9): Pencerenin opaklığını artırır
- `M-C-7` (Command-Control-7): Pencerenin opaklığını %100'e çıkartır

## Arama ve Gezinme

- `C-s`: Swiper işlevi (Mevcut tamponu arama)
- `M-x`: `counsel-M-x` işlevi (Emacs komutlarını arama ve çalıştırma)
- `C-x b`: `counsel-ibuffer` işlevi (Mevcut tamponları gösterir ve arama)
- `C-M-j`: `counsel-switch-buffer` işlevi (Mevcut tamponlar arasında geçiş)
- `C-M-l`: `counsel-imenu` işlevi (İmenu arama ve seçim, kod dosyasındaki fonksiyonlar veya sınıflar gibi)
- `M-o`: `switch-window` işlevi (Mevcut pencereler arasında geçiş)

## Pencere Yönetimi

- `C-x 2`: Dikey olarak pencereyi bölen ve yeni pencerede diğer tamponu gösteren bir işlev
- `C-x 3`: Yatay olarak pencereyi bölen ve yeni pencerede diğer tamponu gösteren bir işlev
- `C-x 1`: `sanityinc/toggle-delete-other-windows` işlevi (Diğer tüm pencereleri siler veya önceki pencere konfigürasyonunu geri yükler)
- `C-x |`: `split-window-horizontally-instead` işlevi (Diğer tüm pencereleri siler ve mevcut pencereyi ekranın üst yarısında olacak şekilde yatay olarak böler)
- `C-x _`: `split-window-vertically-instead` işlevi (Diğer tüm pencereleri siler ve mevcut pencereyi ekranın sol yarısında olacak şekilde dikey olarak böler)
- `C-c`: `sanityinc/toggle-current-window-dedication` işlevi (Mevcut pencerenin mevcut tampona adanmış olup olmadığını değiştirir)
- `C-S-[Up,Down,Left,Right]`: Birden çok pencere varken focus olan pencreyi seçtinizi yöne doğru kayrıdır.

## Git

- `C-x v t`: Git zaman makinesini açıp kapatmak
- `M-f12` ya da `C-x g`: `magit-status` işlevi (Magit durumu)
- `C-x M-g`: `magit-dispatch` işlevi (Magit komutlarını çağırma)

## Diğer Kısayollar

Bu bölümde, Emacs'ta kullanılabilir çeşitli diğer kısayollar ve atamalar verilmiştir.

- `Shift + Enter`: 'sanityinc/newline-at-end-of-line' işlevini çalıştırır
- `M-i`: 'symbol-overlay-put' işlevini çalıştırır
- `M-S + i`: 'symbol-overlay-remove-all' işlevini çalıştırır
- `M-n`: 'symbol-overlay-jump-next' işlevini çalıştırır
- `M-p`: 'symbol-overlay-jump-prev' işlevini çalıştırır
- `M-S + z`: 'zap-up-to-char' işlevini çalıştırır
- `M-S + y`: 'browse-kill-ring' işlevini çalıştırır
- `C-;`: 'avy-goto-char-timer' işlevini çalıştırır
- `C-<`: 'mc/mark-previous-like-this' işlevini çalıştırır
- `C->`: 'mc/mark-next-like-this' işlevini çalıştırır
- `C-+`: 'mc/mark-next-like-this' işlevini çalıştırır
- `C-c, C-<`: 'mc/mark-all-like-this' işlevini çalıştırır
- `C-.`: 'set-mark-command' işlevini çalıştırır
- `C-x, C-.`: 'pop-global-mark' işlevini çalıştırır
- `C-;`: 'avy-goto-char-timer' işlevini çalıştırır
- `C-o`: 'sanityinc/open-line-with-reindent' işlevini çalıştırır

Daha fazlası için kodu kontrol edin!

---

Bu, Emacs'ın bazı kısayollarını açıklayan bir README örneğidir. Daha fazla bilgi için, kodunuzu veya Emacs belgelerini kontrol edin. Kısayollar genellikle kullanıcıların kişisel tercihlerine göre değişir, bu yüzden kendi kısayollarınızı eklemekte özgürsünüz.

