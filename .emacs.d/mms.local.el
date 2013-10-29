(setq default-frame-alist
      '((top . 0) (left . 0)
        (width . 236) (height . 73)
        (font . "SourceCodePro-10")))

;(setq initial-frame-alist '((top . 10) (left . 30)))

;; HAD TO MOVE THE NEXT 2 LINES OUT FROM THE init.el FILE
(require 'yasnippet)
(yas-global-mode 1)

