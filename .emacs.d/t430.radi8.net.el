;;; package --- t430.radi8.net.el local settings

;;; Commentary:

;;; Code:

(setq default-frame-alist
      '((top . 10) (left . 44)
        (width . 180) (height . 44)
        (font . "Source Code Pro 10")))

;; HAD TO MOVE THE NEXT 2 LINES OUT FROM THE init.el FILE
(require 'yasnippet)
(yas-global-mode 1)

;;; t430.radi8.net.el ends here
