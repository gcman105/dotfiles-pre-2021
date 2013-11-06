;;; package --- t430.radi8.net.el local settings

;;; Commentary:

;;; Code:

(setq default-frame-alist
      '((top . 1) (left . 45)
        (width . 185) (height . 44)
        (font . "Source Code Pro 10")))

;; Save place in file when I exit
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/t430/places")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
 ido-save-directory-list-file "~/.emacs.d/t430/ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (defvar resize-minibuffer-window-max-height 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
    (setq recentf-load-file "~/.emacs.d/t430/recentf")
    (setq recentf-save-file "~/.emacs.d/t430/recentf")
    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key (kbd "<f9>") 'recentf-open-files)

;; HAD TO MOVE THE NEXT 2 LINES OUT FROM THE init.el FILE
(require 'yasnippet)
(yas-global-mode 1)

;;; t430.radi8.net.el ends here
