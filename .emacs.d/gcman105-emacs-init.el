
(defvar gcman105-emacs-init-file "~/.emacs.d/init.el")
(defvar gcman105-backups-folder "~/backups/")
(defvar gcman105-dropbox-folder "~/Dropbox/")
(defvar gcman105-emacs-config-dir
  (file-name-directory gcman105-emacs-init-file))

(setq user-emacs-directory gcman105-emacs-config-dir)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "emacs" gcman105-backups-folder))))

(setq custom-system-file (expand-file-name system-name gcman105-emacs-config-dir))
(setq custom-system-path (file-name-as-directory custom-system-file))

(require 'cl)
(require 'package)
(dolist (repo '(("melpa"        . "http://melpa.milkbox.net/packages/")
                ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                ("org"          . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives repo))

(defvar packages-list
  '(
    ac-emmet
    ac-helm
    ac-ispell
    ace-isearch
    ace-jump-mode
    ace-window
    ag
    ample-zen-theme
    auto-complete
    bm
    clojure-mode
    coffee-mode
    color-theme-solarized
    cursor-chg
    deft
    emmet-mode
    eredis
    evil
    evil-exchange
    evil-leader
    evil-matchit
    evil-nerd-commenter
    evil-numbers
    evil-surround
    evil-visualstar
    expand-region
    feature-mode
    fill-column-indicator
    flx
    flx-ido
    guide-key
    haml-mode
    handlebars-mode
    helm
    helm-ag
    helm-bm
    helm-css-scss
    helm-dash
    helm-descbinds
    helm-dictionary
    helm-emmet
    helm-helm-commands  ;
    helm-projectile
    helm-rails
    helm-rb
    helm-swoop
    heroku
    highlight-symbol
    http-post-simple
    js3-mode
    key-chord
    magit
    markdown-mode
    markdown-mode+
    move-text
    multi-term
    multiple-cursors
    org
    php-mode
    projectile
    rainbow-mode
    ruby-mode
    rvm
    scss-mode
    slim-mode
    smartparens
    subatomic256-theme
    xclip
    yaml-mode
    yasnippet
)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(defun gcman105/scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun gcman105/scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

;; Get current system's name
(defun gcman105/insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )

;; Get current system type
(defun gcman105/insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; (setq-default ido-ignore-buffers '(
;;                "^ "
;;                "*Completions*"
;;                "*Shell Command Output*"
;;                "*Messages*"
;;                "Async Shell Command"
;;                "*Compile-Log*"
;;                "*Customize"))

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq projectile-cache-file (concatenate 'string custom-system-path "projectile.cache"))
(setq projectile-known-projects-file (concatenate 'string custom-system-path "projectile-bookmarks.eld"))
(require 'projectile)
(projectile-global-mode)
(setq projectile-use-native-indexing t)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'helm-projectile-find-file)

(require 'helm-projectile)
(helm-projectile-on)
(require 'helm-config)

(require 'helm-descbinds)
(helm-descbinds-mode)

(require 'helm-dictionary)

(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)

(require 'recentf)
(setq recentf-load-file (concatenate 'string custom-system-path "recentf"))
(setq recentf-save-file (concatenate 'string custom-system-path "recentf"))
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)                           ; Enable guide-key-mode
(setq guide-key/highlight-command-regexp "rectangle")

(require 'auto-complete-config)
(setq ac-comphist-file (concatenate 'string custom-system-path "ac-comphist.dat"))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'ace-isearch)

(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(evil-mode t)

(require 'evil-surround)

(require 'evil-exchange)
(setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

;; HAD TO MOVE THE NEXT 2 LINES INTO THE SYSTEM FILE FOR EACH SYSTEM
;;(require 'yasnippet)
;;(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"            ;; personal snippets
  ;;        "/path/to/some/collection/"      ;; just some foo-mode snippets
  ;;        "/path/to/some/othercollection/" ;; some more foo-mode and a complete baz-mode
        "~/.emacs.d/yasnippet-snippets"    ;; the default collection
        ))

(add-to-list 'load-path "~/.emacs.d/gcman105")
(require 'rcodetools)
(require 'ruby-mode)
(require 'ruby-mode-expansions)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concatenate 'string custom-system-path "places"))

(setq bm-repository-file (concatenate 'string custom-system-path ".bm-repository"))
(setq-default bm-restore-repository-on-load t)
(require 'bm)
(setq-default bm-buffer-persistence t)       ; make bookmarks persistent as default

;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)
 
;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)
 
;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)
 
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

(setq deft-extension "md")
(setq deft-directory (expand-file-name "MarkDown" gcman105-dropbox-folder))
(setq deft-text-mode 'markdown-mode)
(setq deft-use-filename-as-title 1)

(require 'move-text)
;;(move-text-default-bindings)

(require 'multi-term)

(require 'multiple-cursors)

;;(add-to-list 'load-path "~/.emacs.d/elpa/eredis*")
(require 'eredis)

(require 'rainbow-mode)

(setq scss-compile-at-save nil)

;; Set to the location of your Org files on your local system
(setq org-directory (expand-file-name "org" gcman105-dropbox-folder))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory (expand-file-name "Apps/MobileOrg" gcman105-dropbox-folder))

;;(setq remember-data-file (expand-file-name "journal.org" org-directory))
(setq org-default-notes-file (expand-file-name "journal.org" org-directory))

;;(setq remember-annotation-functions '(org-remember-annotation))
;;(setq remember-handler-functions '(org-remember-handler))
;;(add-hook 'remember-mode-hook 'org-remember-apply-template)

'(org-refile-targets (quote (((expand-file-name "gtd.org" org-directory) :maxlevel . 1) 
           ((expand-file-name "someday.org" org-directory) :level . 2))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks")
   ;;"* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
   "* TODO %^{Brief Description} %^g\n  %?\n  %i\n  Added: %U")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
   "* %?\nEntered on %U\n  %i\n  %a")))

;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/Dropbox/org/gtd.org" "Tasks")
;;  ("Journal"   ?j "** %^{Head Line} %U %^g\n%i%?"  "~/Dropbox/org/journal.org")
;;  ("Clipboard" ?c "** %^{Head Line} %U %^g\n%c\n%?"  "~/Dropbox/org/journal.org")
;;  ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Dropbox/org/finances.org")
;;  ("Book" ?b "** %^{Book Title} %t :BOOK: \n%[~/Dropbox/org/.book_template.txt]\n" 
;;          "~/Dropbox/org/journal.org")
;;  ("Film" ?f "** %^{Film Title} %t :FILM: \n%[~/Dropbox/org/.film_template.txt]\n" 
;;          "~/Dropbox/org/journal.org")
;;  ("Daily Review" ?a "** %t :COACH: \n%[~/Dropbox/org/.daily_review.txt]\n" 
;;          "~/Dropbox/org/journal.org")
;;  ("Someday"   ?s "** %^{Someday Heading} %U\n%?\n"  "~/Dropbox/org/someday.org")
;;  ("Vocab"   ?v "** %^{Word?}\n%?\n"  "~/Dropbox/org/vocab.org")
;;  )
;;       )

(setq org-agenda-files (quote (
                               "~/Dropbox/org/birthday.org"
                               "~/Dropbox/org/gtd.org"
                               "~/Dropbox/org/emails.org"
                               "~/Dropbox/org/finances.org")))

(require 'org-octopress)
(setq org-octopress-directory-top       "~/Dropbox/gcman105-blog/source")
(setq org-octopress-directory-posts     "~/Dropbox/gcman105-blog/source/_posts")
(setq org-octopress-directory-org-top   "~/Dropbox/gcman105-blog/source")
(setq org-octopress-directory-org-posts "~/Dropbox/gcman105-blog/source/blog")
(setq org-octopress-setup-file          "~/Dropbox/gcman105-blog/org-sty/setupfile.org")

(defun gcman105/deft-mode-hook ()
  "deft-mode-hook"
  (turn-off-evil-mode))
(add-hook 'deft-mode-hook '(lambda() (gcman105/deft-mode-hook)))

(defun gcman105/markdown-mode-hook ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (gcman105/markdown-mode-hook)))

(add-hook 'html-mode-hook 'turn-off-auto-fill)

;;(add-hook 'org-mode-hook 'org-src-fontify-buffer)
(defun gcman105/org-mode-hook ()
  "org-mode-hook"
  (org-src-fontify-buffer)
  (turn-off-smartparens-mode))
(add-hook 'org-mode-hook '(lambda() (gcman105/org-mode-hook)))

(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(setq php-file-patterns (quote ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.php\\'")))

(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<f9>") 'recentf-open-files)
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key [f8] 'deft)
(global-set-key [S-f8] 'deft-new-file-named)
(global-set-key [C-return] 'emmet-expand-line)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "<f2>") 'ispell-word)

;; set bookmarking keys
(global-set-key (kbd "<C-f7>") 'bm-next)
(global-set-key (kbd "<f7>")   'bm-toggle)
(global-set-key (kbd "<S-f7>") 'bm-previous)
(global-set-key (kbd "<M-f7>") 'bm-show-all)

;; set move-text block bubble keys
(global-set-key (kbd "<S-f6>") 'move-text-up)
(global-set-key (kbd "<C-f6>") 'move-text-down)

(global-set-key (kbd "C-c m") 'multi-term)

;; set multiple-cursors.el keys
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; set keys for text scaling
(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x _") 'text-scale-decrease)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; set keys for spliting window
(global-set-key (kbd "C-\\") 'split-window-below)
(global-set-key (kbd "C-|") 'split-window-right)

;; set org-mode global keys
(define-key global-map "\C-ct" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; Unset Arrow keys, this should help force me to learn the Emacs keys!
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

;; set custom function keys
(global-set-key [M-up] 'gcman105/scroll-down-in-place)
(global-set-key [M-down] 'gcman105/scroll-up-in-place)

(add-hook 'org-mode-hook
    (lambda ()
      (local-unset-key "\C-c")
      (local-set-key "\C-cd" 'org-toodledo-mark-task-deleted)
      (local-set-key "\C-cs" 'org-toodledo-sync)
      )
    )

(add-hook 'org-agenda-mode-hook
    (lambda ()
      (local-unset-key "\C-c")
      (local-set-key "\C-cd" 'org-toodledo-agenda-mark-task-deleted)
      )
    )

(evilnc-default-hotkeys)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'helm-find-files
  "y" 'helm-show-kill-ring
  "o" 'helm-occur
  "v" 'helm-projectile
  "h" 'helm-man-woman
  "," 'helm-resume
  "." 'helm-calcul-expression
  "d" 'helm-descbinds
  "m" 'helm-mini
  "i" 'helm-semantic-or-imenu
  "p" 'ffap
  "j" 'ace-jump-mode
  "b" 'helm-buffers-list
  "k" 'kill-buffer)

(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.4)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
;;(add-hook 'ruby-mode-hook (lambda () (local-set-key "C-c C-c" 'xmp)))

(global-evil-matchit-mode)
(global-ace-isearch-mode)
(global-auto-complete-mode)
(global-font-lock-mode)                      ; activate font-lock mode (syntax coloring)
(global-linum-mode)                          ; add line numbers on the left
(global-visual-line-mode)                    ; wrap long lines
(global-hl-line-mode)                        ; highlight current line
(setq-default linum-format "%7d ")
(setq-default rainbow-mode t)                ; highlight color codes
(setq-default org-src-fontify-natively t)    ; fontify code in code blocks

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)                         ; hide scroll bar
(size-indication-mode t)

(setq inhibit-startup-screen t)
(setq max-specpdl-size 1800)
(show-paren-mode t)

(setq blink-cursor-mode 1)                   ; I like my cursor to blink
(setq x-stretch-cursor t)                    ; I also like my cursor to stretch
(setq evil-default-cursor 1)
(set-cursor-color "orange")                  ; I want an orange cursor

(setq column-number-mode 1)                  ; show column numbers

(setq gc-cons-threshold 20000000)            ; garbage collection tuning
(setq-default flyspell-mode nil)             ; turn off flyspell
(setq x-select-enable-clipboard t)           ; use the clipboard, so that copy/paste works

(setq echo-keystrokes 0.1)                   ; show keystrokes in progress

(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default indent-tabs-mode nil)

(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

(setq user-full-name "Gary Cheeseman"
      user-mail-address "gary@cheeseman.me.uk")

(if (eq system-type 'darwin)
    (progn
      (push "/usr/local/bin" exec-path)
      (setq osx-pseudo-daemon-mode t)
      (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))))

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (set-keyboard-coding-system nil)
  (setq mac-option-modifier nil)
  (menu-bar-mode t))

;; key bindings
(when (eq system-type 'darwin)               ; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)  ; sets fn-delete to be right-delete
  )

;; setup if we are using a graphic display ----------------------------------
(if (display-graphic-p)
  (setq xterm-mouse-mode nil)
  (setq server-mode nil))

(let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))

;; if its not a mac, do these things
;;(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty

;; setup theme --------------------------------------------------------------
;; load theme depening on window type
(when (eq window-system 'x)
;;  (load-theme 'ample-zen t)
  (load-theme 'subatomic256 t)
  )
(when (eq window-system 'ns)
  (load-theme 'ample-zen t)
;;  (load-theme 'afternoon t)
  )
(when (eq window-system nil)
  (load-theme 'ample-zen t)
;;  (load-theme 'solarized-light t)
  (global-hl-line-mode)                      ; highlight current line
  (blink-cursor-mode)
  (setq x-stretch-cursor t)
;;  (load-theme 'subatomic256 t)
  )

(setq custom-file (expand-file-name "emacs-customizations.el" gcman105-emacs-config-dir))
(load custom-file)

(load custom-system-file)

;; Flymake
;; (require 'flymake)
;; (global-set-key [C-f3] 'flymake-display-err-menu-for-current-line)
;; (global-set-key [C-f4] 'flymake-goto-next-error)
;; (setq flymake-log-level 3)

;; Flycheck mode
;; Enable flymake for all files
;;(require 'flycheck)
;;(add-hook 'find-file-hook 'flycheck-mode)

;; Load 'custom' system file ------------------------------------------------

;;; init.el ends here

;; Any lines below are for testing
;;(menu-bar-mode -1)
