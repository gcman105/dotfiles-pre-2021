
(require 'cl)
(require 'package)
(dolist (repo '(("elpa"    . "http://tromey.com/elpa/")
                ("melpa"   . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(package-initialize)

(defvar packages-list
  '(evil evil-leader evil-numbers
   evil-matchit evil-nerd-commenter
   evil-exchange evil-visualstar
   evil-surround
   key-chord deft markdown-mode markdown-mode+
   ample-zen-theme subatomic256-theme
   color-theme-solarized
   magit multi-term
   flx flx-ido
   move-text
   http-post-simple eredis projectile
   helm
   helm-ag helm-css-scss helm-emmet helm-rails helm-rb
   helm-bm helm-dash helm-helm-commands helm-projectile
   helm-swoop helm-descbinds helm-dictionary
   heroku
   coffee-mode js3-mode slim-mode haml-mode feature-mode
   emmet-mode auto-complete
   yaml-mode
   ag ac-emmet ac-helm ac-ispell
   multiple-cursors
   php-mode ruby-mode
   xclip yasnippet
   ace-jump-mode ace-isearch ace-window
   guide-key
   smartparens
   bm scss-mode
   expand-region
   rainbow-mode
   fill-column-indicator
   clojure-mode
   handlebars-mode
   cursor-chg
   highlight-symbol
   rvm)
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

(defvar grc-emacs-init-file "~/.emacs.d/init.el")
(defvar grc-backups-folder "~/backups/")
(defvar grc-dropbox-folder "~/Dropbox/")
(defvar grc-emacs-config-dir
  (file-name-directory grc-emacs-init-file))

(setq user-emacs-directory grc-emacs-config-dir)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "emacs" grc-backups-folder))))

(setq custom-system-file (expand-file-name system-name grc-emacs-config-dir))
(setq custom-system-path (file-name-as-directory custom-system-file))
(setq ede-project-placeholder-cache-file (concatenate 'string custom-system-path "ede-projects.el"))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command" "*Compile-Log*"
               "*Customize"))
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
(global-auto-complete-mode 1)

(require 'ace-isearch)
(global-ace-isearch-mode +1)

(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'evil-surround)
(global-evil-matchit-mode 1)

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
(setq deft-directory (expand-file-name "MarkDown" grc-dropbox-folder))
(setq deft-text-mode 'markdown-mode)
(setq deft-use-filename-as-title 1)

(defun gcman-deft-mode-hook ()
  "deft-mode-hook"
  (turn-off-evil-mode))
(add-hook 'deft-mode-hook '(lambda() (gcman-deft-mode-hook)))

(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<f9>") 'recentf-open-files)
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key [f8] 'deft)
(global-set-key [S-f8] 'deft-new-file-named)

;; set bookmarking keys
(global-set-key (kbd "<C-f7>") 'bm-next)
(global-set-key (kbd "<f7>")   'bm-toggle)
(global-set-key (kbd "<S-f7>") 'bm-previous)
(global-set-key (kbd "<M-f7>") 'bm-show-all)

(global-linum-mode t)                        ; add line numbers on the left
(setq scroll-bar-mode -1)                    ; hide scroll bars
(setq org-src-fonfify-natively t)            ; fontify code in code blocks
(org-src-fontify-buffer)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq user-full-name "Gary Cheeseman"
      user-mail-address "gary@cheeseman.me.uk")

;; gcman Markdown mode hook -------------------------------------------------
;; Stop markdown-mode interfeering with yasnippet
(defun gcman-markdown-mode-hook ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (gcman-markdown-mode-hook)))

;; Unset Arrow keys, this should help force me to learn the Emacs keys!
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

 

;; Setup GLOBAL keys --------------------------------------------------------

;; move text
(require 'move-text)
;;(move-text-default-bindings)
;; set block bubble keys
(global-set-key (kbd "<S-f6>") 'move-text-up)
(global-set-key (kbd "<C-f6>") 'move-text-down)

;; set keys for multi-term
(require 'multi-term)
(global-set-key (kbd "C-c m") 'multi-term)

;; set keys for multiple-cursors.el
(require 'multiple-cursors)
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

;; org-mode settings --------------------------------------------------------

;; Useful key bindings for org-mode
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

(define-key global-map "\C-ct" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; Set to the location of your Org files on your local system
(setq org-directory (expand-file-name "org" grc-dropbox-folder))
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory))
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory (expand-file-name "Apps/MobileOrg" grc-dropbox-folder))
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

(setq org-agenda-files (quote ("~/Dropbox/org/birthday.org" "~/Dropbox/org/gtd.org" "~/Dropbox/org/emails.org" "~/Dropbox/org/finances.org")))

(setq php-file-patterns (quote ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.php\\'")))

;; setup eredis ---------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/eredis*")
(require 'eredis)

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
;; Also allow hash to be entered
(if (eq system-type 'darwin)
    (progn
      (push "/usr/local/bin" exec-path)
      (setq osx-pseudo-daemon-mode t)
      (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(define-key global-map (kbd "<f2>") 'ispell-word)

;; Minor Mode Hooks
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Flymake
;; (require 'flymake)
;; (global-set-key [C-f3] 'flymake-display-err-menu-for-current-line)
;; (global-set-key [C-f4] 'flymake-goto-next-error)
;; (setq flymake-log-level 3)

;; Flycheck mode
;; Enable flymake for all files
;;(require 'flycheck)
;;(add-hook 'find-file-hook 'flycheck-mode)

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; SCSS options
(setq scss-compile-at-save nil)

;; setup if we are using a graphic display ----------------------------------
(if (display-graphic-p)
  (setq xterm-mouse-mode nil)
  (setq server-mode nil))

;; I like my cursor to blink and stretch
(setq blink-cursor-mode 1)
(setq x-stretch-cursor t)

;; show column numbers
(setq column-number-mode 1)

(let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))

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

;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

(setq evil-default-cursor 1)
(set-cursor-color "orange")

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.4)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key [M-up] 'scroll-down-in-place)
(global-set-key [M-down] 'scroll-up-in-place)

(global-set-key [C-return] 'emmet-expand-line)

;; Turn off flyspell
(setq-default flyspell-mode nil)

;; if its not a mac, do these things
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (set-keyboard-coding-system nil)
  (setq mac-option-modifier nil)
  (menu-bar-mode t))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Get current system's name
(defun insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )

;; Get current system type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )

(evil-mode t)
(setq inhibit-startup-screen t)
(setq max-specpdl-size 1800)
(show-paren-mode t)

;; key bindings
(when (eq system-type 'darwin)               ; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)  ; sets fn-delete to be right-delete
  )

;;(global-hl-line-mode)                      ; highlight current line
(global-linum-mode t)                        ; add line numbers on the left
(setq linum-format "%7d ")

(require 'rainbow-mode)
(rainbow-mode t)

(evil-exchange-install)
(evilnc-default-hotkeys)

;; garbage collection tuning
(setq gc-cons-threshold 20000000)

;; xmpfilter and rcodetools
(require 'rcodetools)
(require 'ruby-mode)
(require 'ruby-mode-expansions)
(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
;;(add-hook 'ruby-mode-hook (lambda () (local-set-key "C-c C-c" 'xmp)))

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

;; Set up 'custom' emacs ----------------------------------------------------
(setq custom-file (expand-file-name "emacs-customizations.el" grc-emacs-config-dir))
(load custom-file)

;; Load 'custom' system file ------------------------------------------------
(load custom-system-file)

;;; init.el ends here
