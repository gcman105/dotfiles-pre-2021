;;; Package --- gcman105's emacs init.el

;;; Commentary:

;;; Code:

(with-no-warnings
  (require 'cl))

(require 'package)

(dolist (repo '(("elpa"      . "http://tromey.com/elpa/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
;; (defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings)
;;   "A list of packages to ensure are installed at launch.")

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

(defvar grc-emacs-init-file "~/.emacs.d/init.el")
(defvar grc-backups-folder "~/backups/")
(defvar grc-dropbox-folder "~/Dropbox/")

(defvar grc-emacs-config-dir
      (file-name-directory grc-emacs-init-file))

(defvar user-emacs-directory grc-emacs-config-dir)

(defvar backup-directory-alist
      (list (cons "." (expand-file-name "emacs" grc-backups-folder))))

;; setup yasnippet  ---------------------------------------------------------
;; HAD TO MOVE THE NEXT 2 LINES INTO THE SYSTEM FILE FOR EACH SYSTEM
;;(require 'yasnippet)
;;(yas-global-mode 1)

(defvar yas-snippet-dirs
      '("~/.emacs.d/snippets"            ;; personal snippets
;;        "/path/to/some/collection/"      ;; just some foo-mode snippets
;;        "/path/to/some/othercollection/" ;; some more foo-mode and a complete baz-mode
        "~/.emacs.d/snippets-default"    ;; the default collection
        ))

;; Set up 'custom' system ---------------------------------------------------
(setq custom-file (expand-file-name "emacs-customizations.el" grc-emacs-config-dir))
(load custom-file)

;; setup email --------------------------------------------------------------
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gcman105@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "server.local")

;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
;; (setq gnus-large-newsgroup 20)

;; (setq gnus-select-method
;; '(nnimap "gmail"
;;         (nnimap-address "imap.gmail.com")
;;         (nnimap-server-port 993)
;;         (nnimap-stream ssl)))

;; Deft config for nvALT files with md extension ----------------------------
(defvar deft-extension "md")
(defvar deft-directory (expand-file-name "MarkDown" grc-dropbox-folder))
(defvar deft-text-mode 'markdown-mode)
(defvar deft-use-filename-as-title t)
(global-set-key [f8] 'deft)
(global-set-key [S-f8] 'deft-new-file)
(global-set-key (kbd "<C-f8>") 'deft-new-file-named)

;; Markdown settings --------------------------------------------------------
;; Stop markdown-mode interfeering with yasnippet
(defun markdown-unset-tab ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))

(add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))

;; Unset Arrow keys, this should help force me to learn the Emacs keys!
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

;; Setup GLOBAL keys --------------------------------------------------------
;; set bookmarking keys
(global-set-key (kbd "<C-f7>") 'bm-next)
(global-set-key (kbd "<f7>")   'bm-toggle)
(global-set-key (kbd "<S-f7>") 'bm-previous)
(global-set-key (kbd "<M-f7>") 'bm-show-all)
;; set block bubble keys
(global-set-key (kbd "<S-f6>") 'move-text-up)
(global-set-key (kbd "<C-f6>") 'move-text-down)

;; set keys for multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; set keys for expand-region.el
;; first we need to redefine text-scale-decrease
(global-set-key (kbd "C-_") 'text-scale-decrease)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; org-mode settings --------------------------------------------------------
(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-ca" 'org-agenda)

;; Set to the location of your Org files on your local system
(defvar org-directory (expand-file-name "org" grc-dropbox-folder))
;; Set to the name of the file where new notes will be stored
(defvar org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory))
;; Set to <your Dropbox root directory>/MobileOrg.
(defvar org-mobile-directory (expand-file-name "Apps/MobileOrg" grc-dropbox-folder))
(defvar remember-data-file (expand-file-name "journal.org" org-directory))
(defvar org-default-notes-file (expand-file-name "journal.org" org-directory))
(defvar remember-annotation-functions '(org-remember-annotation))
(defvar remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

'(org-refile-targets (quote (((expand-file-name "gtd.org" org-directory) :maxlevel . 1) 
                              ((expand-file-name "someday.org" org-directory) :level . 2))))

(setq org-remember-templates
    '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/Dropbox/org/gtd.org" "Tasks")
      ("Journal"   ?j "** %^{Head Line} %U %^g\n%i%?"  "~/Dropbox/org/journal.org")
      ("Clipboard" ?c "** %^{Head Line} %U %^g\n%c\n%?"  "~/Dropbox/org/journal.org")
      ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Dropbox/org/finances.org")
      ("Book" ?b "** %^{Book Title} %t :BOOK: \n%[~/Dropbox/org/.book_template.txt]\n" 
         "~/Dropbox/org/journal.org")
          ("Film" ?f "** %^{Film Title} %t :FILM: \n%[~/Dropbox/org/.film_template.txt]\n" 
         "~/Dropbox/org/journal.org")
      ("Daily Review" ?a "** %t :COACH: \n%[~/Dropbox/org/.daily_review.txt]\n" 
         "~/Dropbox/org/journal.org")
      ("Someday"   ?s "** %^{Someday Heading} %U\n%?\n"  "~/Dropbox/org/someday.org")
      ("Vocab"   ?v "** %^{Word?}\n%?\n"  "~/Dropbox/org/vocab.org")
     )
   )

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
;; Also allow hash to be entered
(if (eq system-type 'darwin)
  (progn
    (push "/usr/local/bin" exec-path)
    (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Tabs and Indents
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq scroll-bar-mode nil)

;; Minor Mode Hooks
(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'html-mode-hook 'turn-off-flypell)
(add-hook 'org-mode-hook 'turn-off-flyspell)

;; Flymake
;; (require 'flymake)
;; (global-set-key [C-f3] 'flymake-display-err-menu-for-current-line)
;; (global-set-key [C-f4] 'flymake-goto-next-error)
;; (setq flymake-log-level 3)

;; Flycheck mode
;; Enable flymake for all files
(add-hook 'find-file-hook 'flycheck-mode)

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; SCSS options
(defvar scss-compile-at-save nil)

;; setup if we are using a graphic display ----------------------------------
(if (display-graphic-p)
  (menu-bar-mode nil)
  (xterm-mouse-mode nil)
  (server-mode nil))

;; I like my cursor to blink
(blink-cursor-mode nil)

;; show column numbers
(column-number-mode 1)

(let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))

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
;;; init.el ends here
