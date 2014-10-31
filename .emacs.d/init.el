;;; init.el --- Where all the magic begins
;;
;; This file loads both
;; - Org-mode : http://orgmode.org/ and
;; - Org-babel: http://orgmode.org/worg/org-contrib/babel/org-babel.php#library-of-babel
;;
;; It then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; setting el-get packaged org-mode to initial load path, should fail
;; safely to builtin org
(let* ((org-dir (expand-file-name "org" (expand-file-name "el-get" dotfiles-dir)))
       (load-path (append (list org-dir) (or load-path nil))))
  (require 'org-id)
  (require 'org-element)
  ;;TODO make bug report, this contains defcustom definitions that are needed during tangle operations... 
  (require 'org)
  (mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$")))

;;; init.el ends here
