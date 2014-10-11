;;; Compiled snippets and support files for `coffee-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'coffee-mode
		     '(("cl" "console.log(`yas/selected-text`$1)\n$0" "console.log" nil nil
			((yas/indent-line 'fixed)
			 (yas/wrap-around-region 'nil))
			nil nil nil)
		       ("cc" "class $1\n  constructor: ($2) ->\n    $3" "class" nil nil
			((yas/indent-line 'fixed)
			 (yas/wrap-around-region 'nil))
			nil nil nil)))


;;; Do not edit! File generated at Thu Aug 21 12:05:35 2014
