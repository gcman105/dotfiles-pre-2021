;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
		     '(("ishf" "it { should have_field(:$1).of_type($2)$3 }$0" "it should have_field" nil nil nil nil nil nil)
		       ("ishfd" "it { should have_field(:$1).of_type($2).with_default_value_of($3) }$0" "it should have_field with default" nil nil nil nil nil nil)
		       ("ishi" "it { should have_index_for($1: 1)$2 }$0" "it should have_index_for" nil nil nil nil nil nil)
		       ("isvp" "it { should validate_presence_of(:$1)$2 }$0" "it should validate_presence_of" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Aug 21 12:05:35 2014
