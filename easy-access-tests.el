;;; easy-access-tests.el --- ERT test suite for easy-access  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy
;; Keywords: lisp, tests

;;; Commentary:
;;
;; ERT test suite for `easy-access'.  Run headless:
;;
;;     emacs --batch -L . -l easy-access-tests.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Tests are organised into eight groups:
;;
;;  1. Walk correctness        -- pure unit tests on `easy-access-walk'.
;;  2. Runtime dispatch        -- tests on `easy-access-lookup' directly.
;;  3. End-to-end through eval -- bare accessor forms reaching `eval'.
;;  4. End-to-end through load -- temp files loaded with the mode on.
;;  5. Interactive evaluation  -- simulates `C-x C-e', `C-M-x', `M-:'.
;;  6. Regression              -- guards that stock Elisp still works.
;;  7. Mode toggle             -- install/uninstall symmetry.
;;  8. Byte-compilation        -- walked forms compile and run correctly.
;;
;; The fixtures ensure `easy-access-mode' is enabled for each test case
;; that needs it, and restored to the pre-test state afterward.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'easy-access)

;;; -------------------------------------------------------------------------
;;; Fixtures
;;; -------------------------------------------------------------------------

(defmacro easy-access-tests--with-mode (&rest body)
  "Evaluate BODY with `easy-access-mode' enabled; restore prior state."
  (declare (indent 0))
  `(let ((was-on easy-access-mode))
     (unwind-protect
         (progn
           (unless was-on (easy-access-mode 1))
           ,@body)
       (unless was-on (easy-access-mode -1)))))

(defmacro easy-access-tests--without-mode (&rest body)
  "Evaluate BODY with `easy-access-mode' disabled; restore prior state."
  (declare (indent 0))
  `(let ((was-on easy-access-mode))
     (unwind-protect
         (progn
           (when was-on (easy-access-mode -1))
           ,@body)
       (when was-on (easy-access-mode 1)))))

(cl-defstruct easy-access-tests--point
  "Simple struct used to exercise struct-accessor dispatch."
  x y label)

;; Forward declarations for variables set by the loaded temp file in
;; `easy-access/e2e-load-file'.  These silence byte-compiler warnings.
(defvar easy-access-tests--loaded-plist)
(defvar easy-access-tests--loaded-index)

;;; -------------------------------------------------------------------------
;;; Group 1 -- Walk correctness
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/walk-atoms ()
  "Atoms pass through the walker unchanged."
  (dolist (atom '(42 :keyword "string" nil t symbol 3.14))
    (should (equal atom (easy-access-walk atom)))))

(ert-deftest easy-access/walk-keyword-accessor ()
  "(:K obj) rewrites to (easy-access-lookup obj :K)."
  (should (equal '(easy-access-lookup x :foo)
                 (easy-access-walk '(:foo x)))))

(ert-deftest easy-access/walk-integer-accessor ()
  "(N obj) rewrites to (easy-access-lookup obj N)."
  (should (equal '(easy-access-lookup x 3)
                 (easy-access-walk '(3 x)))))

(ert-deftest easy-access/walk-chained-accessor ()
  "(K1 K2 ... Kn obj) threads left-to-right -- the path reads in
natural order, matching Clojure's `get-in'."
  (should (equal '(easy-access-lookup
                   (easy-access-lookup
                    (easy-access-lookup obj :a)
                    :b)
                   2)
                 (easy-access-walk '(:a :b 2 obj)))))

(ert-deftest easy-access/walk-idempotent ()
  "Walking an already-walked form is a no-op."
  (dolist (form '((:foo x)
                  (3 x)
                  (:a :b 2 obj)
                  (let ((p '(:foo 1))) (:foo p))
                  (lambda (x) (:key x))))
    (let ((once (easy-access-walk form)))
      (should (equal once (easy-access-walk once))))))

(ert-deftest easy-access/walk-skips-quote ()
  "Quoted forms are data; walker leaves their contents alone."
  (should (equal '(quote (:foo bar))
                 (easy-access-walk '(quote (:foo bar)))))
  (should (equal '(quote (3 4 5))
                 (easy-access-walk '(quote (3 4 5))))))

(ert-deftest easy-access/walk-skips-function-quote ()
  "`function' / #'' quoted forms are data; walker leaves them alone."
  (should (equal '(function my-fn)
                 (easy-access-walk '(function my-fn)))))

(ert-deftest easy-access/walk-let-binding-positions ()
  "`let' binding names are NOT walked; RHS expressions ARE walked.
Body forms are also walked."
  (should (equal '(let ((x (easy-access-lookup p :foo)))
                    (easy-access-lookup x :bar))
                 (easy-access-walk '(let ((x (:foo p)))
                                      (:bar x))))))

(ert-deftest easy-access/walk-let-star ()
  "`let*' binding positions handled like `let'."
  (should (equal '(let* ((a (easy-access-lookup p :a))
                         (b (easy-access-lookup a :b)))
                    b)
                 (easy-access-walk '(let* ((a (:a p))
                                           (b (:b a)))
                                      b)))))

(ert-deftest easy-access/walk-lambda ()
  "Lambda arg lists are NOT walked; body IS walked."
  (should (equal '(lambda (x y) (easy-access-lookup x :foo))
                 (easy-access-walk '(lambda (x y) (:foo x))))))

(ert-deftest easy-access/walk-defun ()
  "Defun arg lists are NOT walked; body IS walked."
  (should (equal '(defun f (x y) "doc" (easy-access-lookup x :foo))
                 (easy-access-walk '(defun f (x y) "doc" (:foo x))))))

(ert-deftest easy-access/walk-condition-case ()
  "`condition-case' VAR stays untouched; FORM and handlers walked."
  (should (equal '(condition-case err
                      (easy-access-lookup p :foo)
                    (error (easy-access-lookup err :message)))
                 (easy-access-walk '(condition-case err
                                        (:foo p)
                                      (error (:message err)))))))

(ert-deftest easy-access/walk-dotted-pair ()
  "Improper lists / dotted pairs are data -- walker leaves them alone."
  (should (equal '("/path/file.elc" . 83)
                 (easy-access-walk '("/path/file.elc" . 83)))))

(ert-deftest easy-access/walk-cl-defstruct ()
  "`cl-defstruct' forms are declarative; walker does not touch them."
  (let ((form '(cl-defstruct my-point x y label)))
    (should (equal form (easy-access-walk form)))))

;;; -------------------------------------------------------------------------
;;; Group 2 -- Runtime dispatch (`easy-access-lookup')
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/lookup-plist ()
  "`easy-access-lookup' on a plist returns the matching value."
  (should (equal "hello" (easy-access-lookup '(:greeting "hello") :greeting)))
  (should (equal 42 (easy-access-lookup '(:a 1 :b 42 :c 3) :b))))

(ert-deftest easy-access/lookup-plist-missing ()
  "Missing plist keys return nil, matching `plist-get' semantics."
  (should (null (easy-access-lookup '(:a 1) :missing))))

(ert-deftest easy-access/lookup-integer-on-list ()
  "Integer keys index into lists via `nth'."
  (should (equal 'c (easy-access-lookup '(a b c d e) 2))))

(ert-deftest easy-access/lookup-integer-on-vector ()
  "Integer keys index into vectors via `elt'."
  (should (equal 30 (easy-access-lookup [10 20 30 40] 2))))

(ert-deftest easy-access/lookup-struct ()
  "Keyword dispatch on a cl-defstruct instance uses the generated accessor."
  (let ((p (make-easy-access-tests--point :x 1 :y 2 :label "origin")))
    (should (equal 1 (easy-access-lookup p :x)))
    (should (equal 2 (easy-access-lookup p :y)))
    (should (equal "origin" (easy-access-lookup p :label)))))

(ert-deftest easy-access/lookup-hash-table ()
  "Keyword dispatch on a hash-table falls back to `gethash'."
  (let ((h (make-hash-table :test 'equal)))
    (puthash :alpha "A" h)
    (puthash :beta "B" h)
    (should (equal "A" (easy-access-lookup h :alpha)))
    (should (equal "B" (easy-access-lookup h :beta)))))

(ert-deftest easy-access/lookup-alist ()
  "Keyword dispatch on an alist uses `assq' + `cdr'.
Alists are distinguished from plists by the shape of their CAR:
an alist's first element is a cons cell, a plist's is a keyword."
  (let ((al '((:name . "Ali") (:age . 42) (:city . "Kufa"))))
    (should (equal "Ali" (easy-access-lookup al :name)))
    (should (equal 42 (easy-access-lookup al :age)))
    (should (equal "Kufa" (easy-access-lookup al :city)))))

(ert-deftest easy-access/lookup-alist-missing ()
  "Missing alist keys return nil, matching `assq' semantics."
  (let ((al '((:a . 1) (:b . 2))))
    (should (null (easy-access-lookup al :missing)))))

(ert-deftest easy-access/lookup-invalid-signals ()
  "Nonsensical dispatch signals `easy-access-invalid-key'.
Keyword key on a non-list non-record non-hash-table is invalid;
integer key on a non-sequence is invalid."
  (should-error (easy-access-lookup 42 :foo) :type 'easy-access-invalid-key)
  (should-error (easy-access-lookup 'some-symbol 3)
                :type 'easy-access-invalid-key))

(ert-deftest easy-access/lookup-string-as-sequence ()
  "Strings ARE sequences -- integer keys return the character code."
  (should (equal ?i (easy-access-lookup "string" 3))))

;;; -------------------------------------------------------------------------
;;; Group 3 -- End-to-end through `eval'
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/e2e-eval-plist ()
  "Bare (:K P) reaches `eval' and evaluates correctly."
  (easy-access-tests--with-mode
    (should (equal 42 (eval '(:foo (list :foo 42)) t)))))

(ert-deftest easy-access/e2e-eval-integer ()
  "Bare (N S) reaches `eval' and evaluates correctly."
  (easy-access-tests--with-mode
    (should (equal 300 (eval '(2 (list 100 200 300 400)) t)))))

(ert-deftest easy-access/e2e-eval-chain ()
  "Three-level chain resolves in left-to-right path order.
Path `(1 :b :a obj)' reads: index 1 into OBJ, then `:b', then `:a'."
  (easy-access-tests--with-mode
    (should (equal "found!"
                   (eval '(1 :b :a (quote (ignored (:b (:a "found!")))))
                         t)))))

(ert-deftest easy-access/e2e-eval-struct ()
  "Struct accessor via bare (:field instance)."
  (easy-access-tests--with-mode
    (let ((result (eval '(let ((p (make-easy-access-tests--point
                                   :x 10 :y 20 :label "demo")))
                           (:label p))
                        t)))
      (should (equal "demo" result)))))

;;; -------------------------------------------------------------------------
;;; Group 4 -- End-to-end through `load'
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/e2e-load-file ()
  "A file loaded with the mode on honours accessor syntax."
  (let ((tmp (make-temp-file "easy-access-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(setq easy-access-tests--loaded-plist\n")
            (insert "      (:greeting (list :greeting \"from-file\")))\n")
            (insert "(setq easy-access-tests--loaded-index\n")
            (insert "      (1 (list :zero :one :two)))\n"))
          (easy-access-tests--with-mode
            (load tmp nil t t)
            (should (equal "from-file" easy-access-tests--loaded-plist))
            (should (equal :one easy-access-tests--loaded-index))))
      (delete-file tmp)
      (makunbound 'easy-access-tests--loaded-plist)
      (makunbound 'easy-access-tests--loaded-index))))

;;; -------------------------------------------------------------------------
;;; Group 5 -- Interactive evaluation
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/interactive-eval-last-sexp ()
  "Simulate `C-x C-e' on a bare keyword-accessor form."
  (easy-access-tests--with-mode
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(:a (list :a 55 :b 66))")
      (goto-char (point-max))
      (should (equal 55 (eval-last-sexp nil))))))

(ert-deftest easy-access/interactive-eval-last-sexp-integer ()
  "Simulate `C-x C-e' on a bare integer-accessor form."
  (easy-access-tests--with-mode
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(2 (list 10 20 30 40))")
      (goto-char (point-max))
      (should (equal 30 (eval-last-sexp nil))))))

(ert-deftest easy-access/interactive-eval-last-sexp-chained ()
  "Simulate `C-x C-e' on a chained accessor form.
Path `(0 :a obj)' = (index 0, then :a) = \"deep\"."
  (easy-access-tests--with-mode
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(0 :a (list (list :a \"deep\")))")
      (goto-char (point-max))
      (should (equal "deep" (eval-last-sexp nil))))))

;;; -------------------------------------------------------------------------
;;; Group 6 -- Regression: stock Elisp still works under the mode
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/regression-keyword-eq ()
  "Keywords still compare `eq' under the mode."
  (easy-access-tests--with-mode
    (should (eq :foo :foo))
    (should (keywordp :foo))))

(ert-deftest easy-access/regression-plist-get ()
  "Explicit `plist-get' still works -- it's what the walker expands TO."
  (easy-access-tests--with-mode
    (should (equal 1 (plist-get '(:a 1 :b 2) :a)))))

(ert-deftest easy-access/regression-pcase-keyword-pattern ()
  "`pcase' keyword patterns continue to match."
  (easy-access-tests--with-mode
    (should (equal 'matched
                   (pcase :foo
                     (:foo 'matched)
                     (_ 'no-match))))))

(ert-deftest easy-access/regression-backquote ()
  "Backquoted templates with keyword literals are preserved."
  (easy-access-tests--with-mode
    (let ((x 1) (y 2))
      (should (equal '(:a 1 :b 2) `(:a ,x :b ,y))))))

(ert-deftest easy-access/regression-cl-loop-clauses ()
  "`cl-loop' keyword clauses still work -- they are symbols, not keywords,
but commonly confused."
  (easy-access-tests--with-mode
    (should (equal '(1 4 9)
                   (cl-loop for x in '(1 2 3) collect (* x x))))))

(ert-deftest easy-access/regression-defstruct-slot-access ()
  "Existing struct accessors still work -- they're plain functions."
  (easy-access-tests--with-mode
    (let ((p (make-easy-access-tests--point :x 1 :y 2 :label "ok")))
      (should (equal 1 (easy-access-tests--point-x p)))
      (should (equal "ok" (easy-access-tests--point-label p))))))

;;; -------------------------------------------------------------------------
;;; Group 7 -- Mode toggle symmetry
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/toggle-off-restores-error ()
  "After disabling the mode, bare accessor forms again signal
`void-function'."
  (let ((was-on easy-access-mode))
    (unwind-protect
        (progn
          (unless was-on (easy-access-mode 1))
          (easy-access-mode -1)
          (should-error (eval '(:foo (list :foo 42)) t)
                        :type 'void-function))
      (when was-on (unless easy-access-mode (easy-access-mode 1))))))

(ert-deftest easy-access/toggle-off-restores-load-read-function ()
  "Disabling the mode restores the prior `load-read-function'."
  (let ((was-on easy-access-mode)
        (original load-read-function))
    (unwind-protect
        (progn
          (unless was-on (easy-access-mode 1))
          ;; Under the mode, load-read-function is our wrapper.
          (should (eq load-read-function #'easy-access--read))
          (easy-access-mode -1)
          ;; After disable, restored.
          (should (equal original load-read-function)))
      (when was-on (unless easy-access-mode (easy-access-mode 1))))))

;;; -------------------------------------------------------------------------
;;; Group 8 -- Byte-compilation
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/byte-compilation ()
  "A walked form byte-compiles and produces the same value as
interpreted evaluation."
  (easy-access-tests--with-mode
    (let* ((form '(lambda (p) (:foo p)))
           (walked (easy-access-walk form))
           (compiled (byte-compile walked)))
      (should (equal 99
                     (funcall compiled '(:foo 99)))))))

;;; -------------------------------------------------------------------------
;;; Group 9 -- Debug helpers
;;; -------------------------------------------------------------------------

(ert-deftest easy-access/expand-does-not-mutate-global-state ()
  "`easy-access-expand' is pure -- calling it leaves the mode state
unchanged."
  (let ((before easy-access-mode))
    (easy-access-expand '(:foo x))
    (should (eq before easy-access-mode))))

;;; -------------------------------------------------------------------------
;;; Group 10 -- Threading macros
;;; -------------------------------------------------------------------------
;;;
;;; Threading macros (`thread-first', `thread-last', and dash's `->'/`->>')
;;; expand AFTER the read-time walker runs.  The walker handles this by
;;; expanding listed threading macros one step and re-walking the result.

(ert-deftest easy-access/threading-thread-first-accessors ()
  "`thread-first' with accessor forms descends through the path.
`(thread-first OBJ (:a) (:b))' expands to `(:b (:a OBJ))' which
the walker rewrites to nested `easy-access-lookup' calls."
  (easy-access-tests--with-mode
    (require 'subr-x)
    (should (equal "threaded!"
                   (eval '(thread-first (list :a (list :b "threaded!"))
                            (:a)
                            (:b))
                         t)))))

(ert-deftest easy-access/threading-thread-first-mixed ()
  "`thread-first' mixing accessor forms with regular function calls.
`thread-first' inserts the value as the FIRST argument of each
subsequent form -- we pair it with accessor forms (which expect
their target first) and with `car', which also takes its target
first."
  (easy-access-tests--with-mode
    (require 'subr-x)
    (should (equal 10
                   (eval '(thread-first (list :xs (list 10 20 30))
                            (:xs)
                            (car))
                         t)))))

(ert-deftest easy-access/threading-thread-last-accessors ()
  "`thread-last' inserts OBJ as the LAST argument of each form.
`(:key)' has only one slot, so first-vs-last is indistinguishable --
both threading styles produce the same nested lookup."
  (easy-access-tests--with-mode
    (require 'subr-x)
    (should (equal "same"
                   (eval '(thread-last (list :a (list :b "same"))
                            (:a)
                            (:b))
                         t)))))

(ert-deftest easy-access/walk-thread-first-expansion ()
  "The walker rewrites `thread-first' into nested lookups by
expanding one step and re-walking the result."
  (should (equal '(easy-access-lookup (easy-access-lookup obj :a) :b)
                 (easy-access-walk '(thread-first obj (:a) (:b))))))

;;; -------------------------------------------------------------------------
;;; Group 11 -- `setf' support
;;; -------------------------------------------------------------------------
;;;
;;; `easy-access-lookup' is registered as a generalised-variable via
;;; `gv-define-setter', so `(setf (:K obj) val)' dispatches at runtime
;;; by type -- mirroring the reader.

;; Note: the setf tests below are written with `eval' of a quoted form
;; so that the raw accessor syntax `(setf (:k m) v)' is not seen by the
;; byte-compiler at test-file compile time (the mode is not active during
;; `make compile').  At test RUN time, the mode is active, the walker
;; rewrites the form, and the generalised-variable setter dispatches.
;;
;; The place-target must be a VARIABLE that is `setf'-able, not the
;; VALUE of the variable -- so we use dynamic scoping (`defvar' + `eval'
;; with lexical=nil) and let the symbol itself appear in the form.

(defvar easy-access-tests--setf-plist nil "Fixture for plist setf test.")
(defvar easy-access-tests--setf-struct nil "Fixture for struct setf test.")
(defvar easy-access-tests--setf-hash nil "Fixture for hash setf test.")
(defvar easy-access-tests--setf-list nil "Fixture for list-index setf test.")
(defvar easy-access-tests--setf-vector nil "Fixture for vector setf test.")
(defvar easy-access-tests--setf-alist nil "Fixture for alist setf test.")
(defvar easy-access-tests--setf-chain-plist nil "Chained plist-in-plist.")
(defvar easy-access-tests--setf-chain-mixed nil "Vector-in-alist fixture.")
(defvar easy-access-tests--setf-chain-struct nil "Struct-in-plist fixture.")

(ert-deftest easy-access/setf-plist-existing-key ()
  "`(setf (:k plist) v)' updates an existing plist key in place.
The place is the variable holding the plist -- `plist-put' mutates
the cdr for existing keys, so the variable sees the change."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-plist (list :a 1 :b 2 :c 3))
    (eval '(setf (:b easy-access-tests--setf-plist) 99) nil)
    (should (equal 99 (plist-get easy-access-tests--setf-plist :b)))
    (should (equal 1 (plist-get easy-access-tests--setf-plist :a)))
    (should (equal 3 (plist-get easy-access-tests--setf-plist :c)))))

(ert-deftest easy-access/setf-struct-slot ()
  "`(setf (:field instance) v)' updates a cl-defstruct slot."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-struct
          (make-easy-access-tests--point :x 1 :y 2 :label "origin"))
    (eval '(setf (:label easy-access-tests--setf-struct) "moved") nil)
    (should (equal "moved"
                   (easy-access-tests--point-label
                    easy-access-tests--setf-struct)))
    (should (equal 1 (easy-access-tests--point-x
                      easy-access-tests--setf-struct)))))

(ert-deftest easy-access/setf-hash-table ()
  "`(setf (:key hash-table) v)' is a `puthash'."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-hash (make-hash-table :test 'equal))
    (eval '(setf (:alpha easy-access-tests--setf-hash) "A") nil)
    (eval '(setf (:beta easy-access-tests--setf-hash) "B") nil)
    (should (equal "A" (gethash :alpha easy-access-tests--setf-hash)))
    (should (equal "B" (gethash :beta easy-access-tests--setf-hash)))))

(ert-deftest easy-access/setf-list-integer-index ()
  "`(setf (N list) v)' replaces the N-th element in place."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-list (list 10 20 30 40))
    (eval '(setf (1 easy-access-tests--setf-list) 99) nil)
    (should (equal '(10 99 30 40) easy-access-tests--setf-list))))

(ert-deftest easy-access/setf-vector-integer-index ()
  "`(setf (N vector) v)' is an `aset'."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-vector (vector 10 20 30))
    (eval '(setf (1 easy-access-tests--setf-vector) 99) nil)
    (should (equal [10 99 30] easy-access-tests--setf-vector))))

(ert-deftest easy-access/setf-alist-existing-key ()
  "`(setf (:k alist) v)' mutates the existing pair's cdr."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-alist
          (list (cons :name "Ali") (cons :age 42) (cons :city "Kufa")))
    (eval '(setf (:age easy-access-tests--setf-alist) 99) nil)
    (should (equal 99 (cdr (assq :age easy-access-tests--setf-alist))))
    (should (equal "Ali" (cdr (assq :name easy-access-tests--setf-alist))))
    (should (equal "Kufa"
                   (cdr (assq :city easy-access-tests--setf-alist))))))

;;; Chained setf -- the gv-setter composes through nested
;;; `easy-access-lookup' calls, so `(setf (:a :b obj) v)' means "walk the
;;; path :a then :b, set the endpoint".  The outer key in the reader is
;;; the LAST-applied; in setf, it is the slot being written.  Dispatch
;;; at each step is independent, so a path may freely cross container
;;; types.

(ert-deftest easy-access/setf-chain-plist-in-plist ()
  "`(setf (:a :b obj) v)' on a plist-of-plists mutates the inner value.
Path reads `:a' first, then `:b' -- so `:b' is the slot being written
on the result of `(:a obj)'."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-chain-plist
          (list :a (list :b 1 :c 2) :d "untouched"))
    (eval '(setf (:a :b easy-access-tests--setf-chain-plist) 99) nil)
    (should (equal 99 (plist-get (plist-get easy-access-tests--setf-chain-plist
                                            :a)
                                 :b)))
    ;; Sibling key on the inner plist is untouched.
    (should (equal 2 (plist-get (plist-get easy-access-tests--setf-chain-plist
                                           :a)
                                :c)))
    ;; Sibling key on the outer plist is untouched.
    (should (equal "untouched"
                   (plist-get easy-access-tests--setf-chain-plist :d)))))

(ert-deftest easy-access/setf-chain-vector-in-alist ()
  "Chained setf crosses container types.
The path `(:users 1 obj)' descends into an alist, then indexes into
the vector stored under `:users', and `aset's the new value in place."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-chain-mixed
          (list (cons :users (vector "Ali" "Hassan" "Hussain"))
                (cons :count 3)))
    (eval '(setf (:users 1 easy-access-tests--setf-chain-mixed) "Fatima")
          nil)
    (should (equal ["Ali" "Fatima" "Hussain"]
                   (cdr (assq :users
                              easy-access-tests--setf-chain-mixed))))
    (should (equal 3
                   (cdr (assq :count
                              easy-access-tests--setf-chain-mixed))))))

(ert-deftest easy-access/setf-chain-struct-in-plist ()
  "Chained setf writes through a plist into a struct slot.
Exercises that the gv-setter picks the struct-slot branch on the
LAST step of the path, not the plist branch of the first step."
  (easy-access-tests--with-mode
    (setq easy-access-tests--setf-chain-struct
          (list :point (make-easy-access-tests--point
                        :x 1 :y 2 :label "origin")
                :meta "keep"))
    (eval '(setf (:point :label easy-access-tests--setf-chain-struct)
                 "moved")
          nil)
    (should (equal "moved"
                   (easy-access-tests--point-label
                    (plist-get easy-access-tests--setf-chain-struct
                               :point))))
    ;; Other slot on the struct is untouched.
    (should (equal 1
                   (easy-access-tests--point-x
                    (plist-get easy-access-tests--setf-chain-struct
                               :point))))
    ;; Sibling key on the outer plist is untouched.
    (should (equal "keep"
                   (plist-get easy-access-tests--setf-chain-struct
                              :meta)))))

;;; -------------------------------------------------------------------------
;;; Group 12 -- Quoted-symbol accessors
;;; -------------------------------------------------------------------------
;;;
;;; Plain (non-keyword) symbols are often used as plist/alist/hash keys
;;; in Emacs -- `symbol-plist' is the obvious example, where the keys
;;; like `custom-group', `variable-documentation' are bare symbols.
;;; Writing `('custom-group (symbol-plist 'x))' reads the intent cleanly.
;;;
;;; The surface syntax is a /quoted/ symbol in CAR position.  The walker
;;; recognises `(quote SYM)' as a valid accessor key and rewrites the
;;; form into `(easy-access-lookup TARGET 'SYM)'.  Runtime dispatch
;;; mirrors the keyword path -- `assq' / `plist-get' / `gethash' /
;;; struct-slot, all of which accept plain symbols.

(defvar easy-access-tests--qsym-setf-plist nil "Fixture for plist test.")
(defvar easy-access-tests--qsym-setf-alist nil "Fixture for alist test.")

(ert-deftest easy-access/walk-quoted-symbol-accessor ()
  "('SYM obj) rewrites to (easy-access-lookup obj 'SYM)."
  (should (equal '(easy-access-lookup obj 'custom-group)
                 (easy-access-walk '('custom-group obj)))))

(ert-deftest easy-access/walk-quoted-symbol-chain ()
  "Chains mix quoted symbols with keywords and integers freely."
  (should (equal '(easy-access-lookup
                   (easy-access-lookup
                    (easy-access-lookup obj :a)
                    'b)
                   2)
                 (easy-access-walk '(:a 'b 2 obj)))))

(ert-deftest easy-access/lookup-symbol-plist ()
  "Bare-symbol keys work on a plist -- matching `symbol-plist' semantics."
  (let ((sp '(custom-group ((some stuff)) slot-name t)))
    (should (equal '((some stuff)) (easy-access-lookup sp 'custom-group)))
    (should (equal t (easy-access-lookup sp 'slot-name)))))

(ert-deftest easy-access/lookup-symbol-alist ()
  "Bare-symbol keys work on an alist via `assq'."
  (let ((al '((name . "Ali") (age . 42) (city . "Kufa"))))
    (should (equal "Ali" (easy-access-lookup al 'name)))
    (should (equal 42 (easy-access-lookup al 'age)))))

(ert-deftest easy-access/lookup-symbol-hash ()
  "Bare-symbol keys work on a hash table via `gethash'."
  (let ((h (make-hash-table :test 'eq)))
    (puthash 'alpha "A" h)
    (puthash 'beta "B" h)
    (should (equal "A" (easy-access-lookup h 'alpha)))))

(ert-deftest easy-access/lookup-symbol-struct ()
  "Bare-symbol keys work on a cl-defstruct instance -- the slot name
is used directly, without stripping a leading colon."
  (let ((p (make-easy-access-tests--point :x 1 :y 2 :label "origin")))
    (should (equal 1 (easy-access-lookup p 'x)))
    (should (equal "origin" (easy-access-lookup p 'label)))))

(ert-deftest easy-access/e2e-eval-symbol-plist ()
  "The motivating example -- `('custom-group (symbol-plist 'X))' works.
Uses a fresh symbol to keep the test hermetic."
  (easy-access-tests--with-mode
    (let ((sym (gensym "easy-access-test-sym")))
      (put sym 'custom-group '((mock-group)))
      (put sym 'slot-name t)
      (should (equal '((mock-group))
                     (eval `('custom-group (symbol-plist ',sym)) t)))
      (should (equal t
                     (eval `('slot-name (symbol-plist ',sym)) t))))))

(ert-deftest easy-access/setf-symbol-key-on-plist ()
  "`(setf ('SYM plist) v)' updates the plist in place."
  (easy-access-tests--with-mode
    (setq easy-access-tests--qsym-setf-plist (list 'a 1 'b 2 'c 3))
    (eval '(setf ('b easy-access-tests--qsym-setf-plist) 99) nil)
    (should (equal 99 (plist-get easy-access-tests--qsym-setf-plist 'b)))
    (should (equal 1 (plist-get easy-access-tests--qsym-setf-plist 'a)))))

(ert-deftest easy-access/setf-symbol-key-on-alist ()
  "`(setf ('SYM alist) v)' mutates the matching pair's cdr."
  (easy-access-tests--with-mode
    (setq easy-access-tests--qsym-setf-alist
          (list (cons 'name "Ali") (cons 'age 42)))
    (eval '(setf ('age easy-access-tests--qsym-setf-alist) 43) nil)
    (should (equal 43 (cdr (assq 'age easy-access-tests--qsym-setf-alist))))
    (should (equal "Ali"
                   (cdr (assq 'name easy-access-tests--qsym-setf-alist))))))

;;; Regression: `(quote X)' in DATA position (i.e. as a whole form, not
;;; as CAR of another form) must still be untouched.  This guards the
;;; walker against mistakenly treating quoted lists in value positions
;;; as accessor forms.

(ert-deftest easy-access/quoted-data-passes-through ()
  "A quoted list in value position is not an accessor -- it is data.
The walker must leave it unchanged; only a `(quote SYM)' in CAR
position of an outer list is recognised as an accessor key."
  (should (equal '(quote (:a b c))
                 (easy-access-walk '(quote (:a b c)))))
  (should (equal '(quote (1 2 3))
                 (easy-access-walk '(quote (1 2 3))))))

;;; -------------------------------------------------------------------------
;;; Group 13 -- `defcall' extensibility
;;; -------------------------------------------------------------------------
;;;
;;; `defcall' lifts CAR-shape dispatch into user-space -- the three
;;; built-in rules (integers, keywords, quoted-symbols) are themselves
;;; `defcall' forms, and user code can declare additional rules that
;;; register new CAR shapes as funcall-able.  These tests demonstrate
;;; extensibility end-to-end: define a rule mid-test, verify walk +
;;; runtime + `setf', then unregister to keep the registry clean.

(defun easy-access-tests--unregister-rule (name)
  "Drop any rule with NAME from `easy-access--rules'."
  (setq easy-access--rules
        (cl-remove-if (lambda (r) (eq (easy-access-rule-name r) name))
                      easy-access--rules)))

(defmacro easy-access-tests--with-rule (name &rest body)
  "Evaluate BODY; always unregister rule NAME afterward.
Used to keep the registry pristine across tests even when the
body errors out mid-way."
  (declare (indent 1))
  `(unwind-protect (progn ,@body)
     (easy-access-tests--unregister-rule ',name)))

(ert-deftest easy-access/defcall-non-accessor-passes-through ()
  "Ordinary function calls whose CAR is neither keyword, integer, nor
quoted symbol are left alone by the walker -- crucially, the runtime
dispatcher is not invoked, so no `easy-access-invalid-key' is raised.
`(+ 1 2)' is the canonical example: `+' is a bound function, no rule's
`:when' matches, walker is a no-op, form evaluates normally."
  (easy-access-tests--with-mode
    (should (equal 3 (eval '(+ 1 2) t)))
    (should (equal '(+ 1 2) (easy-access-walk '(+ 1 2))))
    (should (equal '(car (list 1 2 3))
                   (easy-access-walk '(car (list 1 2 3)))))))

(ert-deftest easy-access/defcall-custom-rule-get ()
  "A user-defined rule extends CAR-shape dispatch.
We register `strings-as-regex' where a string CAR matches against
its target string and returns the first match.  Verifies walk,
runtime dispatch, and that the rule's body sees `easy-access-setting-p'
as nil on read."
  (easy-access-tests--with-mode
    (easy-access-tests--with-rule strings-as-regex
      (eval
       '(defcall strings-as-regex (head target &optional value)
          :when (stringp head)
          "String CARs match as regex against the target string."
          (if (easy-access-setting-p)
              (error "strings-as-regex is read-only")
            (when (string-match head target)
              (match-string 0 target))))
       t)
      ;; Walker picks up the new rule -- ("[0-9]+" "abc123def") rewrites.
      (should (equal '(easy-access-lookup "abc123def" "[0-9]+")
                     (easy-access-walk '("[0-9]+" "abc123def"))))
      ;; End-to-end -- bare accessor form reaches `eval' and matches.
      (should (equal "123"
                     (eval '("[0-9]+" "abc123def") t))))))

(ert-deftest easy-access/defcall-custom-rule-setf ()
  "A user rule can own both getter and setter by branching on
`easy-access-setting-p'.  We register a rule where character CARs
act as a single-slot getter/setter over a one-element vector fixture."
  (easy-access-tests--with-mode
    (easy-access-tests--with-rule chars-as-slot
      (eval
       '(defcall chars-as-slot (head target &optional value)
          :when (characterp head)
          "Characters index into a one-element vector -- slot 0."
          (if (easy-access-setting-p)
              (aset target 0 value)
            (elt target 0)))
       t)
      (let ((v (vector "initial")))
        (should (equal "initial" (eval `(?a ,v) t)))
        (eval `(setf (?a ,v) "replaced") nil)
        (should (equal "replaced" (elt v 0)))))))

(ert-deftest easy-access/defcall-most-recent-wins ()
  "When two rules' `:when' predicates overlap, the most recently
defined rule fires.  Mirrors `defun' redefinition -- last one wins.
We register `int-doubles' then `int-triples' (both guard on integers)
and verify the second one shadows the first."
  (easy-access-tests--with-mode
    (easy-access-tests--with-rule int-triples
      (easy-access-tests--with-rule int-doubles
        (eval
         '(defcall int-doubles (head target &optional value)
            :when (integerp head)
            "Double HEAD; ignore TARGET."
            (ignore target value)
            (* 2 head))
         t)
        (eval
         '(defcall int-triples (head target &optional value)
            :when (integerp head)
            "Triple HEAD; ignore TARGET."
            (ignore target value)
            (* 3 head))
         t)
        ;; Most recent (triples) wins over doubles AND the built-in.
        (should (equal 21 (eval '(7 nil) t)))))))

(ert-deftest easy-access/defcall-redefinition-promotes ()
  "Redefining a rule by NAME upserts in place AND promotes it to
the front of the registry.  After re-evaluation the rule wins over
anything registered after its initial definition."
  (easy-access-tests--with-mode
    (easy-access-tests--with-rule int-shadow
      (easy-access-tests--with-rule int-original
        ;; Initial: int-original returns 100.
        (eval
         '(defcall int-original (head target &optional value)
            :when (integerp head)
            "First version."
            (ignore head target value) 100)
         t)
        ;; int-shadow registered AFTER -- currently wins.
        (eval
         '(defcall int-shadow (head target &optional value)
            :when (integerp head)
            "Shadowing rule."
            (ignore head target value) 200)
         t)
        (should (equal 200 (eval '(1 nil) t)))
        ;; Now redefine int-original -- should promote to front, win.
        (eval
         '(defcall int-original (head target &optional value)
            :when (integerp head)
            "Second version -- promoted."
            (ignore head target value) 300)
         t)
        (should (equal 300 (eval '(1 nil) t)))
        ;; Registry has no duplicate entries for int-original.
        (should (equal 1 (cl-count 'int-original easy-access--rules
                                   :key #'easy-access-rule-name)))))))

(ert-deftest easy-access/defcall-read-key ()
  "A `:read-key' canonicaliser reshapes HEAD before the rule body
sees it.  The built-in `quoted-symbols-as-accessors' uses
`:read-key cadr' to strip the `(quote ...)' wrapper -- we exercise
the same mechanism with a user rule.  `[[foo]]' CARs are bracketed
symbols unwrapped to plain symbols."
  (easy-access-tests--with-mode
    (easy-access-tests--with-rule bracketed-symbol
      (eval
       '(defcall bracketed-symbol (head target &optional value)
          :when (and (listp head)
                     (eq (car-safe head) 'bracket)
                     (symbolp (cadr head)))
          :read-key cadr                 ; strip the (bracket ...) wrapper
          "Treat (bracket SYM) CARs as bare-symbol accessors on plists."
          (ignore value)
          (plist-get target head))
       t)
      ;; Walker strips the wrapper via `:read-key' then renders the
      ;; canonical bare symbol as a quoted literal -- runtime reads the
      ;; symbol `foo' directly rather than dereferencing a variable.
      (should (equal '(easy-access-lookup obj 'foo)
                     (easy-access-walk '((bracket foo) obj))))
      (should (equal 42
                     (eval '((bracket foo) (list 'foo 42 'bar 7)) t))))))

(defvar easy-access-tests--setting-log nil
  "Log fixture for `defcall-body-sees-setting-p'.")
(defvar easy-access-tests--setting-cell nil
  "Mutable cell fixture for `defcall-body-sees-setting-p'.")

(ert-deftest easy-access/defcall-body-sees-setting-p ()
  "The rule body can branch on `(easy-access-setting-p)' to serve
both reads and writes from a single declaration.  We register a
rule keyed on the sentinel keyword `:probe', and verify the getter
arm fires on read and the setter arm fires on setf."
  (easy-access-tests--with-mode
    (easy-access-tests--with-rule probe-rule
      (setq easy-access-tests--setting-log nil)
      (setq easy-access-tests--setting-cell (vector 'initial))
      (eval
       '(defcall probe-rule (head target &optional value)
          :when (and (keywordp head) (eq head :probe))
          "Observe read vs write via a log; store through target vector."
          (ignore head)
          (if (easy-access-setting-p)
              (progn (push 'set easy-access-tests--setting-log)
                     (aset target 0 value))
            (push 'get easy-access-tests--setting-log)
            (aref target 0)))
       t)
      (should (equal 'initial
                     (eval '(:probe easy-access-tests--setting-cell) t)))
      (eval '(setf (:probe easy-access-tests--setting-cell) 'updated) nil)
      (should (equal 'updated (aref easy-access-tests--setting-cell 0)))
      ;; `push'es the most recent action to the front, so reverse order.
      (should (equal '(set get) easy-access-tests--setting-log)))))

;;; -------------------------------------------------------------------------
;;; Group 14 -- Walker safety under `require'
;;; -------------------------------------------------------------------------
;;;
;;; Regression guard: the walker runs inside `load-read-function', which
;;; is invoked for every `require'.  If the walker itself used `cl-*' or
;;; `seq-*' helpers, a `require' of `cl-extra' with the mode on could
;;; recursively re-enter the walker and crash.  The walker path uses
;;; plain `while'/`dolist' only -- this test verifies the invariant.

(ert-deftest easy-access/walker-no-recursive-load ()
  "Requiring `cl-extra' under an active mode does not crash.
If the walker ever regresses to using `cl-every' or similar, this
test will fail with a recursive-load error."
  (easy-access-tests--with-mode
    (should (progn (require 'cl-extra) t))))

;;; -------------------------------------------------------------------------
;;; Group 15 -- Integer indexing: negative indices & out-of-bounds nil
;;; -------------------------------------------------------------------------
;;;
;;; `integers-as-accessors' gives sequences a uniform, lenient indexing
;;; discipline -- irrespective of container shape:
;;;
;;;   * Negative indices count from the end, Python-style: -1 is last.
;;;   * Out-of-bounds reads (either side) return nil, not a signal.
;;;   * Writes past the end still error -- clobbering a non-existent
;;;     index has no sensible meaning.
;;;
;;; Previously, `elt' on a vector or string would signal
;;; `args-out-of-range' for an out-of-bounds index, while `nth' on a
;;; list would quietly return nil.  The rule now normalises both --
;;; matching `nth''s forgiveness across the three sequence kinds.

(defvar easy-access-tests--neg-vec nil "Fixture for negative-index vector setf.")
(defvar easy-access-tests--neg-list nil "Fixture for negative-index list setf.")

(ert-deftest easy-access/negative-index-vector-read ()
  "Negative indices on vectors count from the end.
`-1' is the last element, `-2' the second-to-last, and so on."
  (should (equal 'c (easy-access-lookup [a b c] -1)))
  (should (equal 'b (easy-access-lookup [a b c] -2)))
  (should (equal 'a (easy-access-lookup [a b c] -3))))

(ert-deftest easy-access/negative-index-string-read ()
  "Negative indices on strings count from the end.
`elt' on a string returns the character code."
  (should (equal ?c (easy-access-lookup "abc" -1)))
  (should (equal ?b (easy-access-lookup "abc" -2)))
  (should (equal ?a (easy-access-lookup "abc" -3))))

(ert-deftest easy-access/negative-index-list-read ()
  "Negative indices on lists count from the end -- same discipline as
vectors and strings.  Stock `nth' does not honour negative indices;
the rule normalises the index before dispatching."
  (should (equal 'c (easy-access-lookup '(a b c) -1)))
  (should (equal 'b (easy-access-lookup '(a b c) -2)))
  (should (equal 'a (easy-access-lookup '(a b c) -3))))

(ert-deftest easy-access/out-of-bounds-positive-returns-nil ()
  "Positive out-of-bounds integer reads return nil uniformly --
matching `nth's lenient behaviour rather than `elt's signal."
  (should (null (easy-access-lookup '(a b c) 99)))
  (should (null (easy-access-lookup [a b c] 99)))
  (should (null (easy-access-lookup "abc" 99))))

(ert-deftest easy-access/out-of-bounds-negative-returns-nil ()
  "Negative out-of-bounds integer reads also return nil -- `-99' on
a 3-sequence normalises to `-96', still out of bounds, caught by
the downstream guard."
  (should (null (easy-access-lookup '(a b c) -99)))
  (should (null (easy-access-lookup [a b c] -99)))
  (should (null (easy-access-lookup "abc" -99))))

(ert-deftest easy-access/negative-index-e2e-eval ()
  "End-to-end: bare negative-index accessor forms reach `eval' and
return the tail element.  This is the README's motivating example."
  (easy-access-tests--with-mode
    (should (equal 'c (eval '(-1 [a b c]) t)))
    (should (equal ?b (eval '(-2 "abc") t)))
    (should (equal 'c (eval '(-1 (list 'a 'b 'c)) t)))))

(ert-deftest easy-access/out-of-bounds-e2e-eval ()
  "End-to-end: out-of-bounds reads return nil rather than signalling
`args-out-of-range' -- previously the vector and string cases would
error.  The list case has always been lenient via `nth'."
  (easy-access-tests--with-mode
    (should (null (eval '(99 [a b c]) t)))
    (should (null (eval '(99 "abc") t)))
    (should (null (eval '(99 (list 'a 'b 'c)) t)))))

(ert-deftest easy-access/negative-index-setf-vector ()
  "`(setf (-1 v) x)' writes to the last slot via `aset' -- negative
normalisation applies on the setter path too."
  (easy-access-tests--with-mode
    (setq easy-access-tests--neg-vec (vector 'a 'b 'c))
    (eval '(setf (-1 easy-access-tests--neg-vec) 'Z) nil)
    (should (equal [a b Z] easy-access-tests--neg-vec))
    (eval '(setf (-3 easy-access-tests--neg-vec) 'A) nil)
    (should (equal [A b Z] easy-access-tests--neg-vec))))

(ert-deftest easy-access/negative-index-setf-list ()
  "`(setf (-1 xs) x)' writes to the last cons cell of a list via
`setcar' + `nthcdr'."
  (easy-access-tests--with-mode
    (setq easy-access-tests--neg-list (list 'a 'b 'c))
    (eval '(setf (-1 easy-access-tests--neg-list) 'Z) nil)
    (should (equal '(a b Z) easy-access-tests--neg-list))
    (eval '(setf (-2 easy-access-tests--neg-list) 'Y) nil)
    (should (equal '(a Y Z) easy-access-tests--neg-list))))

(ert-deftest easy-access/out-of-bounds-setf-still-errors ()
  "Writes past the end still signal -- this is `aset's native
behaviour on vectors, and `setcar' on a too-short `nthcdr' tail
for lists.  Lenient reads are asymmetric with strict writes by
design -- a nil-slot to write to has no sensible interpretation."
  (easy-access-tests--with-mode
    (setq easy-access-tests--neg-vec (vector 'a 'b 'c))
    (should-error (eval '(setf (99 easy-access-tests--neg-vec) 'X) nil))
    (setq easy-access-tests--neg-list (list 'a 'b 'c))
    (should-error (eval '(setf (99 easy-access-tests--neg-list) 'X) nil))))

;;; -------------------------------------------------------------------------
;;; Group 16 -- String keys as accessors
;;; -------------------------------------------------------------------------
;;;
;;; The `strings-as-accessors' rule lets string literals in CAR position
;;; look up by `equal' association — dispatching to `assoc' for alists
;;; and `gethash' for hash-tables.  This covers the common Emacs pattern
;;; `(cdr (assoc "Authorization" headers))'.

(ert-deftest easy-access/string-key-alist-read ()
  "String literal in CAR looks up an alist keyed by `equal'."
  (let ((headers '(("Authorization" . "Bearer tok")
                   ("Content-Type" . "application/json"))))
    (should (equal "Bearer tok" (easy-access-lookup headers "Authorization")))
    (should (equal "application/json" (easy-access-lookup headers "Content-Type")))
    (should (null (easy-access-lookup headers "Missing")))))

(ert-deftest easy-access/string-key-hash-read ()
  "String literal in CAR looks up an `equal' hash-table."
  (let ((h (make-hash-table :test 'equal)))
    (puthash "name" "Ali" h)
    (puthash "city" "Kufa" h)
    (should (equal "Ali" (easy-access-lookup h "name")))
    (should (null (easy-access-lookup h "missing")))))

(defvar easy-access-tests--str-alist nil "Fixture for string-key alist setf.")
(defvar easy-access-tests--str-hash nil "Fixture for string-key hash setf.")

(ert-deftest easy-access/string-key-alist-setf ()
  "`(setf (\"key\" alist) val)' writes to a string-keyed alist."
  (easy-access-tests--with-mode
    (setq easy-access-tests--str-alist
          (list (cons "a" 1) (cons "b" 2)))
    (eval '(setf ("a" easy-access-tests--str-alist) 99) nil)
    (should (equal 99 (easy-access-lookup easy-access-tests--str-alist "a")))
    ;; New key — nconc'd onto the end.
    (eval '(setf ("c" easy-access-tests--str-alist) 3) nil)
    (should (equal 3 (easy-access-lookup easy-access-tests--str-alist "c")))))

(ert-deftest easy-access/string-key-hash-setf ()
  "`(setf (\"key\" hash) val)' writes to a string-keyed hash-table."
  (easy-access-tests--with-mode
    (setq easy-access-tests--str-hash (make-hash-table :test 'equal))
    (puthash "x" 10 easy-access-tests--str-hash)
    (eval '(setf ("x" easy-access-tests--str-hash) 20) nil)
    (should (equal 20 (gethash "x" easy-access-tests--str-hash)))
    (eval '(setf ("y" easy-access-tests--str-hash) 30) nil)
    (should (equal 30 (gethash "y" easy-access-tests--str-hash)))))

(ert-deftest easy-access/string-key-chained ()
  "Chained string-key lookups thread left-to-right."
  (let ((db '(("users" . (("admin" . "Ali")
                           ("guest" . "Hassan"))))))
    (should (equal '(("admin" . "Ali") ("guest" . "Hassan"))
                   (easy-access-lookup db "users")))
    (should (equal "Ali"
                   (easy-access-lookup
                    (easy-access-lookup db "users")
                    "admin")))))

(defvar easy-access-tests--str-db nil "Fixture for string-key chained e2e.")

(ert-deftest easy-access/string-key-chained-e2e ()
  "End-to-end: chained string accessor forms via eval.
Path reads left-to-right: first \"users\" on db, then \"admin\" on the result."
  (easy-access-tests--with-mode
    (setq easy-access-tests--str-db
          '(("users" . (("admin" . "Ali")))))
    (should (equal "Ali"
                   (eval '("users" "admin" easy-access-tests--str-db) t)))))

(ert-deftest easy-access/string-key-error-on-non-collection ()
  "String key on a non-collection signals `easy-access-invalid-key'."
  (should-error (easy-access-lookup 42 "key")
                :type 'easy-access-invalid-key))

(ert-deftest easy-access/string-key-walker-recognises-form ()
  "The walker rewrites (\"key\" obj) into an `easy-access-lookup' call."
  (should (equal (easy-access-walk '("Auth" headers))
                 '(easy-access-lookup headers "Auth"))))

;;; -------------------------------------------------------------------------
;;; Group 17 -- Variable keys: known limitation
;;; -------------------------------------------------------------------------
;;;
;;; Bare symbols in CAR position remain function calls — the walker
;;; cannot distinguish `(var alist)' from `(some-fn arg)' at read-time.
;;; Users who need a variable key at runtime call `easy-access-lookup'
;;; directly.

(ert-deftest easy-access/variable-keys-are-not-accessors ()
  "Bare symbols in CAR position remain function calls — not accessors.
A form like (my-var alist) is indistinguishable from a function call
at walk-time, so the walker leaves it untouched."
  (should (equal (easy-access-walk '(foo bar))
                 '(foo bar)))
  (should (equal (easy-access-walk '(my-key my-obj))
                 '(my-key my-obj))))

(ert-deftest easy-access/variable-key-via-lookup-directly ()
  "For variable keys, call `easy-access-lookup' explicitly."
  (let ((key :name)
        (plist (list :name "Ali" :age 42)))
    (should (equal "Ali" (easy-access-lookup plist key))))
  (let ((key "Authorization")
        (headers '(("Authorization" . "Bearer tok"))))
    (should (equal "Bearer tok" (easy-access-lookup headers key)))))

(provide 'easy-access-tests)
;;; easy-access-tests.el ends here
