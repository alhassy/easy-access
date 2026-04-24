;;; easy-access-tests.el --- ERT test suite for easy-access  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy
;; Keywords: lisp, tests

;;; Commentary:
;;
;; ERT test suite for `easy-access'.  Uses `deftestfixture' from snap.el
;; for fixtures.  Run headless:
;;
;;     emacs --batch -L . -L ~/snap --eval '(package-initialize)' \
;;           -l easy-access-tests.el -f ert-run-tests-batch-and-exit
;;
;; Tests are organised into twenty groups:
;;
;;  1. Walk correctness        -- pure unit tests on `easy-access-walk'.
;;  2. Runtime dispatch        -- tests on `easy-access-lookup' directly.
;;  3. End-to-end through eval -- bare accessor forms reaching `eval'.
;;  4. End-to-end through load -- temp files loaded with the mode on.
;;  5. Interactive evaluation  -- simulates `C-x C-e', `C-M-x', `M-:'.
;;  6. Regression              -- guards that stock Elisp still works.
;;  7. Mode toggle             -- install/uninstall symmetry.
;;  8. Byte-compilation        -- walked forms compile and run correctly.
;;  9. Debug helpers           -- `easy-access-expand' purity.
;; 10. Threading macros        -- `thread-first'/`thread-last' interop.
;; 11. `setf' support          -- generalised-variable dispatch.
;; 12. Quoted-symbol accessors -- `('SYM obj)' syntax.
;; 13. `defcall' extensibility -- user-defined CAR-shape rules.
;; 14. Walker safety           -- no recursive-load crash.
;; 15. Negative/OOB indexing   -- Python-style negative indices.
;; 16. String keys             -- `("str" alist)' syntax.
;; 17. Variable keys           -- known limitation documentation.
;; 18. Walker special-forms    -- pcase, cond, cl-defmethod, etc.
;; 19. GV macro integration    -- cl-incf, push, cl-decf on accessor places.
;; 20. Threading macro interop -- keyword/string/integer/quoted-symbol keys.
;;
;; `defaccesstest' wraps each test that needs it in an
;; `easy-access-mode' enable/disable fixture.  `deftest' (the identity
;; fixture from snap.el) is used for tests that need no mode wrapping.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'easy-access)
(require 'snap)  ;; deftestfixture, deftest — load path set by Makefile

;;; -------------------------------------------------------------------------
;;; Fixtures
;;; -------------------------------------------------------------------------

(deftestfixture defaccesstest
  "Enable `easy-access-mode' for the test body; restore prior state."
  (let ((was-on easy-access-mode))
    (unwind-protect
        (progn
          (unless was-on (easy-access-mode 1))
          &body)
      (unless was-on (easy-access-mode -1)))))

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

(deftest "walk-atoms -- atoms pass through the walker unchanged"
  (dolist (atom '(42 :keyword "string" nil t symbol 3.14))
    (should (equal atom (easy-access-walk atom)))))

(deftest "walk-keyword-accessor -- (:K obj) rewrites to (easy-access-lookup obj :K)"
  (should (equal '(easy-access-lookup x :foo)
                 (easy-access-walk '(:foo x)))))

(deftest "walk-integer-accessor -- (N obj) rewrites to (easy-access-lookup obj N)"
  (should (equal '(easy-access-lookup x 3)
                 (easy-access-walk '(3 x)))))

(deftest "walk-chained-accessor -- (K1 K2 ... Kn obj) threads left-to-right"
  (should (equal '(easy-access-lookup
                   (easy-access-lookup
                    (easy-access-lookup obj :a)
                    :b)
                   2)
                 (easy-access-walk '(:a :b 2 obj)))))

(deftest "walk-idempotent -- walking an already-walked form is a no-op"
  (dolist (form '((:foo x)
                  (3 x)
                  (:a :b 2 obj)
                  (let ((p '(:foo 1))) (:foo p))
                  (lambda (x) (:key x))))
    (let ((once (easy-access-walk form)))
      (should (equal once (easy-access-walk once))))))

(deftest "walk-skips-quote -- quoted forms are data"
  (should (equal '(quote (:foo bar))
                 (easy-access-walk '(quote (:foo bar)))))
  (should (equal '(quote (3 4 5))
                 (easy-access-walk '(quote (3 4 5))))))

(deftest "walk-skips-function-quote -- #' quoted forms are data"
  (should (equal '(function my-fn)
                 (easy-access-walk '(function my-fn)))))

(deftest "walk-let-binding-positions -- names NOT walked, RHS and body ARE"
  (should (equal '(let ((x (easy-access-lookup p :foo)))
                    (easy-access-lookup x :bar))
                 (easy-access-walk '(let ((x (:foo p)))
                                      (:bar x))))))

(deftest "walk-let-star -- binding positions handled like let"
  (should (equal '(let* ((a (easy-access-lookup p :a))
                         (b (easy-access-lookup a :b)))
                    b)
                 (easy-access-walk '(let* ((a (:a p))
                                           (b (:b a)))
                                      b)))))

(deftest "walk-lambda -- arg lists NOT walked, body IS"
  (should (equal '(lambda (x y) (easy-access-lookup x :foo))
                 (easy-access-walk '(lambda (x y) (:foo x))))))

(deftest "walk-defun -- arg lists NOT walked, body IS"
  (should (equal '(defun f (x y) "doc" (easy-access-lookup x :foo))
                 (easy-access-walk '(defun f (x y) "doc" (:foo x))))))

(deftest "walk-condition-case -- VAR untouched, FORM and handlers walked"
  (should (equal '(condition-case err
                      (easy-access-lookup p :foo)
                    (error (easy-access-lookup err :message)))
                 (easy-access-walk '(condition-case err
                                        (:foo p)
                                      (error (:message err)))))))

(deftest "walk-dotted-pair -- improper lists are data"
  (should (equal '("/path/file.elc" . 83)
                 (easy-access-walk '("/path/file.elc" . 83)))))

(deftest "walk-cl-defstruct -- declarative forms are untouched"
  (let ((form '(cl-defstruct my-point x y label)))
    (should (equal form (easy-access-walk form)))))

;;; -------------------------------------------------------------------------
;;; Group 2 -- Runtime dispatch (`easy-access-lookup')
;;; -------------------------------------------------------------------------

(deftest "lookup-plist -- returns the matching value"
  (should (equal "hello" (easy-access-lookup '(:greeting "hello") :greeting)))
  (should (equal 42 (easy-access-lookup '(:a 1 :b 42 :c 3) :b))))

(deftest "lookup-plist-missing -- returns nil"
  (should (null (easy-access-lookup '(:a 1) :missing))))

(deftest "lookup-integer-on-list -- indexes via nth"
  (should (equal 'c (easy-access-lookup '(a b c d e) 2))))

(deftest "lookup-integer-on-vector -- indexes via elt"
  (should (equal 30 (easy-access-lookup [10 20 30 40] 2))))

(deftest "lookup-struct -- keyword dispatch on cl-defstruct"
  (let ((p (make-easy-access-tests--point :x 1 :y 2 :label "origin")))
    (should (equal 1 (easy-access-lookup p :x)))
    (should (equal 2 (easy-access-lookup p :y)))
    (should (equal "origin" (easy-access-lookup p :label)))))

(deftest "lookup-hash-table -- keyword dispatch falls back to gethash"
  (let ((h (make-hash-table :test 'equal)))
    (puthash :alpha "A" h)
    (puthash :beta "B" h)
    (should (equal "A" (easy-access-lookup h :alpha)))
    (should (equal "B" (easy-access-lookup h :beta)))))

(deftest "lookup-alist -- keyword dispatch via assq + cdr"
  (let ((al '((:name . "Ali") (:age . 42) (:city . "Kufa"))))
    (should (equal "Ali" (easy-access-lookup al :name)))
    (should (equal 42 (easy-access-lookup al :age)))
    (should (equal "Kufa" (easy-access-lookup al :city)))))

(deftest "lookup-alist-missing -- returns nil"
  (let ((al '((:a . 1) (:b . 2))))
    (should (null (easy-access-lookup al :missing)))))

(deftest "lookup-invalid-signals -- nonsensical dispatch signals error"
  (should-error (easy-access-lookup 42 :foo) :type 'easy-access-invalid-key)
  (should-error (easy-access-lookup 'some-symbol 3)
                :type 'easy-access-invalid-key))

(deftest "lookup-string-as-sequence -- integer keys return the character code"
  (should (equal ?i (easy-access-lookup "string" 3))))

;;; -------------------------------------------------------------------------
;;; Group 3 -- End-to-end through `eval'
;;; -------------------------------------------------------------------------

(defaccesstest "e2e-eval-plist -- bare (:K P) reaches eval"
  (should (equal 42 (eval '(:foo (list :foo 42)) t))))

(defaccesstest "e2e-eval-integer -- bare (N S) reaches eval"
  (should (equal 300 (eval '(2 (list 100 200 300 400)) t))))

(defaccesstest "e2e-eval-chain -- three-level chain resolves left-to-right"
  (should (equal "found!"
                 (eval '(1 :b :a (quote (ignored (:b (:a "found!")))))
                       t))))

(defaccesstest "e2e-eval-struct -- struct accessor via bare (:field instance)"
  (let ((result (eval '(let ((p (make-easy-access-tests--point
                                 :x 10 :y 20 :label "demo")))
                         (:label p))
                      t)))
    (should (equal "demo" result))))

;;; -------------------------------------------------------------------------
;;; Group 4 -- End-to-end through `load'
;;; -------------------------------------------------------------------------

(defaccesstest "e2e-load-file -- a file loaded with the mode on honours accessor syntax"
  (let ((tmp (make-temp-file "easy-access-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(setq easy-access-tests--loaded-plist\n")
            (insert "      (:greeting (list :greeting \"from-file\")))\n")
            (insert "(setq easy-access-tests--loaded-index\n")
            (insert "      (1 (list :zero :one :two)))\n"))
          (load tmp nil t t)
          (should (equal "from-file" easy-access-tests--loaded-plist))
          (should (equal :one easy-access-tests--loaded-index)))
      (delete-file tmp)
      (makunbound 'easy-access-tests--loaded-plist)
      (makunbound 'easy-access-tests--loaded-index))))

;;; -------------------------------------------------------------------------
;;; Group 5 -- Interactive evaluation
;;; -------------------------------------------------------------------------
;;;
;;; snap.el installs `:around' advice on `eval-last-sexp' that calls
;;; `backward-sexp' to peek at the form under point.  In batch mode
;;; this interacts poorly with `with-temp-buffer', so we temporarily
;;; remove the advice for the duration of each test.

(defaccesstest "interactive-eval-last-sexp -- C-x C-e on keyword accessor"
  (unwind-protect
      (progn
        (advice-remove 'eval-last-sexp #'my/eval-last-sexp--run-test)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(:a (list :a 55 :b 66))")
          (goto-char (point-max))
          (should (equal 55 (eval-last-sexp nil)))))
    (advice-add 'eval-last-sexp :around #'my/eval-last-sexp--run-test)))

(defaccesstest "interactive-eval-last-sexp-integer -- C-x C-e on integer accessor"
  (unwind-protect
      (progn
        (advice-remove 'eval-last-sexp #'my/eval-last-sexp--run-test)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(2 (list 10 20 30 40))")
          (goto-char (point-max))
          (should (equal 30 (eval-last-sexp nil)))))
    (advice-add 'eval-last-sexp :around #'my/eval-last-sexp--run-test)))

(defaccesstest "interactive-eval-last-sexp-chained -- C-x C-e on chained accessor"
  (unwind-protect
      (progn
        (advice-remove 'eval-last-sexp #'my/eval-last-sexp--run-test)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(0 :a (list (list :a \"deep\")))")
          (goto-char (point-max))
          (should (equal "deep" (eval-last-sexp nil)))))
    (advice-add 'eval-last-sexp :around #'my/eval-last-sexp--run-test)))

;;; -------------------------------------------------------------------------
;;; Group 6 -- Regression: stock Elisp still works under the mode
;;; -------------------------------------------------------------------------

(defaccesstest "regression-keyword-eq -- keywords still compare eq"
  (should (eq :foo :foo))
  (should (keywordp :foo)))

(defaccesstest "regression-plist-get -- explicit plist-get still works"
  (should (equal 1 (plist-get '(:a 1 :b 2) :a))))

(defaccesstest "regression-pcase-keyword-pattern -- pcase keywords still match"
  (should (equal 'matched
                 (pcase :foo
                   (:foo 'matched)
                   (_ 'no-match)))))

(defaccesstest "regression-backquote -- templates with keyword literals preserved"
  (let ((x 1) (y 2))
    (should (equal '(:a 1 :b 2) `(:a ,x :b ,y)))))

(defaccesstest "regression-cl-loop-clauses -- cl-loop keyword clauses still work"
  (should (equal '(1 4 9)
                 (cl-loop for x in '(1 2 3) collect (* x x)))))

(defaccesstest "regression-defstruct-slot-access -- existing accessors still work"
  (let ((p (make-easy-access-tests--point :x 1 :y 2 :label "ok")))
    (should (equal 1 (easy-access-tests--point-x p)))
    (should (equal "ok" (easy-access-tests--point-label p)))))

;;; -------------------------------------------------------------------------
;;; Group 7 -- Mode toggle symmetry
;;; -------------------------------------------------------------------------

(deftest "toggle-off-restores-error -- bare accessor signals void-function"
  (let ((was-on easy-access-mode))
    (unwind-protect
        (progn
          (unless was-on (easy-access-mode 1))
          (easy-access-mode -1)
          (should-error (eval '(:foo (list :foo 42)) t)
                        :type 'void-function))
      (when was-on (unless easy-access-mode (easy-access-mode 1))))))

(deftest "toggle-off-restores-load-read-function"
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

(defaccesstest "byte-compilation -- walked forms compile and produce same value"
  (let* ((form '(lambda (p) (:foo p)))
         (walked (easy-access-walk form))
         (compiled (byte-compile walked)))
    (should (equal 99
                   (funcall compiled '(:foo 99))))))

;;; -------------------------------------------------------------------------
;;; Group 9 -- Debug helpers
;;; -------------------------------------------------------------------------

(deftest "expand-does-not-mutate-global-state -- easy-access-expand is pure"
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

(defaccesstest "threading-thread-first-accessors -- descends through the path"
  (should (equal "threaded!"
                 (eval '(thread-first (list :a (list :b "threaded!"))
                          (:a)
                          (:b))
                       t))))

(defaccesstest "threading-thread-first-mixed -- accessors with regular function calls"
  (should (equal 10
                 (eval '(thread-first (list :xs (list 10 20 30))
                          (:xs)
                          (car))
                       t))))

(defaccesstest "threading-thread-last-accessors -- inserts as last argument"
  (should (equal "same"
                 (eval '(thread-last (list :a (list :b "same"))
                          (:a)
                          (:b))
                       t))))

(deftest "walk-thread-first-expansion -- walker rewrites into nested lookups"
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

(defaccesstest "setf-plist-existing-key -- (setf (:k plist) v) updates in place"
  (setq easy-access-tests--setf-plist (list :a 1 :b 2 :c 3))
  (eval '(setf (:b easy-access-tests--setf-plist) 99) nil)
  (should (equal 99 (plist-get easy-access-tests--setf-plist :b)))
  (should (equal 1 (plist-get easy-access-tests--setf-plist :a)))
  (should (equal 3 (plist-get easy-access-tests--setf-plist :c))))

(defaccesstest "setf-struct-slot -- (setf (:field instance) v) updates slot"
  (setq easy-access-tests--setf-struct
        (make-easy-access-tests--point :x 1 :y 2 :label "origin"))
  (eval '(setf (:label easy-access-tests--setf-struct) "moved") nil)
  (should (equal "moved"
                 (easy-access-tests--point-label
                  easy-access-tests--setf-struct)))
  (should (equal 1 (easy-access-tests--point-x
                    easy-access-tests--setf-struct))))

(defaccesstest "setf-hash-table -- (setf (:key hash-table) v) is a puthash"
  (setq easy-access-tests--setf-hash (make-hash-table :test 'equal))
  (eval '(setf (:alpha easy-access-tests--setf-hash) "A") nil)
  (eval '(setf (:beta easy-access-tests--setf-hash) "B") nil)
  (should (equal "A" (gethash :alpha easy-access-tests--setf-hash)))
  (should (equal "B" (gethash :beta easy-access-tests--setf-hash))))

(defaccesstest "setf-list-integer-index -- (setf (N list) v) replaces the Nth element"
  (setq easy-access-tests--setf-list (list 10 20 30 40))
  (eval '(setf (1 easy-access-tests--setf-list) 99) nil)
  (should (equal '(10 99 30 40) easy-access-tests--setf-list)))

(defaccesstest "setf-vector-integer-index -- (setf (N vector) v) is an aset"
  (setq easy-access-tests--setf-vector (vector 10 20 30))
  (eval '(setf (1 easy-access-tests--setf-vector) 99) nil)
  (should (equal [10 99 30] easy-access-tests--setf-vector)))

(defaccesstest "setf-alist-existing-key -- (setf (:k alist) v) mutates the pair"
  (setq easy-access-tests--setf-alist
        (list (cons :name "Ali") (cons :age 42) (cons :city "Kufa")))
  (eval '(setf (:age easy-access-tests--setf-alist) 99) nil)
  (should (equal 99 (cdr (assq :age easy-access-tests--setf-alist))))
  (should (equal "Ali" (cdr (assq :name easy-access-tests--setf-alist))))
  (should (equal "Kufa"
                 (cdr (assq :city easy-access-tests--setf-alist)))))

;;; Chained setf -- the gv-setter composes through nested
;;; `easy-access-lookup' calls, so `(setf (:a :b obj) v)' means "walk the
;;; path :a then :b, set the endpoint".  The outer key in the reader is
;;; the LAST-applied; in setf, it is the slot being written.  Dispatch
;;; at each step is independent, so a path may freely cross container
;;; types.

(defaccesstest "setf-chain-plist-in-plist -- mutates the inner value"
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
                 (plist-get easy-access-tests--setf-chain-plist :d))))

(defaccesstest "setf-chain-vector-in-alist -- crosses container types"
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
                            easy-access-tests--setf-chain-mixed)))))

(defaccesstest "setf-chain-struct-in-plist -- writes through plist into struct slot"
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
                            :meta))))

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

(deftest "walk-quoted-symbol-accessor -- ('SYM obj) rewrites to lookup"
  (should (equal '(easy-access-lookup obj 'custom-group)
                 (easy-access-walk '('custom-group obj)))))

(deftest "walk-quoted-symbol-chain -- mixes quoted symbols with keywords and integers"
  (should (equal '(easy-access-lookup
                   (easy-access-lookup
                    (easy-access-lookup obj :a)
                    'b)
                   2)
                 (easy-access-walk '(:a 'b 2 obj)))))

(deftest "lookup-symbol-plist -- bare-symbol keys on a plist"
  (let ((sp '(custom-group ((some stuff)) slot-name t)))
    (should (equal '((some stuff)) (easy-access-lookup sp 'custom-group)))
    (should (equal t (easy-access-lookup sp 'slot-name)))))

(deftest "lookup-symbol-alist -- bare-symbol keys via assq"
  (let ((al '((name . "Ali") (age . 42) (city . "Kufa"))))
    (should (equal "Ali" (easy-access-lookup al 'name)))
    (should (equal 42 (easy-access-lookup al 'age)))))

(deftest "lookup-symbol-hash -- bare-symbol keys via gethash"
  (let ((h (make-hash-table :test 'eq)))
    (puthash 'alpha "A" h)
    (puthash 'beta "B" h)
    (should (equal "A" (easy-access-lookup h 'alpha)))))

(deftest "lookup-symbol-struct -- bare-symbol key on cl-defstruct"
  (let ((p (make-easy-access-tests--point :x 1 :y 2 :label "origin")))
    (should (equal 1 (easy-access-lookup p 'x)))
    (should (equal "origin" (easy-access-lookup p 'label)))))

(defaccesstest "e2e-eval-symbol-plist -- ('custom-group (symbol-plist 'X)) works"
  (let ((sym (gensym "easy-access-test-sym")))
    (put sym 'custom-group '((mock-group)))
    (put sym 'slot-name t)
    (should (equal '((mock-group))
                   (eval `('custom-group (symbol-plist ',sym)) t)))
    (should (equal t
                   (eval `('slot-name (symbol-plist ',sym)) t)))))

(defaccesstest "setf-symbol-key-on-plist -- (setf ('SYM plist) v) updates in place"
  (setq easy-access-tests--qsym-setf-plist (list 'a 1 'b 2 'c 3))
  (eval '(setf ('b easy-access-tests--qsym-setf-plist) 99) nil)
  (should (equal 99 (plist-get easy-access-tests--qsym-setf-plist 'b)))
  (should (equal 1 (plist-get easy-access-tests--qsym-setf-plist 'a))))

(defaccesstest "setf-symbol-key-on-alist -- (setf ('SYM alist) v) mutates the pair"
  (setq easy-access-tests--qsym-setf-alist
        (list (cons 'name "Ali") (cons 'age 42)))
  (eval '(setf ('age easy-access-tests--qsym-setf-alist) 43) nil)
  (should (equal 43 (cdr (assq 'age easy-access-tests--qsym-setf-alist))))
  (should (equal "Ali"
                 (cdr (assq 'name easy-access-tests--qsym-setf-alist)))))

;;; Regression: `(quote X)' in DATA position (i.e. as a whole form, not
;;; as CAR of another form) must still be untouched.  This guards the
;;; walker against mistakenly treating quoted lists in value positions
;;; as accessor forms.

(deftest "quoted-data-passes-through -- quoted list in value position is data"
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

(defaccesstest "defcall-non-accessor-passes-through -- ordinary function calls untouched"
  (should (equal 3 (eval '(+ 1 2) t)))
  (should (equal '(+ 1 2) (easy-access-walk '(+ 1 2))))
  (should (equal '(car (list 1 2 3))
                 (easy-access-walk '(car (list 1 2 3))))))

(defaccesstest "defcall-custom-rule-get -- user-defined strings-as-regex"
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
                   (eval '("[0-9]+" "abc123def") t)))))

(defaccesstest "defcall-custom-rule-setf -- character CARs as getter/setter"
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
      (should (equal "replaced" (elt v 0))))))

(defaccesstest "defcall-most-recent-wins -- last registered rule fires"
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
      (should (equal 21 (eval '(7 nil) t))))))

(defaccesstest "defcall-redefinition-promotes -- re-eval upserts and promotes"
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
                                 :key #'easy-access-rule-name))))))

(defaccesstest "defcall-read-key -- :read-key canonicaliser reshapes HEAD"
  (easy-access-tests--with-rule bracketed-symbol
    (eval
     '(defcall bracketed-symbol (head target &optional value)
        :when (and (symbolp head) (not (keywordp head))
                   (not (null head)) (not (eq head t)))
        :read-key (lambda (form)
                    (unless (and (consp form)
                                 (eq (car form) 'bracket)
                                 (consp (cdr form))
                                 (null (cddr form))
                                 (symbolp (cadr form)))
                      (signal 'wrong-type-argument
                              (list 'bracketed-symbol-form form)))
                    (cadr form))
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
                   (eval '((bracket foo) (list 'foo 42 'bar 7)) t)))))

(defvar easy-access-tests--setting-log nil
  "Log fixture for `defcall-body-sees-setting-p'.")
(defvar easy-access-tests--setting-cell nil
  "Mutable cell fixture for `defcall-body-sees-setting-p'.")

(defaccesstest "defcall-body-sees-setting-p -- getter arm on read, setter arm on setf"
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
    (should (equal '(set get) easy-access-tests--setting-log))))

;;; -------------------------------------------------------------------------
;;; Group 14 -- Walker safety under `require'
;;; -------------------------------------------------------------------------
;;;
;;; Regression guard: the walker runs inside `load-read-function', which
;;; is invoked for every `require'.  If the walker itself used `cl-*' or
;;; `seq-*' helpers, a `require' of `cl-extra' with the mode on could
;;; recursively re-enter the walker and crash.  The walker path uses
;;; plain `while'/`dolist' only -- this test verifies the invariant.

(defaccesstest "walker-no-recursive-load -- requiring cl-extra under mode does not crash"
  (should (progn (require 'cl-extra) t)))

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

(deftest "negative-index-vector-read -- -1 is last element"
  (should (equal 'c (easy-access-lookup [a b c] -1)))
  (should (equal 'b (easy-access-lookup [a b c] -2)))
  (should (equal 'a (easy-access-lookup [a b c] -3))))

(deftest "negative-index-string-read -- -1 is last character"
  (should (equal ?c (easy-access-lookup "abc" -1)))
  (should (equal ?b (easy-access-lookup "abc" -2)))
  (should (equal ?a (easy-access-lookup "abc" -3))))

(deftest "negative-index-list-read -- same discipline as vectors and strings"
  (should (equal 'c (easy-access-lookup '(a b c) -1)))
  (should (equal 'b (easy-access-lookup '(a b c) -2)))
  (should (equal 'a (easy-access-lookup '(a b c) -3))))

(deftest "out-of-bounds-positive-returns-nil -- matching nth semantics"
  (should (null (easy-access-lookup '(a b c) 99)))
  (should (null (easy-access-lookup [a b c] 99)))
  (should (null (easy-access-lookup "abc" 99))))

(deftest "out-of-bounds-negative-returns-nil"
  (should (null (easy-access-lookup '(a b c) -99)))
  (should (null (easy-access-lookup [a b c] -99)))
  (should (null (easy-access-lookup "abc" -99))))

(defaccesstest "negative-index-e2e-eval -- bare negative-index accessors via eval"
  (should (equal 'c (eval '(-1 [a b c]) t)))
  (should (equal ?b (eval '(-2 "abc") t)))
  (should (equal 'c (eval '(-1 (list 'a 'b 'c)) t))))

(defaccesstest "out-of-bounds-e2e-eval -- returns nil rather than signalling"
  (should (null (eval '(99 [a b c]) t)))
  (should (null (eval '(99 "abc") t)))
  (should (null (eval '(99 (list 'a 'b 'c)) t))))

(defaccesstest "negative-index-setf-vector -- (setf (-1 v) x) writes to last slot"
  (setq easy-access-tests--neg-vec (vector 'a 'b 'c))
  (eval '(setf (-1 easy-access-tests--neg-vec) 'Z) nil)
  (should (equal [a b Z] easy-access-tests--neg-vec))
  (eval '(setf (-3 easy-access-tests--neg-vec) 'A) nil)
  (should (equal [A b Z] easy-access-tests--neg-vec)))

(defaccesstest "negative-index-setf-list -- (setf (-1 xs) x) writes to last cons"
  (setq easy-access-tests--neg-list (list 'a 'b 'c))
  (eval '(setf (-1 easy-access-tests--neg-list) 'Z) nil)
  (should (equal '(a b Z) easy-access-tests--neg-list))
  (eval '(setf (-2 easy-access-tests--neg-list) 'Y) nil)
  (should (equal '(a Y Z) easy-access-tests--neg-list)))

(defaccesstest "out-of-bounds-setf-still-errors -- writes past the end signal"
  (setq easy-access-tests--neg-vec (vector 'a 'b 'c))
  (should-error (eval '(setf (99 easy-access-tests--neg-vec) 'X) nil))
  (setq easy-access-tests--neg-list (list 'a 'b 'c))
  (should-error (eval '(setf (99 easy-access-tests--neg-list) 'X) nil)))

;;; -------------------------------------------------------------------------
;;; Group 16 -- String keys as accessors
;;; -------------------------------------------------------------------------
;;;
;;; The `strings-as-accessors' rule lets string literals in CAR position
;;; look up by `equal' association -- dispatching to `assoc' for alists
;;; and `gethash' for hash-tables.  This covers the common Emacs pattern
;;; `(cdr (assoc "Authorization" headers))'.

(deftest "string-key-alist-read -- string literal in CAR looks up alist"
  (let ((headers '(("Authorization" . "Bearer tok")
                   ("Content-Type" . "application/json"))))
    (should (equal "Bearer tok" (easy-access-lookup headers "Authorization")))
    (should (equal "application/json" (easy-access-lookup headers "Content-Type")))
    (should (null (easy-access-lookup headers "Missing")))))

(deftest "string-key-hash-read -- string literal in CAR looks up equal hash"
  (let ((h (make-hash-table :test 'equal)))
    (puthash "name" "Ali" h)
    (puthash "city" "Kufa" h)
    (should (equal "Ali" (easy-access-lookup h "name")))
    (should (null (easy-access-lookup h "missing")))))

(defvar easy-access-tests--str-alist nil "Fixture for string-key alist setf.")
(defvar easy-access-tests--str-hash nil "Fixture for string-key hash setf.")
(defvar easy-access-tests--str-plist nil "Fixture for string-key plist setf.")

(defaccesstest "string-key-alist-setf -- (setf (\"key\" alist) val) writes"
  (setq easy-access-tests--str-alist
        (list (cons "a" 1) (cons "b" 2)))
  (eval '(setf ("a" easy-access-tests--str-alist) 99) nil)
  (should (equal 99 (easy-access-lookup easy-access-tests--str-alist "a")))
  ;; New key -- nconc'd onto the end.
  (eval '(setf ("c" easy-access-tests--str-alist) 3) nil)
  (should (equal 3 (easy-access-lookup easy-access-tests--str-alist "c"))))

(defaccesstest "string-key-hash-setf -- (setf (\"key\" hash) val) writes"
  (setq easy-access-tests--str-hash (make-hash-table :test 'equal))
  (puthash "x" 10 easy-access-tests--str-hash)
  (eval '(setf ("x" easy-access-tests--str-hash) 20) nil)
  (should (equal 20 (gethash "x" easy-access-tests--str-hash)))
  (eval '(setf ("y" easy-access-tests--str-hash) 30) nil)
  (should (equal 30 (gethash "y" easy-access-tests--str-hash))))

(deftest "string-key-chained -- chained string-key lookups thread left-to-right"
  (let ((db '(("users" . (("admin" . "Ali")
                           ("guest" . "Hassan"))))))
    (should (equal '(("admin" . "Ali") ("guest" . "Hassan"))
                   (easy-access-lookup db "users")))
    (should (equal "Ali"
                   (easy-access-lookup
                    (easy-access-lookup db "users")
                    "admin")))))

(defvar easy-access-tests--str-db nil "Fixture for string-key chained e2e.")

(defaccesstest "string-key-chained-e2e -- chained string accessor forms via eval"
  (setq easy-access-tests--str-db
        '(("users" . (("admin" . "Ali")))))
  (should (equal "Ali"
                 (eval '("users" "admin" easy-access-tests--str-db) t))))

(deftest "string-key-plist-read -- string literal looks up a plist via equal"
  (let ((p '("hello" "world" "where" "here")))
    (should (equal "world" (easy-access-lookup p "hello")))
    (should (equal "here" (easy-access-lookup p "where")))
    (should (null (easy-access-lookup p "missing")))))

(defaccesstest "string-key-plist-e2e -- (\"hello\" plist) reaches eval"
  (should (equal "world"
                 (eval '("hello" '("hello" "world" "where" "here")) t))))

(defaccesstest "string-key-plist-setf -- (setf (\"key\" plist) val) writes"
  (setq easy-access-tests--str-plist (list "a" 1 "b" 2))
  (eval '(setf ("a" easy-access-tests--str-plist) 99) nil)
  (should (equal 99 (plist-get easy-access-tests--str-plist "a" #'equal)))
  (should (equal 2 (plist-get easy-access-tests--str-plist "b" #'equal))))

(deftest "string-key-plist-vs-alist -- alist heuristic still wins when CAR is cons"
  (let ((alist '(("hello" . "world") ("where" . "here")))
        (plist '("hello" "world" "where" "here")))
    ;; Alist: CAR is a cons cell — dispatches to assoc path.
    (should (equal "world" (easy-access-lookup alist "hello")))
    ;; Plist: CAR is a string atom — dispatches to plist-get path.
    (should (equal "world" (easy-access-lookup plist "hello")))))

(deftest "string-key-error-on-non-collection -- signals easy-access-invalid-key"
  (should-error (easy-access-lookup 42 "key")
                :type 'easy-access-invalid-key))

(deftest "string-key-walker-recognises-form -- (\"key\" obj) rewrites to lookup"
  (should (equal (easy-access-walk '("Auth" headers))
                 '(easy-access-lookup headers "Auth"))))

;;; -------------------------------------------------------------------------
;;; Group 17 -- Variable keys: the `easy-access' macro
;;; -------------------------------------------------------------------------
;;;
;;; Bare symbols in CAR position remain function calls -- the walker
;;; cannot distinguish `(var alist)' from `(some-fn arg)' at read-time.
;;; That is a deliberate design choice, not a bug.
;;;
;;; For the variable-key case we provide the `easy-access' macro:
;;; `(easy-access K0 K1 ... Kn COLL)' evaluates each Ki as an ordinary
;;; Elisp expression, then folds left-to-right over `easy-access-lookup'.
;;; The fold direction matches `easy-access--expand-accessor', so the
;;; explicit macro form agrees with the literal reader form when every
;;; key is literal.

(deftest "variable-keys-are-not-accessors -- bare symbols remain function calls"
  (should (equal (easy-access-walk '(foo bar))
                 '(foo bar)))
  (should (equal (easy-access-walk '(my-key my-obj))
                 '(my-key my-obj))))

(deftest "variable-key-via-lookup-directly -- call easy-access-lookup for variable keys"
  (let ((key :name)
        (plist (list :name "Ali" :age 42)))
    (should (equal "Ali" (easy-access-lookup plist key))))
  (let ((key "Authorization")
        (headers '(("Authorization" . "Bearer tok"))))
    (should (equal "Bearer tok" (easy-access-lookup headers key)))))

(deftest "easy-access-macro -- single key expands to a single lookup"
  (should (equal (macroexpand '(easy-access k obj))
                 '(easy-access-lookup obj k))))

(deftest "easy-access-macro -- multi-key fold is left-to-right"
  ;; `(K0 K1 K2 obj)' ==> leftmost key first, outermost last.  This is
  ;; the same fold as `easy-access--expand-accessor'.
  (should (equal (macroexpand '(easy-access :a :b :c obj))
                 '(easy-access-lookup
                    (easy-access-lookup
                      (easy-access-lookup obj :a)
                      :b)
                    :c))))

(deftest "easy-access-macro -- agrees with the walker on literal keys"
  ;; The whole point of the fold-direction choice: the explicit
  ;; macro must produce the same tree the walker produces.
  (should (equal (macroexpand '(easy-access :a :b obj))
                 (easy-access-walk '(:a :b obj)))))

(deftest "easy-access-macro -- runtime computed symbol key on a plist"
  (let ((kind 'b)
        (plist '(a ?A b ?B c ?C)))
    (should (equal ?B (easy-access (intern (symbol-name kind)) plist)))))

(deftest "easy-access-macro -- runtime string key on an alist uses equal"
  ;; The `strings-as-accessors' rule dispatches alist lookups via
  ;; `assoc' (i.e. `equal'), which is `string=' on strings -- exactly
  ;; what the stock `(alist-get KEY AL nil nil #'string=)' call site
  ;; wanted.
  (let ((key "button one")
        (alist '(("button one" . one-handler)
                 ("button two" . two-handler))))
    (should (eq 'one-handler (easy-access key alist)))))

(deftest "easy-access-macro -- runtime key on a hash-table"
  (let ((key 'zayd)
        (ht (make-hash-table :test #'eq)))
    (puthash 'zayd 12 ht)
    (puthash 'layla 34 ht)
    (should (equal 12 (easy-access key ht)))))

(deftest "easy-access-macro -- integer key on a vector"
  (let ((i 2)
        (v [:a :b :c :d]))
    (should (eq :c (easy-access i v)))))

(deftest "easy-access-macro -- chained runtime keys compose"
  (let ((outer 'user) (inner :name))
    (should (equal "Fatima"
                   (easy-access outer inner
                                '((user . (:name "Fatima" :age 30))
                                  (role . (:name "admin"))))))))

(deftest "easy-access-macro -- arity check rejects fewer than two args"
  (should-error (macroexpand '(easy-access only-one)))
  (should-error (macroexpand '(easy-access))))

(deftest "easy-access-macro -- setf through the macro writes the endpoint"
  ;; `gv-define-setter' is registered on `easy-access-lookup', so
  ;; `(setf (easy-access k obj) v)' descends into the outermost
  ;; `easy-access-lookup' and dispatches by type, just like the
  ;; literal reader form `(setf (:k obj) v)'.
  (let ((key :age)
        (p (list :name "Hussain" :age 10)))
    (setf (easy-access key p) 11)
    (should (equal 11 (plist-get p :age)))))

(deftest "easy-access-macro -- walker leaves the macro call alone"
  ;; The walker should not rewrite `(easy-access ...)' as an accessor
  ;; pattern -- the head is a bare symbol, not a key atom.  It IS free
  ;; to `macroexpand' the form (that's how `easy-access' actually
  ;; fires), and the macro's expansion is itself a `easy-access-lookup'
  ;; chain.  Either outcome is correct; we just assert the form is
  ;; NOT misread as `((quote easy-access) ...)'.
  (should-not (easy-access--accessor-form-p '(easy-access :a obj)))
  (should-not (easy-access--key-atom-p 'easy-access)))

;;; -------------------------------------------------------------------------
;;; Group 18 -- Walker special-form handling
;;; -------------------------------------------------------------------------
;;;
;;; The walker must skip data positions inside special forms that contain
;;; keywords, integers, and quoted symbols as match patterns or
;;; annotations rather than accessor keys.  Without explicit handling,
;;; the default "walk every sub-form" clause would rewrite these data
;;; positions into `easy-access-lookup' calls.

(deftest "walker-define-advice-name-spec -- keywords in name-spec untouched"
  (should
   (equal
    '(define-advice foo (:around (orig &rest args) my-advice)
       (easy-access-lookup bar :key))
    (easy-access-walk
     '(define-advice foo (:around (orig &rest args) my-advice)
        (:key bar))))))

(deftest "walker-pcase-integer-pattern -- integer patterns untouched"
  (should
   (equal
    '(pcase x
       (0 (easy-access-lookup items :first))
       (1 (easy-access-lookup items :second))
       (_ nil))
    (easy-access-walk
     '(pcase x
        (0 (:first items))
        (1 (:second items))
        (_ nil))))))

(deftest "walker-pcase-quoted-symbol-pattern -- match-equal patterns untouched"
  (should
   (equal
    '(pcase status
       ('todo "do it")
       ('done "finished"))
    (easy-access-walk
     '(pcase status
        ('todo "do it")
        ('done "finished"))))))

(deftest "walker-pcase-let -- binding patterns are data"
  (should
   (equal
    '(pcase-let ((`(,a ,b) (easy-access-lookup obj :pair)))
       (easy-access-lookup a :name))
    (easy-access-walk
     '(pcase-let ((`(,a ,b) (:pair obj)))
        (:name a))))))

(deftest "walker-pcase-let* -- same treatment as pcase-let"
  (should
   (equal
    '(pcase-let* ((`(,h ,m) (easy-access-lookup times :pair))
                  (`(,x ,y) (easy-access-lookup coords :pos)))
       (list h m x y))
    (easy-access-walk
     '(pcase-let* ((`(,h ,m) (:pair times))
                   (`(,x ,y) (:pos coords)))
        (list h m x y))))))

(deftest "walker-pcase-expr-is-walked -- discriminant EXPR is code"
  (should
   (equal
    '(pcase (easy-access-lookup obj :status)
       ('active t)
       (_ nil))
    (easy-access-walk
     '(pcase (:status obj)
        ('active t)
        (_ nil))))))

(deftest "walker-cl-defmethod -- arglists with type specializers untouched"
  (should
   (equal
    '(cl-defmethod my-method ((x my-struct))
       (easy-access-lookup x :name))
    (easy-access-walk
     '(cl-defmethod my-method ((x my-struct))
        (:name x))))))

(deftest "walker-cl-defmethod-qualifier -- :around keyword untouched"
  (should
   (equal
    '(cl-defmethod my-method :around ((x my-struct))
       (easy-access-lookup x :name))
    (easy-access-walk
     '(cl-defmethod my-method :around ((x my-struct))
        (:name x))))))

(deftest "walker-cl-case-patterns -- match keys are data"
  (should
   (equal
    '(cl-case (easy-access-lookup obj :type)
       (4 "four")
       (:otherwise "default"))
    (easy-access-walk
     '(cl-case (:type obj)
        (4 "four")
        (:otherwise "default"))))))

(deftest "walker-cond-not-false-positive -- cond clauses are not accessor forms"
  (should
   (equal
    '(cond
      ((and very-stale large-change) "abandon")
      ((<= total 10) 1)
      ((work-item-urgent it) (push it urgent))
      (:otherwise "default")
      (t nil))
    (easy-access-walk
     '(cond
        ((and very-stale large-change) "abandon")
        ((<= total 10) 1)
        ((work-item-urgent it) (push it urgent))
        (:otherwise "default")
        (t nil))))))

;;;; Group 19 -- GV macro integration (cl-incf, push, cl-decf on accessor places)

(defvar easy-access-tests--gv-alist nil
  "Scratch alist for GV macro tests -- must be dynamic for `eval'.")

(defaccesstest "cl-incf-on-keyword-accessor -- increments the value"
  (setq easy-access-tests--gv-alist '((:minutes . 5)))
  (eval '(cl-incf (:minutes easy-access-tests--gv-alist)) nil)
  (should (equal 6 (cdr (assq :minutes easy-access-tests--gv-alist)))))

(defaccesstest "cl-incf-on-keyword-accessor-with-delta -- explicit delta"
  (setq easy-access-tests--gv-alist '((:minutes . 10)))
  (eval '(cl-incf (:minutes easy-access-tests--gv-alist) 5) nil)
  (should (equal 15 (cdr (assq :minutes easy-access-tests--gv-alist)))))

(defaccesstest "cl-incf-on-quoted-symbol-accessor"
  (setq easy-access-tests--gv-alist '((person . 10)))
  (eval '(cl-incf ('person easy-access-tests--gv-alist)) nil)
  (should (equal 11 (cdr (assoc 'person easy-access-tests--gv-alist)))))

(defaccesstest "cl-decf-on-keyword-accessor -- decrements the value"
  (setq easy-access-tests--gv-alist '((:count . 10)))
  (eval '(cl-decf (:count easy-access-tests--gv-alist)) nil)
  (should (equal 9 (cdr (assq :count easy-access-tests--gv-alist)))))

(defaccesstest "push-onto-keyword-accessor -- prepends to the list"
  (setq easy-access-tests--gv-alist '((:entries "old")))
  (eval '(push "new" (:entries easy-access-tests--gv-alist)) nil)
  (should (equal '("new" "old")
                 (cdr (assq :entries easy-access-tests--gv-alist)))))

(defaccesstest "push-onto-quoted-symbol-accessor"
  (setq easy-access-tests--gv-alist '((items 1 2)))
  (eval '(push 0 ('items easy-access-tests--gv-alist)) nil)
  (should (equal '(0 1 2)
                 (cdr (assoc 'items easy-access-tests--gv-alist)))))

(defaccesstest "cl-incf-setf-then-incf -- setf to create, then incf twice"
  (setq easy-access-tests--gv-alist '((person . 10)))
  (should (equal 10 (eval '('person easy-access-tests--gv-alist) nil)))
  (eval '(cl-incf ('person easy-access-tests--gv-alist)) nil)
  (should (equal 11 (eval '('person easy-access-tests--gv-alist) nil)))
  (eval '(cl-incf ('person easy-access-tests--gv-alist)) nil)
  (should (equal 12 (eval '('person easy-access-tests--gv-alist) nil))))

(defaccesstest "function-ref-still-works -- #'accessor remains callable"
  (let* ((p (make-easy-access-tests--point :x 3 :y 7 :label "hi"))
         (getter #'easy-access-tests--point-x))
    ;; #'accessor works with funcall
    (should (equal 3 (funcall getter p)))
    ;; #'accessor works with mapcar
    (let ((ps (list p p)))
      (should (equal '(3 3) (mapcar #'easy-access-tests--point-x ps))))
    ;; (:x p) still works as accessor syntax
    (should (equal 3 (eval `(:x ,p) nil)))))

;;;; Group 20 -- threading macro interop

(defvar easy-access-tests--thread-obj nil
  "Scratch variable for threading tests.")

(defaccesstest "thread-first-keyword-no-parens -- keywords thread without parens"
  (setq easy-access-tests--thread-obj
        '(:work "Tim Hortons" :owner (:name "Jasim" :address "Canada")))
  (should (equal "Jasim"
                 (eval '(thread-first easy-access-tests--thread-obj
                          :owner :name) nil))))

(defaccesstest "thread-first-quoted-symbol-needs-parens -- quoted symbols require parens"
  (setq easy-access-tests--thread-obj
        '((work . "Tim Hortons")
          (owner . ((name . "Jasim") (address . "Canada")))))
  (should (equal "Jasim"
                 (eval '(thread-first easy-access-tests--thread-obj
                          ('owner) ('name)) nil))))

(defaccesstest "thread-first-string-no-parens -- string keys thread without parens"
  (setq easy-access-tests--thread-obj '(("x" . "y")))
  (should (equal "y"
                 (eval '(thread-first easy-access-tests--thread-obj
                          "x") nil))))

(defaccesstest "thread-first-integer-no-parens -- integer keys thread without parens"
  (setq easy-access-tests--thread-obj '(a b c))
  (should (equal 'b
                 (eval '(thread-first easy-access-tests--thread-obj
                          1) nil))))

(defaccesstest "push-sequential-builds-list -- two pushes accumulate correctly"
  (setq easy-access-tests--gv-alist '((person)))
  (eval '(push 'me ('person easy-access-tests--gv-alist)) nil)
  (should (equal '(me) (eval '('person easy-access-tests--gv-alist) nil)))
  (eval '(push 'you ('person easy-access-tests--gv-alist)) nil)
  (should (equal '(you me) (eval '('person easy-access-tests--gv-alist) nil))))

;;; -------------------------------------------------------------------------
;;; Group 19 -- `defcall-rewrite' mechanism and cl-* coverage
;;; -------------------------------------------------------------------------
;;;
;;; `defcall-rewrite' is the user-extension point for walker behaviour
;;; on compound forms: it lifts the per-macro cond-branch handlers out
;;; of `easy-access--walk-compound' and into a registry that third-party
;;; code can extend.  These tests cover three concerns:
;;;
;;;   1. The mechanism itself -- upsert/remove semantics, validator
;;;      errors, dispatch correctness.
;;;   2. Built-in coverage for the cl-* family: `cl-defun',
;;;      `cl-defmacro', `cl-defsubst', `cl-letf', `cl-letf*',
;;;      `cl-flet', `cl-labels', `cl-macrolet', `cl-symbol-macrolet',
;;;      `iter-defun'.  Each tests that body forms get walked while
;;;      data positions (arglists, patterns, places) do NOT.
;;;   3. Golden-output snapshots for the four migrated handlers --
;;;      `define-advice', `cl-defstruct', `lambda', and the
;;;      `defun' family -- asserting byte-equal output with the old
;;;      hand-coded cond branches.

(defun easy-access-tests--unregister-rewrite (name)
  "Drop any rewrite with NAME from `easy-access--rewrites'."
  (setq easy-access--rewrites
        (cl-remove-if (lambda (r) (eq (easy-access-rewrite-name r) name))
                      easy-access--rewrites)))

(defmacro easy-access-tests--with-rewrite (name &rest body)
  "Evaluate BODY; always unregister rewrite NAME afterward.
Used to keep the registry pristine across tests even when the
body errors out mid-way."
  (declare (indent 1))
  `(unwind-protect (progn ,@body)
     (easy-access-tests--unregister-rewrite ',name)))

;;; --- Mechanism -------------------------------------------------------------

(deftest "defcall-rewrite-registry-upsert -- same name replaces in place"
  (easy-access-tests--with-rewrite test-upsert
    (let ((before (length easy-access--rewrites)))
      (eval '(defcall-rewrite test-upsert
               :source (toy-macro x . body)
               :target (toy-macro x (rewrite-each! body))))
      (should (= (1+ before) (length easy-access--rewrites)))
      ;; Re-define with a different body; count must not grow.
      (eval '(defcall-rewrite test-upsert
               :source (toy-macro x . body)
               :target (toy-macro x (rewrite-each! body))))
      (should (= (1+ before) (length easy-access--rewrites))))))

(deftest "defcall-rewrite-remove -- returns non-nil when a rewrite was removed"
  (easy-access-tests--with-rewrite test-remove
    (eval '(defcall-rewrite test-remove
             :source (toy-macro x . body)
             :target (toy-macro x (rewrite-each! body))))
    (should (easy-access-remove-rewrite 'test-remove))
    ;; Removing again returns nil.
    (should-not (easy-access-remove-rewrite 'test-remove))))

(deftest "defcall-rewrite-validator -- unbound target symbol raises at expand time"
  (should-error
   (macroexpand
    '(defcall-rewrite test-validator
       :source (toy-macro x . body)
       :target (toy-macro x (rewrite-each! boody))))  ; typo: boody
   :type 'error))

(deftest "defcall-rewrite-dispatch -- user rewrite fires through the walker"
  (easy-access-tests--with-rewrite test-dispatch
    (eval '(defcall-rewrite test-dispatch
             :source (my-dsl x . body)
             :target (my-dsl x (rewrite-each! body))))
    (should
     (equal
      '(my-dsl 42 (easy-access-lookup p :a) (easy-access-lookup q :b))
      (easy-access-walk '(my-dsl 42 (:a p) (:b q)))))))

(deftest "defcall-rewrite-dotted-tail -- rest-capture via dotted syntax"
  (easy-access-tests--with-rewrite test-trailing
    ;; Two positional binders, rest captured via dotted tail.
    (eval '(defcall-rewrite test-trailing
             :source (my-form a b . rest)
             :target (my-form a b . rest)))
    (should (equal '(my-form 1 2 3 4 5)
                   (easy-access-walk '(my-form 1 2 3 4 5))))
    (should (equal '(my-form 1 2)
                   (easy-access-walk '(my-form 1 2))))))

(deftest "defcall-rewrite-positional-binder -- proper-list element is positional"
  (easy-access-tests--with-rewrite test-positional
    ;; Proper-list binders bind strictly one element each -- no
    ;; ambiguity with rest-capture.
    (eval '(defcall-rewrite test-positional
             :source (my-wrap expr)
             :target (my-wrap (rewrite! expr))))
    (should (equal '(my-wrap (easy-access-lookup x :k))
                   (easy-access-walk '(my-wrap (:k x)))))))

;;; --- cl-* coverage ---------------------------------------------------------

(deftest "walker-cl-defun -- arglist untouched, body walked"
  (should
   (equal
    '(cl-defun my-fn (x &optional y)
       (easy-access-lookup x :name))
    (easy-access-walk
     '(cl-defun my-fn (x &optional y)
        (:name x))))))

(deftest "walker-cl-defmacro -- arglist untouched, body walked"
  ;; Body is a plain accessor form (not inside a backquote template),
  ;; so the walker rewrites it.
  (should
   (equal
    '(cl-defmacro my-macro (x &rest args)
       (easy-access-lookup x :name))
    (easy-access-walk
     '(cl-defmacro my-macro (x &rest args)
        (:name x))))))

(deftest "walker-cl-defsubst -- rewrite fires before macroexpansion"
  (should
   (equal
    '(cl-defsubst my-sub (x)
       (easy-access-lookup x :name))
    (easy-access-walk
     '(cl-defsubst my-sub (x)
        (:name x))))))

(deftest "walker-cl-letf -- PLACE untouched, VALUE and body walked"
  (should
   (equal
    '(cl-letf (((symbol-function 'foo) (easy-access-lookup h :handler)))
       (easy-access-lookup obj :result))
    (easy-access-walk
     '(cl-letf (((symbol-function 'foo) (:handler h)))
        (:result obj))))))

(deftest "walker-cl-letf* -- same treatment as cl-letf"
  (should
   (equal
    '(cl-letf* (((symbol-function 'foo) (easy-access-lookup h :handler)))
       (easy-access-lookup obj :result))
    (easy-access-walk
     '(cl-letf* (((symbol-function 'foo) (:handler h)))
        (:result obj))))))

(deftest "walker-iter-defun -- body walked"
  (should
   (equal
    '(iter-defun gen (x)
       (iter-yield (easy-access-lookup x :name)))
    (easy-access-walk
     '(iter-defun gen (x)
        (iter-yield (:name x)))))))

;;; --- Golden-output snapshots for the migrated handlers --------------------
;;;
;;; Each snapshot was captured by running the hand-coded cond branch
;;; immediately before deleting it.  If a future edit to the DSL
;;; produces byte-divergent output, these fire.

(deftest "golden-define-advice -- declarative matches hand-coded output"
  (should
   (equal
    '(define-advice foo (:around (orig &rest args) my-advice)
       (easy-access-lookup bar :key)
       (easy-access-lookup baz :other))
    (easy-access-walk
     '(define-advice foo (:around (orig &rest args) my-advice)
        (:key bar)
        (:other baz))))))

(deftest "golden-cl-defstruct -- slot declarations stay verbatim"
  (should
   (equal
    '(cl-defstruct my-type
       (slot1 :default)              ; keyword-looking default untouched
       (slot2 (:fn obj))              ; accessor-shaped default untouched
       slot3)
    (easy-access-walk
     '(cl-defstruct my-type
        (slot1 :default)
        (slot2 (:fn obj))
        slot3)))))

(deftest "golden-lambda -- arglist untouched, body walked"
  (should
   (equal
    '(lambda (x &optional y &rest rest)
       (easy-access-lookup x :name)
       (easy-access-lookup rest 0))
    (easy-access-walk
     '(lambda (x &optional y &rest rest)
        (:name x)
        (0 rest))))))

(deftest "golden-defun-family -- defun/defmacro/defsubst match hand-coded"
  (should
   (equal
    '(defun f (x)
       (easy-access-lookup x :name))
    (easy-access-walk
     '(defun f (x) (:name x)))))
  (should
   (equal
    '(defmacro m (x)
       (easy-access-lookup x :name))
    (easy-access-walk
     '(defmacro m (x) (:name x)))))
  (should
   (equal
    '(defsubst s (x)
       (easy-access-lookup x :name))
    (easy-access-walk
     '(defsubst s (x) (:name x))))))

(deftest "golden-pcase-dolist -- PAT untouched, EXPR and body walked"
  (should
   (equal
    '(pcase-dolist (`(,a ,b) (easy-access-lookup obj :pairs))
       (easy-access-lookup a :name))
    (easy-access-walk
     '(pcase-dolist (`(,a ,b) (:pairs obj))
        (:name a))))))

(deftest "golden-pcase-lambda -- pattern list untouched, body walked"
  (should
   (equal
    '(pcase-lambda (`(,a ,b))
       (easy-access-lookup a :name))
    (easy-access-walk
     '(pcase-lambda (`(,a ,b))
        (:name a))))))

(provide 'easy-access-tests)
;;; easy-access-tests.el ends here
