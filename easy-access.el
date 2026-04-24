;;; easy-access.el --- Clojure-style keyword/integer accessors  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy
;; Version: 0.5.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools, extensions

;;; Commentary:
;;
;; Extend Emacs Lisp with Clojure-style accessor syntax.  Concretely, after
;; enabling `easy-access-mode' the following forms become legal Elisp:
;;
;;     (:key plist)       ==>  (plist-get plist :key)
;;     (N seq)            ==>  (nth N seq)
;;     (:field instance)  ==>  ({STRUCT}-{FIELD} instance) for cl-defstruct
;;     (:f :g N obj)      ==>  threaded lookups, left-to-right path
;;
;; The extension is implemented via a read-time code walker installed on
;; `load-read-function' and the interactive-eval machinery
;; (`elisp--preceding-sexp', `eval', `eval-region').  No evaluator patches
;; required -- we rewrite forms into standard Elisp before they reach the
;; evaluator.
;;
;; Enable with:
;;
;;     (require 'easy-access)
;;     (easy-access-mode 1)
;;
;; Disable with:
;;
;;     (easy-access-mode -1)
;;
;; Rationale -- why a code walker instead of custom macros?
;; --------------------------------------------------------
;; Elisp's evaluator rejects keywords and integers in function position.
;; `(defmacro :foo ...)' works as a per-keyword opt-in but does not
;; generalise: you cannot catch every possible keyword or every integer.
;; Advising `eval' catches only top-level eval calls -- sub-forms reached
;; by the C evaluator during `let', `if', function application, etc.,
;; bypass any Lisp-level advice.
;;
;; The only general solution is to rewrite the forms BEFORE the C
;; evaluator sees them.  `load-read-function' is a built-in hook that
;; `load' and `eval-region' use to read source forms; wrapping it lets
;; us walk every form from disk before evaluation.  For interactive
;; commands (`C-x C-e', `C-M-x', `M-:') we advise their read machinery.
;;
;; Safety argument -- purely additive semantics:
;; ---------------------------------------------
;; Before this package, a form like `(:foo p)' signals `void-function :foo'
;; -- it is an error in stock Elisp.  The walker turns a previously-erroring
;; form into a legal accessor call.  No form that currently works changes
;; meaning.  The walker carefully skips quoted sub-forms, binding positions,
;; arg lists, and other non-code positions -- so data keywords in
;; `(:filter x)' plist literals and the like are untouched.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'cl-macs))  ; for setf on cl-struct-slot-value

;;; -------------------------------------------------------------------------
;;; Customisation
;;; -------------------------------------------------------------------------

(defgroup easy-access nil
  "Clojure-style keyword and integer accessors for Emacs Lisp."
  :group 'lisp
  :prefix "easy-access-")

(defcustom easy-access-lighter " :ea"
  "Mode-line lighter displayed when `easy-access-mode' is enabled."
  :type 'string
  :group 'easy-access)

;;; -------------------------------------------------------------------------
;;; Errors
;;; -------------------------------------------------------------------------

(define-error 'easy-access-invalid-key
  "easy-access: no dispatch for KEY on OBJ")

;;; -------------------------------------------------------------------------
;;; Rule registry -- `defcall' as `defgeneric' for function application
;;; -------------------------------------------------------------------------
;;;
;;; Every CAR shape that is treated as an accessor (keyword, integer,
;;; quoted-symbol, and any future user-defined shape) lives as a single
;;; `easy-access-rule' record in `easy-access--rules'.  Each rule declares:
;;;
;;;   * `guard-fn'  -- (lambda (HEAD) ...) non-nil iff this rule fires
;;;   * `read-key'  -- optional (lambda (HEAD) ...) canonicalising HEAD
;;;                    before it is bound in the body (e.g. stripping
;;;                    `(quote SYM)' to `SYM')
;;;   * `get-fn'    -- (lambda (HEAD TARGET) ...) called on every read
;;;   * `set-fn'    -- (lambda (HEAD TARGET VALUE) ...) called on every
;;;                    `setf' write
;;;
;;; Inside a rule body, `(easy-access-setting-p)' returns non-nil when
;;; invoked as a setter and nil when invoked as a getter -- this is how
;;; one body can carry both read and write logic, branching on VALUE's
;;; presence rather than its value (nil is a legal value to set).
;;;
;;; Resolution -- most-recently-defined matching rule wins.  Rules are
;;; pushed to the front of `easy-access--rules'; redefinition by NAME
;;; upserts in place *and* promotes the rule to the front.  This mirrors
;;; `defun' / `setf symbol-function' redefinition: the newest definition
;;; takes precedence.

(cl-defstruct easy-access-rule
  "A declared extension of Elisp function application.
Each rule governs one CAR shape.  See `defcall' for the surface
syntax and `easy-access-lookup' for the runtime dispatch."
  name docstring read-key guard-fn get-fn set-fn)

(defvar easy-access--rules nil
  "Registry of `easy-access-rule' records.
Most-recently-defined rule sits at the front; iteration returns
the first matching guard.  Populated by `defcall'.")

(defconst easy-access--setting-sentinel
  (make-symbol "easy-access--not-setting")
  "Sentinel value bound to `value' when a rule body is invoked as a getter.
Distinct from any value a user might legitimately pass to `setf'
-- `nil', `0', `'()' are all legal SET values, but none of them
is `eq' to this uninterned symbol.

The macro-generated getter binds the user's VALUE variable to
this sentinel; the setter binds it to the user's intended value.
Body code reads VALUE only after gating on `easy-access-setting-p'.")

(defvar easy-access--call-mode nil
  "Lexically bound by `defcall'-generated closures to `get' or `set'.
`easy-access-setting-p' reads this to distinguish getter from
setter invocations of the same rule body.  Never set globally --
only via the `let' inside each generated closure.")

(defmacro easy-access-setting-p ()
  "Return non-nil if the enclosing `defcall' body is being run as a setter.
Use this inside a rule body to branch between read and write
logic -- read when nil, write when non-nil:

    (cond ((listp target)
           (if (easy-access-setting-p)
               (aset target head value)
             (nth head target))))

The value variable (named via the third element of the `defcall'
arglist, default `value') is bound to the intended value only
when this returns non-nil; it holds the sentinel otherwise, so
never consult it directly -- always gate on `easy-access-setting-p'.

Works by checking a hidden lexical binding of
`easy-access--call-mode', which the `defcall' macro sets to
`'get' in the getter closure and `'set' in the setter closure."
  '(eq easy-access--call-mode 'set))

(defun easy-access--upsert-rule (rule)
  "Insert RULE at the front of `easy-access--rules', replacing any by same NAME.
Re-evaluating a `defcall' form promotes the rule to highest
precedence (most recent wins) -- matches the user's editing
workflow where re-evaluating means \"use my new definition.\""
  (let ((name (easy-access-rule-name rule))
        (acc nil)
        (rest easy-access--rules))
    (while rest
      (unless (eq (easy-access-rule-name (car rest)) name)
        (setq acc (cons (car rest) acc)))
      (setq rest (cdr rest)))
    (setq easy-access--rules (cons rule (nreverse acc))))
  rule)

(defun easy-access--matching-rule (head)
  "Return the first rule whose `guard-fn' matches HEAD, or nil.
HEAD is tested as-is -- runtime callers (`easy-access-lookup')
pass already-canonical keys, so guards match against canonical
values.  Walker surface matching goes through
`easy-access--surface-matching-rule' instead.

Plain `while' -- no `cl-*' / `seq-*' on the walker path, because
the walker may be triggered by `require' of `cl-extra' and a `cl-'
call here would cause a recursive-load crash."
  (let ((rules easy-access--rules) match)
    (while (and rules (not match))
      (when (funcall (easy-access-rule-guard-fn (car rules)) head)
        (setq match (car rules)))
      (setq rules (cdr rules)))
    match))

(defun easy-access--surface-matching-rule (head)
  "Return (RULE . CANONICAL-KEY) for HEAD, or nil.
Walks `easy-access--rules' in order.  For each rule, applies
`:read-key' to HEAD (protected by `condition-case' -- if reading
fails, the rule doesn't match), then tests `:when' against the
canonical key.  The first rule that successfully matches wins.

Used by the walker to decide whether a CAR is an accessor key;
runtime callers use `easy-access--matching-rule' with the
already-canonical key.  Plain `while' loop -- no `cl-*'."
  (let ((rules easy-access--rules) match)
    (while (and rules (not match))
      (let* ((rule (car rules))
             (read-key (easy-access-rule-read-key rule))
             (canonical (condition-case nil
                            (if read-key (funcall read-key head) head)
                          (error 'easy-access--read-key-failed))))
        (unless (eq canonical 'easy-access--read-key-failed)
          (when (funcall (easy-access-rule-guard-fn rule) canonical)
            (setq match (cons rule canonical))))
        (setq rules (cdr rules))))
    match))

(defmacro defcall (name arglist &rest spec)
  "Declare NAME as an accessor rule -- an extension of function application.
ARGLIST is of the form `(HEAD TARGET &optional VALUE)' -- the
method's formal parameters, familiar from `cl-defmethod'.

SPEC is, in order:

  :when GUARD-EXPR       -- an Elisp expression over HEAD; non-nil
                            means this rule fires for that CAR shape.
  [:read-key FN]         -- optional; FN is applied to HEAD before
                            it is bound in BODY.  Useful when the
                            surface syntax carries a wrapper, e.g.
                            `(quote SYM)' unwrapped to `SYM' via
                            `cadr'.
  DOCSTRING              -- optional string.
  BODY                   -- ordinary Elisp, typically a `cond'
                            dispatching on TARGET's shape.

Every rule handles BOTH reads and writes through the same body.
Inside BODY, `(easy-access-setting-p)' returns non-nil when the
current invocation is a setter -- gate on that, not on `value'
itself, because nil is a legal value to set.

Why a rule owns its own setter rather than deferring to `setf':
`setf's setter-expanders are chosen at compile time from a static
symbol name, so they cannot look at the runtime type of TARGET
to pick `plist-put' vs. `puthash' vs. `setcdr'.  The walker
rewrites `(:a p)' into a single `(easy-access-lookup p :a)' call
that must dispatch at runtime -- and since we do not know in
advance what target shapes a user's rule will care about, the
setter logic lives with the rule itself.

Rules are inserted at the front of `easy-access--rules'; when
multiple rules' guards match the same HEAD, the most-recently-
defined one wins.  Redefining a rule by the same NAME upserts in
place and promotes to the front.

Example:

  (defcall integers-as-accessors (head target &optional value)
    :when (integerp head)
    \"Integers in CAR position index into sequences; setf-able.\"
    (cond ((listp target)
           (if (easy-access-setting-p)
               (setcar (nthcdr head target) value)
             (nth head target)))
          ((vectorp target)
           (if (easy-access-setting-p)
               (aset target head value)
             (elt target head)))))

\(fn NAME (HEAD TARGET &optional VALUE) :when GUARD-EXPR [:read-key FN] [DOCSTRING] &rest BODY)"
  (declare (indent 2) (doc-string 4))
  (let* ((head-sym (nth 0 arglist))
         (target-sym (nth 1 arglist))
         ;; User writes `(head target &optional value)'; pick out the
         ;; value symbol past the `&optional' marker.  Default to
         ;; `value' when the third slot is omitted entirely.
         (value-sym (let ((tail (cdddr arglist)))
                      (cond
                       ((null tail) 'value)
                       ((eq (car tail) '&optional)
                        (or (cadr tail) 'value))
                       (t (car tail)))))
         ;; Parse leading keyword options.
         (guard-expr nil)
         (read-key-fn nil))
    (while (and spec (keywordp (car spec)))
      (let ((k (pop spec))
            (v (pop spec)))
        (pcase k
          (:when (setq guard-expr v))
          (:read-key (setq read-key-fn v))
          (_ (error "defcall: unknown option %S" k)))))
    (unless guard-expr
      (error "defcall: missing :when GUARD-EXPR"))
    ;; Optional docstring -- only consume as docstring if BODY follows.
    (let ((docstring (when (and (stringp (car spec)) (cdr spec))
                       (pop spec))))
      `(easy-access--upsert-rule
        (make-easy-access-rule
         :name     ',name
         :docstring ,docstring
         :read-key ,(and read-key-fn `(function ,read-key-fn))
         :guard-fn (lambda (,head-sym) ,guard-expr)
         ;; Getter: bind `value' to the sentinel so any accidental read
         ;; errors out on type; set `easy-access--call-mode' to `get' so
         ;; the `easy-access-setting-p' macro returns nil.
         :get-fn   (lambda (,head-sym ,target-sym)
                     (let ((easy-access--call-mode 'get)
                           (,value-sym easy-access--setting-sentinel))
                       (ignore ,value-sym)
                       ,@spec))
         ;; Setter: bind `value' to the user's intended value; set
         ;; `easy-access--call-mode' to `set' so `easy-access-setting-p'
         ;; returns non-nil.
         :set-fn   (lambda (,head-sym ,target-sym ,value-sym)
                     (let ((easy-access--call-mode 'set))
                       ,@spec)))))))

;;; -------------------------------------------------------------------------
;;; Runtime dispatch
;;; -------------------------------------------------------------------------

(defvar easy-access--struct-accessor-cache (make-hash-table :test 'equal)
  "Memoisation table for struct-slot accessor lookup.
Keys are cons cells (STRUCT-TYPE . :FIELD), values are the
generated accessor function symbols, or nil if no such slot exists.")

(defun easy-access--struct-accessor (struct-type key)
  "Return the accessor function for KEY on STRUCT-TYPE, or nil.
STRUCT-TYPE is the symbol naming a `cl-defstruct' type; KEY is
either a keyword (`:jira' for slot `jira') or a plain symbol
(`jira' for slot `jira').  Results are memoised in
`easy-access--struct-accessor-cache'."
  (let ((cache-key (cons struct-type key)))
    (if-let ((cached (gethash cache-key easy-access--struct-accessor-cache 'miss)))
        (if (eq cached 'miss)
            ;; Compute and cache.  Strip the leading colon only for
            ;; keywords; plain symbols name the slot directly.
            (let* ((slot-name (if (keywordp key)
                                  (intern (substring (symbol-name key) 1))
                                key))
                   (slots (cl-struct-slot-info struct-type))
                   (has-slot (cl-some (lambda (slot) (eq (car slot) slot-name))
                                      slots))
                   (accessor (and has-slot
                                  (intern (format "%s-%s"
                                                  struct-type
                                                  slot-name)))))
              (puthash cache-key (or accessor nil)
                       easy-access--struct-accessor-cache)
              accessor)
          cached)
      nil)))

(defun easy-access--struct-get (target key)
  "Read KEY (keyword or plain symbol) from struct TARGET.
TARGET is a record; its type symbol sits at `(aref TARGET 0)'.
Routes through the memoised `easy-access--struct-accessor'."
  (let ((acc (easy-access--struct-accessor (aref target 0) key)))
    (when acc (funcall acc target))))

(defun easy-access--struct-set (target key value)
  "Write VALUE at KEY (keyword or plain symbol) of struct TARGET.
Uses `cl-struct-slot-value' as an `setf'-able place."
  (let ((slot-name (if (keywordp key)
                       (intern (substring (symbol-name key) 1))
                     key)))
    (setf (cl-struct-slot-value (aref target 0) slot-name target) value)))

(defun easy-access--plist-set (target key value)
  "Write VALUE at KEY in plist TARGET in place (where possible).
`plist-put' mutates in place when KEY is present; for a new key
the outer place must be re-bound -- that responsibility sits with
the surrounding `setf' machinery, not here."
  (plist-put target key value)
  value)

(defun easy-access--alist-set (target key value)
  "Write VALUE at KEY in alist TARGET.
Mutates the existing pair's cdr when present, otherwise nconcs a
new pair on.  Non-empty alists stay consistent with the outer
`setf' place; a fresh empty alist needs the variable rebound,
which the gv-setter does not do -- documented sharp edge."
  (let ((pair (assq key target)))
    (if pair
        (setcdr pair value)
      (nconc target (list (cons key value))))
    value))

(defun easy-access--list-index-set (target index value)
  "Write VALUE at INDEX in list TARGET via `setcar' on `nthcdr'.
Signals an error if INDEX is out of range -- that is the normal
`setcar' behaviour on a too-short list."
  (setcar (nthcdr index target) value)
  value)

(defun easy-access--alistp (obj)
  "Return non-nil if OBJ looks like an alist.
Heuristic: a non-empty proper list whose first element is a cons
cell (not itself a proper list of keyword/value pairs).  This is
how we distinguish alists like `((:a . 1) (:b . 2))' from plists
like `(:a 1 :b 2)' at runtime."
  (and (consp obj)
       (consp (car obj))
       ;; CAR is a cons whose CDR is NOT a list -- i.e. a proper dotted
       ;; pair like (:a . 1), not a sub-list like (:a 1 :b 2).  Both
       ;; (assq K '((:a . 1))) and (assq K '((:a 1))) work, so we accept
       ;; either shape.
       t))

(defun easy-access-lookup (obj key)
  "Return OBJ's element identified by KEY.
Dispatches through `easy-access--rules' -- the first rule whose
guard matches KEY owns the lookup.  KEY is the *canonical* key
value (already stripped of any surface-syntax wrapper by the
walker), so every rule's guard tests canonical shape.

Per-target dispatch happens inside the rule body (typically a
`cond' on OBJ's shape), not at this framework level.

Signals `easy-access-invalid-key' if no rule's guard matches KEY.
This function is the runtime target of forms rewritten by
`easy-access-walk'.  Users rarely call it directly."
  (let ((rule (easy-access--matching-rule key)))
    (unless rule (signal 'easy-access-invalid-key (list key obj)))
    (funcall (easy-access-rule-get-fn rule) key obj)))

;;; -------------------------------------------------------------------------
;;; Explicit macro form -- variable keys
;;; -------------------------------------------------------------------------
;;;
;;; The read-time walker is deliberately literal-only: it rewrites
;;; `(:a :b obj)' and `('name obj)' at read time, before bindings exist,
;;; so a variable or computed expression in head position stays a plain
;;; function call.  That's by design -- the walker has no way to know
;;; that `x' in `(x obj)' is intended as a key rather than a function.
;;;
;;; For the variable-key case we offer `easy-access' as an explicit
;;; macro.  `(easy-access K0 K1 ... Kn COLL)' is a left-fold over
;;; `easy-access-lookup', with each Ki treated as an ordinary Elisp
;;; expression -- so `(intern ...)', `(match-string ...)', and plain
;;; variables all Just Work, dispatching through the same rules
;;; registry as the walker.
;;;
;;; The shape mirrors `easy-access--expand-accessor' (the walker's
;;; fold), so `(easy-access 'owner 'name x)' agrees exactly with the
;;; literal form `('owner 'name x)': left-to-right, outermost key
;;; last.

(defmacro easy-access (&rest args)
  "Left-to-right accessor chain whose keys are ordinary expressions.
ARGS is (K0 K1 ... Kn COLLECTION) with at least one key and a
collection.  Each Ki is evaluated as a normal Elisp expression --
unlike the read-time walker, which treats the head position of
`(K0 K1 ... Kn COLL)' as literal syntax.  Expands to a left-fold
over `easy-access-lookup', mirroring `easy-access--expand-accessor'.

Use this when a key is runtime-computed: `(intern ...)', a let-bound
variable, `(match-string ...)', and so on.  When every key is a
literal, the reader hook already handles the terser
`(K0 K1 ... COLL)' syntax.

Examples:

  (easy-access (intern (string kind)) \\='(b ?\\* i ?\\/ u ?\\_ c ?\\~))
    ==> lookup via `plist-get' on the plist, with a computed key.

  (easy-access (string-trim s) alist)
    ==> lookup via `assoc' on the alist (string key, `equal').

  (easy-access :name 0 users)
    ==> (easy-access-lookup (easy-access-lookup users 0) :name)

Chain order is strictly left-to-right: the first key drills into
COLL, the next into that result, and so on."
  (declare (indent 0))
  (unless (>= (length args) 2)
    (signal 'wrong-number-of-arguments '(easy-access 2)))
  (let ((keys (butlast args))
        (coll (car (last args))))
    (cl-reduce (lambda (acc key) `(easy-access-lookup ,acc ,key))
               keys
               :initial-value coll)))

;;; -------------------------------------------------------------------------
;;; setf support
;;; -------------------------------------------------------------------------
;;;
;;; Register `easy-access-lookup' as a generalised variable so that
;;;
;;;   (setf (:key obj) new-value)
;;;
;;; Just Works.  The walker rewrites the setf place to
;;; `(easy-access-lookup obj :key)', so the generalised-variable setter
;;; dispatches at runtime by type, mirroring the reader.

(gv-define-setter easy-access-lookup (new-value obj key)
  "Generalised-variable setter for `easy-access-lookup'.
Dispatches by type of KEY and OBJ, mirroring the reader:

 * KEY an integer on a list           -- splices via `setcar' + `nthcdr'.
 * KEY an integer on a vector         -- `aset'.
 * KEY a symbol (keyword or plain) on a struct  -- `setf' on the slot.
 * KEY a symbol on a hash             -- `puthash'.
 * KEY a symbol on an alist           -- `setcdr' on the matching pair.
 * KEY a symbol on a plist            -- `plist-put', re-binding OBJ if
   it is a setf-able place.

The plist case requires that OBJ be a place writable by `setf' --
e.g. a variable, slot, or nth of a list."
  `(easy-access--set ,obj ,key ,new-value))

(defun easy-access--set (obj key new-value)
  "Runtime setter for `easy-access-lookup' forms.
Dispatches through `easy-access--rules' -- the first rule whose
guard matches KEY owns the write.  Per-target dispatch happens
inside the rule body.  Target of the generalised-variable setter
installed by `gv-define-setter' -- users do not call directly.

Sharp edges inherited from the stock setters: `plist-put' mutates
in place only when KEY exists, and `nconc' on alists requires
non-empty lists; a fresh empty plist or alist needs the variable
rebound, which the gv-setter does not do."
  (let ((rule (easy-access--matching-rule key)))
    (unless rule (signal 'easy-access-invalid-key (list key obj)))
    (funcall (easy-access-rule-set-fn rule) key obj new-value)))

;;; -------------------------------------------------------------------------
;;; Built-in rules
;;; -------------------------------------------------------------------------
;;;
;;; These three `defcall' forms re-declare the stock keyword / integer /
;;; quoted-symbol behaviour -- everything that used to be hard-coded in
;;; `easy-access-lookup' and `easy-access--set'.  Each rule is self-
;;; contained: the guard names one CAR shape, and the body's `cond'
;;; handles every target shape the rule cares about.  User-defined rules
;;; added later via `defcall' sit in front of these in the registry and
;;; shadow them where their guards overlap.

(defcall integers-as-accessors (head target &optional value)
  :when (integerp head)
  "Integers in CAR position index into sequences; `setf'-able.
Dispatches to `nth' / `elt' for reads and `setcar'+`nthcdr' /
`aset' for writes.  Strings are read-only -- no `setf' branch.

Negative indices count from the end, Python-style: -1 is the
last element, -2 the second-to-last, and so on.  So
`(-1 [a b c])' ⟶ `c' and `(-2 \"abc\")' ⟶ ?b.  This applies
uniformly to lists, vectors, and strings, for both reads and
`setf' writes.

Out-of-bounds reads (on either side) return nil uniformly across
lists, vectors, and strings -- matching `nth's lenient behaviour
rather than `elt's `args-out-of-range' signal.  Writes past the
end still error (via `aset' / `setcar' on a too-short tail), as
clobbering a non-existent index has no sensible meaning."
  (let* ((len (cond ((listp target) (length target))
                    ((sequencep target) (length target))
                    (t 0)))
         ;; Python-style negative indexing: -k ↦ len + (-k).  In-bounds
         ;; negatives land at ≥ 0; out-of-bounds negatives (e.g. -99 on
         ;; a 3-sequence) stay negative, so the downstream bounds check
         ;; still catches them.
         (idx (if (< head 0) (+ len head) head)))
    (cond
     ((listp target)
      (if (easy-access-setting-p)
          (easy-access--list-index-set target idx value)
        (and (>= idx 0) (< idx len)
             (nth idx target))))
     ((vectorp target)
      (if (easy-access-setting-p)
          (aset target idx value)
        (and (>= idx 0) (< idx len)
             (elt target idx))))
     ((sequencep target)                  ; strings etc.
      (if (easy-access-setting-p)
          (signal 'easy-access-invalid-key (list head target))
        (and (>= idx 0) (< idx len)
             (elt target idx))))
     (t (signal 'easy-access-invalid-key (list head target))))))

(defcall keywords-as-accessors (head target &optional value)
  :when (keywordp head)
  "Keywords in CAR position look up by association; `setf'-able.
Dispatches to struct slots, alist pairs, hash-table entries, or
plist cells -- in that order.  Alist detection uses the \"CAR is
a cons cell\" heuristic."
  (cond
   ((and (recordp target)
         (easy-access--struct-accessor (aref target 0) head))
    (if (easy-access-setting-p)
        (easy-access--struct-set target head value)
      (easy-access--struct-get target head)))
   ((easy-access--alistp target)
    (if (easy-access-setting-p)
        (easy-access--alist-set target head value)
      (cdr (assq head target))))
   ((hash-table-p target)
    (if (easy-access-setting-p)
        (puthash head value target)
      (gethash head target)))
   ((listp target)
    (if (easy-access-setting-p)
        (easy-access--plist-set target head value)
      (plist-get target head)))
   (t (signal 'easy-access-invalid-key (list head target)))))

(defcall quoted-symbols-as-accessors (head target &optional value)
  :when (and (symbolp head) (not (keywordp head))
             (not (null head)) (not (eq head t)))
  :read-key easy-access--quoted-symbol-read-key ; strip (quote SYM) -> SYM
  "Quoted symbols -- (quote SYM) in CAR position -- look up by
plain-symbol key; `setf'-able.  The `:read-key' canonicalises the
CAR from `(quote custom-group)' to `custom-group' before it
reaches the body or the runtime guard.  The guard then tests for
a plain (non-keyword, non-nil, non-t) symbol -- the canonical
shape of a quoted-symbol accessor key.  Body is the same shape as
`keywords-as-accessors'."
  (cond
   ((and (recordp target)
         (easy-access--struct-accessor (aref target 0) head))
    (if (easy-access-setting-p)
        (easy-access--struct-set target head value)
      (easy-access--struct-get target head)))
   ((easy-access--alistp target)
    (if (easy-access-setting-p)
        (easy-access--alist-set target head value)
      (cdr (assq head target))))
   ((hash-table-p target)
    (if (easy-access-setting-p)
        (puthash head value target)
      (gethash head target)))
   ((listp target)
    (if (easy-access-setting-p)
        (easy-access--plist-set target head value)
      (plist-get target head)))
   (t (signal 'easy-access-invalid-key (list head target)))))

(defun easy-access--alist-set-equal (target key value)
  "Like `easy-access--alist-set' but compares via `equal', not `eq'.
Needed for string keys where `assq' would miss the pair."
  (let ((pair (assoc key target)))
    (if pair
        (setcdr pair value)
      (nconc target (list (cons key value))))
    value))

(defcall strings-as-accessors (head target &optional value)
  :when (stringp head)
  "String CARs look up by `equal' association; `setf'-able.
Dispatches to alist pairs (via `assoc'), hash-table entries
\(via `gethash'), or plist cells (via `plist-get' with `equal'
predicate).  Alist detection uses the same \"CAR is a cons cell\"
heuristic as keywords — anything else that is a list is a plist."
  (cond
   ((easy-access--alistp target)
    (if (easy-access-setting-p)
        (easy-access--alist-set-equal target head value)
      (cdr (assoc head target))))
   ((hash-table-p target)
    (if (easy-access-setting-p)
        (puthash head value target)
      (gethash head target)))
   ((listp target)
    (if (easy-access-setting-p)
        (progn (plist-put target head value #'equal) value)
      (plist-get target head #'equal)))
   (t (signal 'easy-access-invalid-key (list head target)))))

;;; -------------------------------------------------------------------------
;;; The code walker
;;; -------------------------------------------------------------------------
;;;
;;; `easy-access-walk' recursively rewrites a form, turning accessor
;;; patterns into calls to `easy-access-lookup'.  The walker is careful to
;;; skip sub-forms that are data rather than code: quoted forms, binding
;;; positions, arg lists, etc.  It is IDEMPOTENT: walking an already-
;;; rewritten form is a no-op, so multiple hook points can coexist without
;;; double-rewriting.

(defvar easy-access--walk-skip-cars
  '(quote)
  "CAR symbols whose args are data, not code -- walker skips them entirely.
Note: `function' used to be here but is now handled explicitly in
the walker to distinguish `#'SYMBOL' (data) from `#'(lambda ...)'
(contains code).")

(defvar easy-access--gv-macros
  '(setf setq cl-setf psetf psetq
    push pop cl-pushnew cl-callf cl-callf2
    cl-letf cl-letf*
    cl-incf cl-decf incf decf)
  "Macros that operate on generalised variables (places).
The walker must NOT macroexpand these — the gv machinery needs to
see the raw `(:key obj)' accessor form to dispatch through
`gv-define-setter'.  All other macros are safe to expand before
walking.")


(defun easy-access--quoted-symbol-p (form)
  "Return non-nil if FORM is `(quote SYM)' with SYM a non-keyword symbol.
A quoted symbol in function position is a valid accessor key -- it
targets plain-symbol plists (e.g. `symbol-plist' results), alists
keyed by symbols, and hash-tables with symbol keys.  We exclude
keywords because they are already handled by the bare-keyword
branch: writing `(:foo p)' and `('\\:foo p)' should mean the same
thing."
  (and (consp form)
       (eq (car form) 'quote)
       (consp (cdr form))
       (null (cddr form))
       (symbolp (cadr form))
       (not (keywordp (cadr form)))))

(defun easy-access--quoted-symbol-read-key (form)
  "Extract the symbol from a `(quote SYM)' surface form.
Signals an error if FORM is not exactly `(quote SYM)' with SYM a
non-keyword symbol -- the `condition-case' in
`easy-access--surface-matching-rule' catches the error and skips
the rule.  Bare `cadr' would accept any two-or-more-element list
\(e.g. `(and x y z)'), leading to false positives."
  (unless (easy-access--quoted-symbol-p form)
    (signal 'wrong-type-argument (list 'quoted-symbol-form form)))
  (cadr form))

(defun easy-access--key-atom-p (key)
  "Return non-nil if KEY is a valid accessor-key surface shape.
A CAR position is an accessor iff some rule in `easy-access--rules'
surface-matches KEY.  See `easy-access--surface-matching-rule'
for how surface matching works -- `:read-key' is applied to KEY
and then the rule's `:when' tests the canonical value.

The three built-in rules (keywords, integers, quoted-symbols)
cover the stock shapes; user-defined rules add more.  Bare
symbols are NOT keys by default -- they are variable references
and would be ambiguous with ordinary function-call syntax."
  (and (easy-access--surface-matching-rule key) t))

(defun easy-access--normalise-key (key)
  "Return the runtime key literal for surface-syntax KEY.
Each rule owns a `:read-key' function that canonicalises KEY
before the get/set body sees it -- e.g. `quoted-symbols-as-
accessors' unwraps `(quote SYM)' to `SYM' via `cadr'.  The walker
emits the canonical key literal into the rewritten form, so no
`:read-key' work happens at runtime.

Emitted form:
  keyword / integer key  ==>  :K / N  (self-evaluating)
  quoted-symbol key      ==>  \\='SYM  (quoted symbol form)
  user-defined           ==>  canonical value rendered as a
                               self-evaluating or \\='quote\\='d form."
  (let* ((match (easy-access--surface-matching-rule key)))
    (if (null match)
        key
      (let ((canonical (cdr match)))
        (cond
         ;; Self-evaluating canonical values pass through.
         ((or (keywordp canonical) (numberp canonical)
              (stringp canonical) (characterp canonical)
              (vectorp canonical))
          canonical)
         ;; Non-nil non-t symbols need `quote' so runtime reads the
         ;; symbol literally rather than dereferencing as a variable.
         ((and (symbolp canonical) (not (null canonical)) (not (eq canonical t)))
          (list 'quote canonical))
         ;; Everything else (cons cells etc.) also needs `quote'.
         (t (list 'quote canonical)))))))

(defun easy-access--accessor-form-p (form)
  "Return non-nil if FORM is an accessor pattern.
An accessor pattern is a proper list of length at least 2 whose
leading elements -- all but the last -- are accessor keys
(keywords, integers, or quoted symbols), and whose last element
is the target object.  A single leading key plus a target is the
minimal case; chains of keys are also accepted.  Dotted pairs and
improper lists are never accessor patterns -- they are data
structures (e.g. elements of a byte-compiled load history).

Note -- a form like `((quote f) arg)' would, in dynamic-binding
Elisp prior to 24, have meant \"funcall the function named f on
arg\".  That idiom is rejected outright by modern lexical-binding
Elisp, so claiming this shape as an accessor is additive in
practice -- no form that works today changes meaning."
  (and (consp form)
       (proper-list-p form)
       (>= (length form) 2)
       ;; Every leading element must look like an accessor key.  We
       ;; inline the check with a plain `while' loop rather than
       ;; `cl-every', because the walker runs inside `load-read-function'
       ;; and may itself be triggered by `require' of `cl-extra' -- a
       ;; `cl-' call here would cause a recursive-load error.
       (let ((ok t)
             (rest (butlast form)))
         (while (and ok rest)
           (unless (easy-access--key-atom-p (car rest))
             (setq ok nil))
           (setq rest (cdr rest)))
         ok)))

(defun easy-access--expand-accessor (form)
  "Expand an accessor pattern FORM into its rewritten equivalent.

For a single-accessor form `(:K obj)', produce:

    (easy-access-lookup (easy-access-walk obj) :K)

For a chained form `(K1 K2 ... Kn obj)', produce left-to-right
threaded calls -- the path reads in natural order, just as with
Clojure's `get-in' or `->':

    (:a :b 1 obj)  ==>  (get-in obj [:a :b 1])

is rendered as

    (easy-access-lookup
      (easy-access-lookup
        (easy-access-lookup obj :a)
        :b)
      1)

Each key is a keyword, integer, or quoted symbol.  Keywords and
integers are self-evaluating and pass through verbatim; quoted
symbols like `'custom-group' remain quoted so the runtime sees the
symbol itself, not its variable value.

The last element of FORM is the target object, walked recursively."
  (let* ((keys (mapcar #'easy-access--normalise-key (butlast form)))
         (target (easy-access-walk (car (last form)))))
    ;; Plain left-to-right fold: each key wraps the running value in
    ;; another `easy-access-lookup', so the leftmost key is the
    ;; innermost (i.e. first-applied) access.
    (cl-reduce (lambda (acc key) (list 'easy-access-lookup acc key))
               keys
               :initial-value target)))

(defun easy-access-walk (form)
  "Recursively rewrite FORM, expanding easy-access accessor patterns.

Rewrite rules:

    (:K obj)            ==>  (easy-access-lookup obj :K)
    (N obj)             ==>  (easy-access-lookup obj N)
    (:K1 :K2 ... Kn obj) ==>  threaded left-to-right lookups

Sub-forms that are DATA rather than code are NOT walked:

 * Quoted forms: `(quote X)' and `#''X'.
 * Binding positions in `let' / `let*' / `condition-case'.
 * Arg lists of `defun' / `defmacro' / `lambda' / `cl-defmethod'.
 * Slot declarations inside `cl-defstruct'.
 * Backquote templates -- unquoted sub-expressions are walked;
   quoted portions are not.
 * Improper lists and dotted pairs -- these cannot be function
   call forms in Elisp; they are data (e.g. entries in Emacs's
   load-history).

The walker is IDEMPOTENT: `(equal (walk F) (walk (walk F)))' for
every FORM.  This is necessary because multiple hook points may
walk the same form (e.g. `elisp--preceding-sexp' advice followed
by `eval' advice); double-walking must be harmless."
  (cond
   ;; Atoms pass through unchanged.
   ((atom form) form)
   ;; Improper lists / dotted pairs: data, not code.
   ((not (proper-list-p form)) form)
   ;; Accessor patterns are the whole point.
   ((easy-access--accessor-form-p form)
    (easy-access--expand-accessor form))
   ;; Everything else: dispatch on CAR.
   (t (easy-access--walk-compound form))))

;;; -------------------------------------------------------------------------
;;; Declarative special-form handlers -- `defcall-rewrite'
;;; -------------------------------------------------------------------------
;;;
;;; Most special-form branches in `easy-access--walk-compound' share a
;;; simple structure: destructure the form, pass /some/ sub-forms through
;;; `easy-access-walk', leave the rest verbatim, re-assemble.  That shape
;;; is mechanical and worth capturing declaratively -- especially so that
;;; third-party code can teach the walker about its own macros without
;;; patching this file.
;;;
;;; A `defcall-rewrite' entry pairs a SOURCE (what to match) with a
;;; TARGET (what to emit).  Inside the target, two markers drive
;;; recursive rewriting of sub-forms:
;;;
;;;   (rewrite! X)       -- rewrite one sub-form: expands to
;;;                          `(easy-access-walk X)'.
;;;   (rewrite-each! XS) -- rewrite each element of a list: expands to
;;;                          `(mapcar #'easy-access-walk XS)' spliced in.
;;;
;;; Rest-capture uses native Lisp dotted-tail notation: `(HEAD a b . body)'
;;; matches `(HEAD 1 2 3 4 ...)' with `body' bound to `(3 4 ...)', and a
;;; target like `(HEAD a b . body)' re-attaches that tail verbatim.
;;; Proper-list elements are strictly positional -- no CL-style `&body'
;;; marker needed.
;;;
;;; Any other sub-form in the target is emitted verbatim, with source
;;; binders substituted.  The target is returned AS-IS -- the walker
;;; does NOT re-walk it.  Re-entry happens only through the explicit
;;; `rewrite!' / `rewrite-each!' markers.
;;;
;;; Example:
;;;
;;;   (defcall-rewrite define-advice-rewrite
;;;     \"Skip the name-spec; rewrite only the body.\"
;;;     :source (define-advice fn name-spec . body)
;;;     :target (define-advice fn name-spec (rewrite-each! body)))
;;;
;;; Matcher backend is `pcase' -- we compile SOURCE into a pcase pattern
;;; and TARGET into a constructor expression in one shot, so nested
;;; destructuring, arity-mismatch fallthrough, and rest-capture all
;;; come for free.

(cl-defstruct easy-access-rewrite
  "One rewrite entry: a source pattern plus a target template.
NAME        -- symbol, for upsert / remove by identity.
DOCSTRING   -- optional human-readable description.
SOURCE-HEAD -- the CAR symbol this rewrite matches; used to cull the
               registry scan in `easy-access--rewrite-dispatch'.
MATCH-FN    -- a unary function of FORM that returns either the
               rewritten form or the sentinel symbol
               `easy-access--rewrite-no-match' on failure."
  name docstring source-head match-fn)

(defvar easy-access--rewrites nil
  "List of active `easy-access-rewrite' records.
Walked front-to-back in `easy-access--rewrite-dispatch'; the first
rewrite whose matcher succeeds wins.  Most-recently-defined rewrites
sit at the front -- see `easy-access--upsert-rewrite'.")

(defun easy-access--upsert-rewrite (rewrite)
  "Install REWRITE into `easy-access--rewrites'.
Upserts by NAME: an existing rewrite with the same NAME is replaced
in place /and/ promoted to the front.  A new NAME is pushed to the
front.  Mirrors `easy-access--upsert-rule' for `defcall' rules."
  (let ((name (easy-access-rewrite-name rewrite)))
    (setq easy-access--rewrites
          (cl-delete-if (lambda (r)
                          (eq (easy-access-rewrite-name r) name))
                        easy-access--rewrites))
    (push rewrite easy-access--rewrites))
  rewrite)

(defun easy-access-remove-rewrite (name)
  "Remove the rewrite named NAME from the registry.
Returns non-nil if a rewrite was removed.  Inverse of
`defcall-rewrite'; useful in tests and for undoing a user-defined
handler."
  (let ((before (length easy-access--rewrites)))
    (setq easy-access--rewrites
          (cl-delete-if (lambda (r)
                          (eq (easy-access-rewrite-name r) name))
                        easy-access--rewrites))
    (/= before (length easy-access--rewrites))))

(defun easy-access--source-head (source)
  "Return the head symbol of SOURCE (a `defcall-rewrite' source pattern).
The head is the first element and must be a non-keyword symbol.
Signals an error otherwise -- a rewrite whose head is a binder
would match any compound form, which is never what users want."
  (unless (and (consp source) (symbolp (car source))
               (not (keywordp (car source))))
    (error "defcall-rewrite: SOURCE must start with a literal head symbol, got %S"
           source))
  (car source))

(defun easy-access--source-collect-binders (source)
  "Return the list of binder symbols in SOURCE, in left-to-right order.
The first element of SOURCE is the literal head -- it is NOT a binder.
Nested lists contribute their own binders recursively.  A *dotted*
tail at any list level names a rest-binder; a proper-list positional
element is a plain binder.  Either way it goes into the list."
  (let ((binders nil))
    (cl-labels
        ((walk (p)
           (cond
            ((null p) nil)
            ((symbolp p)
             (unless (keywordp p)
               (push p binders)))
            ((consp p)
             (walk (car p)) (walk (cdr p))))))
      ;; Skip the head; it is the literal match target.
      (walk (cdr source)))
    (nreverse binders)))

(defun easy-access--source->pcase (source)
  "Translate a `defcall-rewrite' SOURCE pattern into a `pcase' pattern.

The top-level head of SOURCE is a literal symbol; inside, every
bare symbol becomes a binder.  A trailing bare symbol in any list
is a rest-capture (the dotted tail), so `(HEAD a b body)' matches
`(HEAD x y z w ...)' with `body' bound to `(z w ...)'.  Nested
lists destructure positionally.

Returns a `pcase' pattern suitable for use as the first element of
a pcase clause."
  (let ((head (car source))
        (rest (cdr source)))
    ;; Top level MUST be a proper backquote pattern with literal head.
    ;; The `rest' items translate via `--source->pcase-inner-list'.
    (cons '\` (list (cons head (easy-access--source->pcase-inner-list rest))))))

(defun easy-access--source->pcase-inner-list (items)
  "Translate a list of source ITEMS to the interior of a backquote pcase.
A *dotted* tail (i.e. ITEMS itself is an improper list ending in a
bare binder symbol) becomes a pcase dotted-tail unquote -- that is
how you opt into rest-capture in the DSL.  Proper lists always
destructure positionally."
  (cond
   ((null items) nil)
   ;; Improper-list dotted tail: `(... . BINDER)'.
   ((and (not (consp items)) (symbolp items) items
         (not (keywordp items)) (not (memq items '(t nil))))
    (list '\, items))
   ((not (consp items))
    ;; Dotted tail that is NOT a plain binder -- literal.
    items)
   (t
    (cons (easy-access--source->pcase-inner-atom (car items))
          (easy-access--source->pcase-inner-list (cdr items))))))

(defun easy-access--source->pcase-inner-atom (item)
  "Translate a single source ITEM (not a rest-capture) to its pcase form.
Symbols become `,SYM' unquote-binders; keywords / t / nil pass
through as literals; nested lists recurse."
  (cond
   ((null item) nil)
   ((symbolp item)
    (if (or (keywordp item) (memq item '(t nil)))
        item
      (list '\, item)))
   ((consp item)
    (easy-access--source->pcase-inner-list item))
   (t item)))

(defun easy-access--target->template (target binders)
  "Translate a `defcall-rewrite' TARGET into a *constructor expression*.
BINDERS is the list of symbols captured by the source pattern; symbols
in TARGET that appear in BINDERS are emitted as variable reads,
everything else is quoted.

The returned expression, when evaluated in a lexical context where the
source binders are bound, produces the rewritten form.

- A binder `X' becomes the expression `X' (reads the binder).
- `(rewrite! X)' becomes `(easy-access-walk X)' (X evaluated in scope).
- `(rewrite-each! XS)' becomes `(mapcar #\\='easy-access-walk XS)'.
- Any other list is built with `cons'/`append' constructors so that
  literal heads remain quoted and binders are spliced in.  A
  `(rewrite-each! XS)' sitting in list position is spliced via `append'."
  (cond
   ((null target) nil)
   ((symbolp target)
    (if (memq target binders) target (list 'quote target)))
   ((consp target)
    (pcase target
      (`(rewrite! ,x)
       (list 'easy-access-walk (easy-access--target->template x binders)))
      (`(rewrite-each! ,xs)
       (list 'mapcar (list 'function 'easy-access-walk)
             (easy-access--target->template xs binders)))
      (_ (easy-access--target->template-list target binders))))
   (t (list 'quote target))))

(defun easy-access--target->template-list (items binders)
  "Translate a proper list ITEMS of target elements into a constructor.
A dotted-tail ITEMS ending in a bare binder symbol emits the binder
in the list's cdr, so a source-side rest-capture flows back into the
target verbatim.  Splices `(rewrite-each! XS)' elements via `append'.
BINDERS is the source pattern's binder list."
  (cond
   ((null items) nil)
   ;; Improper-list dotted tail ending in a source binder: emit as the
   ;; cdr of the enclosing list.
   ((and (not (consp items)) (symbolp items) (memq items binders))
    items)
   ((not (consp items))
    ;; Dotted tail that is NOT a recognised binder: literal.
    (list 'quote items))
   (t
    (let ((head (car items))
          (tail (cdr items)))
      (pcase head
        (`(rewrite-each! ,xs)
         (list 'append
               (list 'mapcar (list 'function 'easy-access-walk)
                     (easy-access--target->template xs binders))
               (easy-access--target->template-list tail binders)))
        (_
         (list 'cons
               (easy-access--target->template head binders)
               (easy-access--target->template-list tail binders))))))))

(defun easy-access--target-collect-binders (target)
  "Collect binder-candidate symbols referenced in TARGET.
`TARGET' is a template form `(HEAD ARG...)'.  `HEAD' is an operator
literal, skipped.  Each `ARG' may itself be a form -- its own head
is then an operator we also skip.  The result is a de-duplicated list
of symbols in argument positions, ready for cross-check against the
source pattern's binders."
  (let ((syms nil))
    (cl-labels
        ((arg (r)
           ;; R sits in an argument position.
           (cond
            ((null r) nil)
            ((symbolp r)
             (unless (or (keywordp r) (memq r '(t nil)))
               (push r syms)))
            ((consp r)
             (pcase r
               (`(quote ,_) nil)                 ; 'x is literal
               (`(function ,_) nil)              ; #'x is literal
               (`(rewrite! ,x) (arg x))
               (`(rewrite-each! ,xs) (arg xs))
               (_
                ;; Nested form `(OP a b c)': OP skipped, children are
                ;; argument positions.
                (mapc #'arg (cdr r))))))))
      ;; The cdr of TARGET is either a proper list of arg positions or
      ;; an improper tail ending in a rest-binder.  Either way, a tail
      ;; walk that visits every cons-cell AND the final atom collects
      ;; every binder-candidate.
      (let ((tail (cdr target)))
        (while (consp tail)
          (arg (car tail))
          (setq tail (cdr tail)))
        (when tail (arg tail))))
    (delete-dups (nreverse syms))))

(defmacro defcall-rewrite (name &rest spec)
  "Declare a walker rewrite NAME for a macro or special-form shape.

SPEC is, in order:

  [DOCSTRING]        -- optional string.
  :source (HEAD ...) -- the shape to match.  HEAD is a literal
                        symbol; every other symbol in SOURCE is a
                        binder.  A *dotted* tail `(... . BINDER)' at
                        any list level names a rest-binder that
                        captures the (possibly empty) cdr.  Proper
                        lists destructure strictly positionally.
  :target (HEAD ...) -- the shape to emit.  Binders from SOURCE
                        substitute in place.  Inside TARGET:
                          (rewrite! X)       -- rewrite one sub-form.
                          (rewrite-each! XS) -- rewrite each element
                                                 of a list and splice
                                                 the results in place.
                        A dotted-tail target `(... . BINDER)' emits
                        the source-side rest-capture as the cdr, so
                        rest flows through without ever appearing
                        nested.  Any other sub-form is emitted
                        verbatim.  The target is returned AS-IS --
                        the walker does NOT re-walk it.  Re-entry is
                        via the explicit `rewrite!' / `rewrite-each!'
                        markers.

NAME is a symbol: re-defining an existing NAME upserts and promotes
it to the front of `easy-access--rewrites'.

Example:

  (defcall-rewrite define-advice-rewrite
    \"Skip the name-spec; rewrite only the body.\"
    :source (define-advice fn name-spec . body)
    :target (define-advice fn name-spec (rewrite-each! body)))

\(fn NAME [DOCSTRING] :source SOURCE :target TARGET)"
  (declare (indent 1) (doc-string 2))
  (let ((docstring (when (stringp (car spec)) (pop spec)))
        source target)
    (while (keywordp (car spec))
      (let ((k (pop spec))
            (v (pop spec)))
        (pcase k
          (:source (setq source v))
          (:target (setq target v))
          (_ (error "defcall-rewrite: unknown option %S" k)))))
    (unless source (error "defcall-rewrite: missing :source"))
    (unless target (error "defcall-rewrite: missing :target"))
    (let* ((head (easy-access--source-head source))
           (binders (easy-access--source-collect-binders source))
           (target-syms (easy-access--target-collect-binders target))
           (unbound (cl-set-difference target-syms binders)))
      ;; Cross-check: every symbol the target references that isn't a
      ;; function or global must come from the source pattern.  We
      ;; can't know at expansion time what's global, but flagging
      ;; unbound symbols catches typos like (rewrite-each! bdoy).
      ;; Allow fboundp symbols through -- they're function references.
      (let ((truly-unbound
             (cl-remove-if (lambda (s)
                             (or (fboundp s)
                                 (boundp s)
                                 (keywordp s)))
                           unbound)))
        (when truly-unbound
          (error "defcall-rewrite %S: target references unbound symbols %S (binders: %S)"
                 name truly-unbound binders)))
      (let ((form-sym (make-symbol "form")))
        `(easy-access--upsert-rewrite
          (make-easy-access-rewrite
           :name ',name
           :docstring ,docstring
           :source-head ',head
           :match-fn
           (lambda (,form-sym)
             (pcase ,form-sym
               (,(easy-access--source->pcase source)
                ,(easy-access--target->template target binders))
               (_ 'easy-access--rewrite-no-match)))))))))

(defun easy-access--rewrite-dispatch (form)
  "Try every registered rewrite against FORM.
Returns the rewritten form, or the sentinel
`easy-access--rewrite-no-match' when no rewrite matches.  Linear
scan over `easy-access--rewrites', culled by `source-head' `eq' match
on FORM's CAR so the typical call checks one or two matchers."
  (let ((head (car-safe form))
        (rs easy-access--rewrites)
        (result 'easy-access--rewrite-no-match))
    (while (and rs (eq result 'easy-access--rewrite-no-match))
      (let ((r (car rs)))
        (when (eq (easy-access-rewrite-source-head r) head)
          (setq result (funcall (easy-access-rewrite-match-fn r)
                                form))))
      (setq rs (cdr rs)))
    result))

(defun easy-access--walk-compound (form)
  "Walk a non-accessor compound FORM.
Dispatches on CAR to handle special forms correctly -- skipping
binding positions, arg lists, and other data sub-forms while
walking code sub-forms."
  (let ((head (car form)))
    (cond
     ;; Quoted forms: data, not code.
     ((eq head 'quote) form)
     ;; Function quotes: #'SYMBOL is data (a function reference),
     ;; but #'(lambda ARGS BODY...) contains code in BODY that
     ;; must be walked -- cl-flet and cl-labels expand to this shape.
     ((eq head 'function)
      (let ((arg (cadr form)))
        (if (and (consp arg) (eq (car arg) 'lambda))
            `(function (lambda ,(cadr arg)
                         ,@(mapcar #'easy-access-walk (cddr arg))))
          form)))
     ;; Backquote: walk the template, respecting unquotes.
     ((eq head '\`)
      (list '\` (easy-access--walk-backquote (cadr form))))
     ;; User-defined rewrites (see `defcall-rewrite').  Dispatched
     ;; AFTER quote/function/backquote (those are load-bearing invariants)
     ;; and BEFORE every other branch -- so a rewrite can override the
     ;; built-in handlers and, crucially, fire before generic macro
     ;; expansion would eat the form.
     ;; Returns the rewritten form if a rewrite fired, else falls
     ;; through.  Real rewrites are always compound forms (their CAR is
     ;; the source head), so nil-vs-no-match is not a practical
     ;; ambiguity.
     ((let ((rewritten (easy-access--rewrite-dispatch form)))
        (and (not (eq rewritten 'easy-access--rewrite-no-match))
             rewritten)))
     ;; let / let*: walk binding RHS expressions, skip binding names;
     ;; walk body.
     ((memq head '(let let*))
      `(,head
        ,(mapcar (lambda (binding)
                   (cond
                    ((symbolp binding) binding)
                    ((and (consp binding) (= 1 (length binding))) binding)
                    ((consp binding)
                     (cons (car binding)
                           (mapcar #'easy-access-walk (cdr binding))))
                    (t binding)))
                 (cadr form))
        ,@(mapcar #'easy-access-walk (cddr form))))
     ;; condition-case: skip the variable name; walk the protected form
     ;; and each handler body.
     ((eq head 'condition-case)
      `(condition-case
           ,(cadr form)
         ,(easy-access-walk (caddr form))
         ,@(mapcar (lambda (handler)
                     (cons (car handler)
                           (mapcar #'easy-access-walk (cdr handler))))
                   (cdddr form))))
     ;; cl-defmethod: (cl-defmethod NAME [QUALIFIER] ARGLIST BODY...)
     ;; The arglist contains type specializers (e.g. ((x my-struct)))
     ;; that must not be walked.  Skip everything up to and including
     ;; the arglist, walk only the body.
     ((eq head 'cl-defmethod)
      (let* ((rest (cdr form))          ; (NAME [QUAL] ARGLIST BODY...)
             (name (pop rest))
             ;; Qualifiers are keywords like :around, :before, :after —
             ;; skip them.
             (quals nil))
        (while (and rest (keywordp (car rest)))
          (push (pop rest) quals))
        ;; Next element is the arglist — skip it too.
        (let ((arglist (pop rest)))
          `(cl-defmethod ,name ,@(nreverse quals) ,arglist
                         ,@(mapcar #'easy-access-walk rest)))))
     ;; cl-letf / cl-letf*: (cl-letf ((PLACE VALUE) ...) BODY...)
     ;; PLACE is a generalized variable -- the gv machinery must see
     ;; raw accessor forms like `(:key obj)' so it can dispatch via
     ;; `gv-define-setter'.  VALUE is code.  BODY is code.  Walk only
     ;; VALUE and BODY; leave PLACE untouched.  This is why `cl-letf'
     ;; sits in `easy-access--gv-macros' -- it must not be
     ;; macroexpanded before the walker gets to it.  A `defcall-rewrite'
     ;; would need to express "walk the 2nd element of each binding,"
     ;; which the declarative DSL cannot say tersely.
     ((memq head '(cl-letf cl-letf*))
      `(,head
        ,(mapcar (lambda (binding)
                   (if (and (consp binding) (consp (cdr binding)))
                       (list (car binding)
                             (easy-access-walk (cadr binding)))
                     binding))
                 (cadr form))
        ,@(mapcar #'easy-access-walk (cddr form))))
     ;; pcase / pcase-exhaustive: (pcase EXPR (PAT BODY...) ...)
     ;; Patterns are data — keywords, integers, quoted symbols all
     ;; appear there as match specs, not accessor forms.  Walk EXPR
     ;; and each clause's BODY but leave PAT untouched.
     ((memq head '(pcase pcase-exhaustive))
      `(,head ,(easy-access-walk (cadr form))
              ,@(mapcar (lambda (clause)
                          (cons (car clause)
                                (mapcar #'easy-access-walk (cdr clause))))
                        (cddr form))))
     ;; pcase-let / pcase-let*: (pcase-let ((PAT EXPR) ...) BODY...)
     ;; Walk each binding's EXPR but leave PAT alone; walk BODY.
     ((memq head '(pcase-let pcase-let*))
      `(,head
        ,(mapcar (lambda (binding)
                   (if (and (consp binding) (cdr binding))
                       (cons (car binding)
                             (mapcar #'easy-access-walk (cdr binding)))
                     binding))
                 (cadr form))
        ,@(mapcar #'easy-access-walk (cddr form))))
     ;; cond: (cond (TEST BODY...) ...)
     ;; Each clause is a list (TEST BODY...).  Without explicit handling,
     ;; the default walker passes each clause through `easy-access-walk'
     ;; as a form — and a clause like (:otherwise BODY) matches the
     ;; accessor pattern (:keyword obj).  Walking TEST and BODY
     ;; individually avoids this.
     ((eq head 'cond)
      `(cond ,@(mapcar (lambda (clause)
                         (mapcar #'easy-access-walk clause))
                       (cdr form))))
     ;; cl-case / case / ecase / cl-ecase / cl-typecase / cl-etypecase:
     ;; (cl-case EXPR (KEY BODY...) ...)
     ;; KEY positions are literal match values (integers, keywords,
     ;; symbols, lists of values, t, otherwise) — not code.  Walk EXPR
     ;; and each clause's BODY, but leave KEY untouched.
     ((memq head '(case cl-case ecase cl-ecase
                        cl-typecase cl-etypecase))
      `(,head ,(easy-access-walk (cadr form))
              ,@(mapcar (lambda (clause)
                          (cons (car clause)
                                (mapcar #'easy-access-walk (cdr clause))))
                        (cddr form))))
     ;; Keyword-plist macros: macroexpand before walking.
     ;;
     ;; Macros like `use-package', `define-relation', and other
     ;; DSL-style macros accept keyword-argument plists where forms
     ;; like `(:url "...")' or `("s1" "s2")' appear in data positions.
     ;; Walking their sub-forms directly would misidentify these as
     ;; accessor forms.  Macroexpansion produces standard code whose
     ;; sub-forms are all in code position.
     ;;
     ;; We expand any macro EXCEPT those known to need the walker's
     ;; raw accessor forms (e.g. `setf', `push', `cl-callf' — the
     ;; generalized-variable machinery must see `(:key obj)' intact).
     ;; Full `macroexpand' (not -1) for multi-step expansions.
     ((and (symbolp head) (macrop head)
           (not (memq head easy-access--gv-macros)))
      (let ((expanded (condition-case nil
                          (macroexpand form)
                        (error nil))))
        (if (and expanded (not (equal expanded form)))
            (easy-access-walk expanded)
          ;; Expansion failed or returned the same form — walk sub-forms.
          (cons head (mapcar #'easy-access-walk (cdr form))))))
     ;; Default: walk every sub-form for function calls and
     ;; special forms not explicitly handled above.
     (t (cons head (mapcar #'easy-access-walk (cdr form)))))))

(defun easy-access--walk-backquote (form)
  "Walk a backquote template FORM.
Only unquoted sub-expressions are walked; the template skeleton
is preserved verbatim."
  (cond
   ((atom form) form)
   ((eq (car-safe form) '\,)
    (list '\, (easy-access-walk (cadr form))))
   ((eq (car-safe form) '\,@)
    (list '\,@ (easy-access-walk (cadr form))))
   ((consp form)
    (cons (easy-access--walk-backquote (car form))
          (easy-access--walk-backquote (cdr form))))
   (t form)))

;;; -------------------------------------------------------------------------
;;; Built-in rewrites
;;; -------------------------------------------------------------------------
;;;
;;; Handlers for special forms whose shape fits the declarative DSL
;;; cleanly.  Registering them here (rather than hand-coding in
;;; `easy-access--walk-compound') means they sit in exactly the same
;;; registry as user-defined rewrites -- users can inspect, override,
;;; or remove them by name just like any other entry.
;;;
;;; Handlers kept imperative in `easy-access--walk-compound' are those
;;; the DSL cannot express without contorting: `let'/`let*' (per-binding
;;; shape dispatch), `cl-defmethod' (variable-length qualifier prefix),
;;; `cond'/`cl-case'/`condition-case' (clause-walking semantics),
;;; `pcase' (subtle quoted-symbol-pattern interaction), and backquote.

(defcall-rewrite define-advice-rewrite
  "Skip the name-spec (2nd element); rewrite only the body forms.
The name-spec contains keywords and arg-list shapes that must not
be rewritten."
  :source (define-advice fn name-spec . body)
  :target (define-advice fn name-spec (rewrite-each! body)))

(defcall-rewrite cl-defstruct-rewrite
  "Everything after the name is declarative; rewrite nothing.
Slot declarations look like `(slot-name default)' -- rewriting them
would turn defaults that happen to match the accessor shape into
`easy-access-lookup' calls."
  :source (cl-defstruct . rest)
  :target (cl-defstruct . rest))

(defcall-rewrite lambda-rewrite
  "Skip the arglist; rewrite the body forms."
  :source (lambda args . body)
  :target (lambda args (rewrite-each! body)))

(defcall-rewrite defun-rewrite
  "Skip the name and arglist; rewrite the body.
Shared entry for the `defun' family -- re-registering with the same
NAME at the call site upserts this rule, so users may override it
per-macro without racing the registration order."
  :source (defun name args . body)
  :target (defun name args (rewrite-each! body)))

(defcall-rewrite defmacro-rewrite
  "Skip the name and arglist; rewrite the body."
  :source (defmacro name args . body)
  :target (defmacro name args (rewrite-each! body)))

(defcall-rewrite defsubst-rewrite
  "Skip the name and arglist; rewrite the body."
  :source (defsubst name args . body)
  :target (defsubst name args (rewrite-each! body)))

(defcall-rewrite cl-defun-rewrite
  "Skip the name and arglist; rewrite the body."
  :source (cl-defun name args . body)
  :target (cl-defun name args (rewrite-each! body)))

(defcall-rewrite cl-defmacro-rewrite
  "Skip the name and arglist; rewrite the body."
  :source (cl-defmacro name args . body)
  :target (cl-defmacro name args (rewrite-each! body)))

(defcall-rewrite cl-defsubst-rewrite
  "Skip the name and arglist; rewrite the body.
`cl-defsubst' macroexpands into a `cl-defun' form, which our
`cl-defun-rewrite' already handles -- but catching it up-front avoids
walking the arglist during the fraction of a millisecond between
declaration and expansion."
  :source (cl-defsubst name args . body)
  :target (cl-defsubst name args (rewrite-each! body)))

(defcall-rewrite iter-defun-rewrite
  "Skip the name and arglist; rewrite the body.
`iter-defun' is from `generator.el' and has the shape of a regular
`defun' plus `iter-yield' in the body."
  :source (iter-defun name args . body)
  :target (iter-defun name args (rewrite-each! body)))

(defcall-rewrite pcase-dolist-rewrite
  "Skip the pattern; rewrite the sequence expression and the body.
The binding has shape `(PAT EXPR)' -- PAT is a match spec, EXPR is
code."
  :source (pcase-dolist (pat expr) . body)
  :target (pcase-dolist (pat (rewrite! expr)) (rewrite-each! body)))

(defcall-rewrite pcase-lambda-rewrite
  "Skip the parameter patterns; rewrite the body."
  :source (pcase-lambda pats . body)
  :target (pcase-lambda pats (rewrite-each! body)))

;;; -------------------------------------------------------------------------
;;; Hooks and advice
;;; -------------------------------------------------------------------------

(defvar easy-access--saved-load-read-function nil
  "Saved value of `load-read-function' before `easy-access-mode' enabled.
Restored on mode disable.")

(defun easy-access--read (stream)
  "Read one form from STREAM, walk it, return the rewritten form.
Replacement for `read' installed as `load-read-function' when the
mode is active."
  (let ((form (read stream)))
    (easy-access-walk form)))

(defun easy-access--advice-preceding-sexp (form)
  "Filter-return advice on `elisp--preceding-sexp'.
Walks FORM through `easy-access-walk' before returning it.  This is
the hook that makes `C-x C-e' and `C-M-x' honour easy-access syntax
-- both commands read the form via `elisp--preceding-sexp'."
  (easy-access-walk form))

(defun easy-access--advice-eval-args (args)
  "Filter-args advice on `eval'.
Walks (car ARGS) through `easy-access-walk'.  Catches `M-:' and
any Lisp-level call to `eval' with a bare accessor form.  Idempotent
on already-walked forms."
  (cons (easy-access-walk (car args)) (cdr args)))

(defun easy-access--install ()
  "Install all hooks and advice that activate easy-access syntax.
Called by `easy-access-mode' when the mode turns on."
  (setq easy-access--saved-load-read-function load-read-function)
  (setq load-read-function #'easy-access--read)
  (advice-add 'elisp--preceding-sexp :filter-return
              #'easy-access--advice-preceding-sexp)
  (advice-add 'eval :filter-args #'easy-access--advice-eval-args))

(defun easy-access--uninstall ()
  "Remove all hooks and advice installed by `easy-access--install'.
Called by `easy-access-mode' when the mode turns off.  Symmetric
with install -- restores `load-read-function' to its pre-install
value and removes every advice."
  (setq load-read-function easy-access--saved-load-read-function)
  (setq easy-access--saved-load-read-function nil)
  (advice-remove 'elisp--preceding-sexp
                 #'easy-access--advice-preceding-sexp)
  (advice-remove 'eval #'easy-access--advice-eval-args))

;;; -------------------------------------------------------------------------
;;; Minor mode
;;; -------------------------------------------------------------------------

;;;###autoload
(define-minor-mode easy-access-mode
  "Globally enable Clojure-style accessor syntax in Emacs Lisp.

When enabled:

 * Files loaded via `load' / `load-file' / `require' have their forms
   walked through `easy-access-walk' before evaluation.
 * Interactive evaluation commands (`C-x C-e', `C-M-x', `M-:') walk
   the form before evaluation.

When disabled, every hook and advice is removed -- semantics revert
to stock Elisp.

This is a GLOBAL mode.  There is no per-buffer variant because the
extension changes how Elisp is read and evaluated everywhere, not
just in one buffer.

See `easy-access-walk' for the rewrite rules, `easy-access-lookup'
for the runtime dispatch, and the package commentary for the
rationale."
  :global t
  :lighter easy-access-lighter
  :group 'easy-access
  (if easy-access-mode
      (easy-access--install)
    (easy-access--uninstall)))

;;; -------------------------------------------------------------------------
;;; Debugging helpers
;;; -------------------------------------------------------------------------

;;;###autoload
(defun easy-access-expand (form)
  "Show the `easy-access-walk' expansion of FORM in a help buffer.
Interactively, reads FORM from the minibuffer.  Useful for
debugging rewrites and for teaching."
  (interactive "xForm: ")
  (let ((expanded (easy-access-walk form)))
    (with-output-to-temp-buffer "*easy-access expansion*"
      (princ (format "Original:\n  %S\n\nExpanded:\n  %S\n"
                     form expanded)))))

(provide 'easy-access)
;;; easy-access.el ends here
