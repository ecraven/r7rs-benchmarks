;; alexpander.scm: a macro expander for scheme.
;; $Id: alexpander.scm,v 1.65 2007/11/05 02:50:34 al Exp $

;; Copyright 2002-2004,2006,2007 Al Petrofsky <alexpander@petrofsky.org>

;; LICENSING (3-clause BSD or GNU GPL 2 and up)

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;   Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;; 
;;   Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.
;; 
;;   Neither the name of the author nor the names of its contributors
;;     may be used to endorse or promote products derived from this
;;     software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; Alternatively, you may redistribute, use, or modify this software
;; according to the terms of the GNU General Public License as
;; published by the Free Software Foundation (fsf.org); either version
;; 2, or (at your option) any later version.

;; INTRODUCTION:

;; This file implements a macro-expander for r5rs scheme (plus some
;; interesting extensions).  There is no magic here to hook this into
;; your native eval system: this is a simple data-in, data-out program
;; that takes a macro-using program represented as scheme data and
;; produces an equivalent macro-free program represented as scheme
;; data.

;; This is mostly intended as a demonstration.  Although it certainly
;; could be useful for adding macros to a simple scheme system that
;; lacks any macros, it may not be feasible to get it to interact
;; properly with a low-level macro system or a module system.

;; The expander is written in portable r5rs scheme, except for one use
;; of the pretty-print procedure which you can easily comment out.

;; To try it out, just load the file and execute (alexpander-repl).
;; Skip to the "BASIC USAGE" section for more information.

;; To find the latest version of this program, try here:
;;    http://petrofsky.org/src/alexpander.scm
;;
;; To find older versions or the log messages between versions, try here:
;;    http://petrofsky.org/src/RCS/alexpander.scm,v

;; If you are wondering what "r5rs scheme" is, see: 
;;    Richard Kelsey, William Clinger, and Jonathan Rees, "Revised^5
;;    report on the algorithmic language Scheme", Higher-Order and
;;    Symbolic Computation, 11(1):7-105, 1998.  Available at:
;;       PDF: http://www-swiss.ai.mit.edu/~jaffer/r5rs.pdf
;;       LaTeX source: ftp://swiss.csail.mit.edu/pub/scheme-reports/r5rs.tar.gz

;; EXTENSIONS:

;; The expander supports all the features of the r5rs macro system,
;; plus several extensions in the way syntaxes can be specified and
;; used, which are best summarized in BNF:

;; Modified r5rs productions:

;;   <expression> ---> <variable> | <literal> | <procedure call>
;;                   | <lambda expression> | <conditional> | <assignment>
;;                   | <derived expression> | <macro use> | <macro block>
;;                   | <keyword>
;;
;;   <syntax definition> ---> (define-syntax <keyword> <syntax or expression>)
;;                          | (begin <syntax definition>*)
;;                          | <macro use>
;;
;;   <syntax spec> ---> (<keyword> <syntax or expression>)
;;
;;   <macro use> ---> (<syntax> <datum>*)
;;
;;   <definition> ---> (define <variable> <expression>)
;;                   | (define (<variable> <def formals>) <body>)
;;                   | (define <expression>)
;;                   | (begin <definition>*)
;;                   | <macro use>
;;                   | <syntax definition>
;;
;;   <command or definition> ---> <command> | <definition>
;;                              | (begin <command or definition>*)
;;                              | <top-level macro block>
;;                              | <macro use>

;; New productions:

;;   <syntax or expression> ---> <syntax> | <expression>
;;
;;   <syntax> ---> <transformer spec>
;;               | <keyword>
;;               | <macro use>
;;               | <syntax macro block>
;;
;;   <syntax macro block> ---> (<syntax-only block stuff> <syntax>)
;;
;;   <top-level macro block>
;;       ---> (<syntax-only block stuff> <command or definition>)
;;
;;   <syntax-only block stuff>
;;      ---> <let-or-letrec-syntax> (<syntax spec>*) <syntax definition>*
;;
;;   <let-or-letrec-syntax> ---> let-syntax | letrec-syntax


;; These extensions all have the obvious meaning.

;; Okay, I'll elaborate on that a little bit.  Consider the intializer
;; position of a syntax definition and the head position of a
;; list-format expression:

;;   (define-syntax <keyword> <xxx>)

;;   (<yyy> <foo>*)

;; In r5rs, <xxx> must be a transformer.  <Yyy> may be an expression,
;; in which case the enclosing expression is taken to be a procedure
;; call and the <foo>s are the expressions for the operands, or <yyy>
;; may be a keyword bound to a syntax (a builtin or transformer), in
;; which case the <foo>s are processed according to that syntax.

;; The core generalization in our system is that both <xxx> and <yyy>
;; may be any type of expression or syntax.  The four forms of syntax
;; allowed are: a transformer (as allowed in the <xxx> position in
;; r5rs), a keyword (as allowed in the <yyy> position in r5rs), a
;; macro use that expands into a syntax, and a macro block (let-syntax
;; or letrec-syntax) whose body is a syntax.

;; Some examples:
;; 
;;  ;; a macro with a local macro
;;  (let-syntax ((foo (let-syntax ((bar (syntax-rules () ((bar x) (- x)))))
;;                      (syntax-rules () ((foo) (bar 2))))))
;;    (foo))
;;  => -2
;;
;;  ;; an anonymous let transformer, used directly in a macro call.
;;  ((syntax-rules ()
;;     ((let ((var init) ...) . body)
;;      ((lambda (var ...) . body)
;;       init ...)))
;;   ((x 1) (y 2))
;;   (+ x y))
;;  => 3
;;
;;  ;; a keyword used to initialize a keyword
;;  (let-syntax ((q quote)) (q x)) => x
;;
;;  ;; Binding a keyword to an expression (which could also be thought
;;  ;; of as creating a macro that is called without arguments).
;;  (let ((n 0))
;;    (let-syntax ((x (set! n (+ n 1))))
;;      (begin x x x n)))
;;  => 3
;;
;;  (let-syntax ((x append)) ((x x))) => ()


;; Internal syntax definitions.

;; Internal syntax definitions are supported wherever they would make
;; sense (see the BNF), and they have the letrec-syntax semantics you
;; would expect.  It is legal for the initializer of an internal
;; variable definition to use one of the internal syntax definitions
;; in the same body:

;; (let ()
;;   (define x (y))
;;   (define-syntax y (syntax-rules () ((y) 1)))
;;   x)
;; => 1

;; It's also legal for internal syntax definitions to be mutually
;; recursive transformers, but it is an error for the expansion of a
;; syntax definition's initializer to require the result of another
;; initializer:

;; (let ()
;;   (define-syntax m1 (syntax-rules () ((m1) #f) ((m1 . args) (m2 . args))))
;;   (define-syntax m2 (syntax-rules () ((m2 arg . args) (m1 . args))))
;;   (m1 foo bar baz))
;; => #f

;; (let ()
;;   (define-syntax simple-transformer
;;     (syntax-rules ()
;;       ((simple-transformer pattern template)
;;        (syntax-rules () (pattern template)))))
;;   (define-syntax m (simple-transformer (m x) (- x)))
;;   (m 1))
;; => error ("Premature use of keyword bound by an internal define-syntax")

;; (let ()
;;   (define-syntax simple-transformer
;;     (syntax-rules ()
;;       ((simple-transformer pattern template)
;;        (syntax-rules () (pattern template)))))
;;   (let ()
;;     (define-syntax m (simple-transformer (m x) (- x)))
;;     (m 1)))
;; => -1


;; Top-level macro blocks.

;; At the top level, if a macro block (i.e., a let-syntax or
;; letrec-syntax form) has only one body element, or if all of the
;; body elements before the last one are internal syntax definitions,
;; then the last body element need not be an expression (as would be
;; required in r5rs).  Instead, it may be anything allowed at top
;; level: an expression, a definition, a begin sequence of top-level
;; forms, or another macro block containing a top-level form.

;;   (let-syntax ((- quote))
;;     (define x (- 1)))
;;
;;   (list x (- 1))
;;   => (1 -1)

;; Note that, unlike the similar extension in Chez scheme 6.0, this is
;; still r5rs-compatible, because we only treat definitions within the
;; last body element as top-level definitions (and r5rs does not allow
;; internal definitions within a body's last element, even if it is a
;; begin form):

;;   (define x 1)
;;   (define (f) x)
;;   (let-syntax ()
;;     (define x 2)
;;     (f))
;;   => 1, in r5rs and alexpander, but 2 in Chez scheme

;;   (define x 1)
;;   (define (f) x)
;;   (let-syntax ()
;;     (begin
;;       (define x 2)
;;       (f)))
;;   => 2, in alexpander and in Chez scheme, but an error in r5rs.


;; Syntax-rules ellipsis

;; Per SRFI-46, syntax-rules transformers can specify the
;; identifier to be used as the ellipsis (such a specification is
;; treated as a hygienic binding), and a list pattern may contain
;; subpatterns after an ellipsis as well as before it:

;;   <transformer spec> ---> (syntax-rules (<identifier>*) <syntax rule>*)
;;              | (syntax-rules <ellipsis> (<identifier>*) <syntax rule>*)
;;   
;;   <syntax rule> ---> (<pattern> <template>)
;;   
;;   <pattern> ---> <pattern identifier>
;;                | (<pattern>*)
;;                | (<pattern>+ . <pattern>)
;;                | (<pattern>* <pattern> <ellipsis> <pattern>*)
;;                | #(<pattern>*)
;;                | #(<pattern>* <pattern> <ellipsis> <pattern>*)
;;                | <pattern datum>
;;   
;;   <pattern identifier> ---> <identifier>
;;   
;;   <ellipsis> ---> <identifier>


;; Expressions among internal definitions.

;; A definition of the form (define <expression>) causes the
;; expression to be evaluated at the conclusion of any enclosing set
;; of internal definitons.  That is, at top level, (define
;; <expression>) is equivalent to just plain <expression>.  As for
;; internal definitions, the following are equivalent:

;; (let ()
;;   (define v1 <init1>)
;;   (define <expr1>)
;;   (define <expr2>)
;;   (define v2 <init2>)
;;   (define <expr3>)
;;   (begin
;;     <expr4>
;;     <expr5>))
;; 
;; (let ()
;;   (define v1 <init1>)
;;   (define v2 <init2>)
;;   (begin
;;     <expr1>
;;     <expr2>
;;     <expr3>
;;     <expr4>
;;     <expr5>))

;; (Yes, it would probably be better to have a separate builtin for
;; this rather than to overload define.)

;; This feature makes it possible to implement a define-values that
;; works properly both at top-level and among internal definitions:

;; (define define-values-temp #f)
;;
;; (define-syntax define-values
;;   (syntax-rules ()
;;     ((define-values (var ...) init)
;;      (begin
;;        (define define-values-temp (call-with-values (lambda () init) list))
;;        (define var #f) ...
;;        (define
;;          (set!-values (var ...) (apply values define-values-temp)))))))

;; (Set!-values is implementable using just r5rs features and is left
;; as an exercise.)

;; When used among internal definitions, the definition of
;; define-values-temp in define-values's output creates a local
;; binding, and thus the top-level binding of define-values-temp is
;; irrelevant.  When used at top-level, the definition of
;; define-values-temp in the output does not create a binding.
;; Instead, it mutates the top-level binding of define-values-temp.
;; Thus, all top-level uses of define-values share a single temp
;; variable.  For internal-definition-level uses of define-values, a
;; single shared temp would not be sufficient, but things work out
;; okay because hygienic renaming causes each such use to create a
;; distinct temp variable.

;; The version below works the same way, but hides from the top-level
;; environment the temp that is shared by top-level uses of
;; define-values.  For a bit of tutorial and rationale about this
;; technique, see usenet article
;; <8765tos2y9.fsf@radish.petrofsky.org>:

;; (define-syntax define-values
;;   (let-syntax ((temp (syntax-rules ())))
;;     (syntax-rules ()
;;       ((define-values (var ...) init)
;;        (begin
;;          (define temp (call-with-values (lambda () init) list))
;;          (define var #f) ...
;;          (define (set!-values (var ...) (apply values temp))))))))


;; Improved nested unquote-splicing.  

;; Quasiquote is extended to make commas and comma-ats distributive
;; over a nested comma-at, as in Common Lisp's backquote.  See my
;; 2004-09-03 usenet article <87pt53f9f2.fsf@radish.petrofsky.org>,
;; Bawden's 1999 quasiquotation paper, and Appendix C of Steele's
;; "Common Lisp the Language 2nd edition".

;;   <splicing unquotation 1> ---> ,@<qq template 0>
;;                               | (unquote-splicing <qq template 0>)
;;
;;   <splicing unquotation D> ---> ,@<qq template D-1>
;;                               | ,<splicing unquotaion D-1>
;;                               | ,@<splicing unquotaion D-1>
;;                               | (unquote-splicing <qq template D-1>)
;;                               | (unquote <splicing unquotaion D-1>)
;;                               | (unquote-splicing <splicing unquotaion D-1>)

;; When a comma at-sign and the expression that follows it are being
;; replaced by the elements of the list that resulted from the
;; expression's evaluation, any sequence of commas and comma at-signs
;; that immediately preceded the comma at-sign is also removed and is
;; added to the front of each of the replacements.

;;  (let ((x '(a b c))) ``(,,x ,@,x ,,@x ,@,@x))
;;  => `(,(a b c) ,@(a b c) ,a ,b ,c ,@a ,@b ,@c)
;;
;;  ``(,,@'() ,@,@(list))
;;  => `()
;;
;;  `````(a ,(b c ,@,,@,@(list 'a 'b 'c)))
;;  => ````(a ,(b c ,@,,@a ,@,,@b ,@,,@c))
;;  
;; (let ((vars '(x y)))
;;   (eval `(let ((x '(1 2)) (y '(3 4)))
;;            `(foo ,@,@vars))
;;         (null-environment 5)))
;; => (foo 1 2 3 4)


;; BASIC USAGE:

;; There are four supported ways to use this:

;;   1. (alexpander-repl)
;;      This starts a read-expand-print-loop.  Type in a program and
;;      see its expansion as you go.
;;
;;   2. (expand-program list-of-the-top-level-forms-of-a-program)
;;      Returns a list of the top-level forms of an equivalent
;;      macro-free program.
;;
;;   3. (expand-top-level-forms! forms mstore)
;;      Returns some macro-expanded forms and mutates mstore.
;;      To use this, first create an initial mutable store with
;;      (null-mstore).  Then you can pass a program in piecemeal, with
;;      the effects of top-level define-syntaxes saved in mstore
;;      between calls to expand-top-level-forms!.
;;
;;   4. (expand-top-level-forms forms store loc-n k)
;;      The purely-functional interface.
;;      This returns by making a tail call to k:
;;      (k expanded-forms new-store new-loc-n)
;;      Use null-store and null-loc-n for store and loc-n arguments
;;      when calling expand-top-level-forms with the first forms in a
;;      program.
;;
;; For options 3 and 4, you need to prepend null-output to the
;; resulting program.  Null-output contains some definitions like
;; (define _eqv?_7 eqv?), which create alternate names for some of the
;; builtin procedures.  These names are used by the standard case and
;; quasiquote macros so that they can keep working even if you
;; redefine one of the standard procedures.

;; The output programs use a small subset of the r5rs syntax, namely:
;; BEGIN, DEFINE, DELAY, IF, LAMBDA, LETREC, QUOTE, AND SET!.
;; Furthermore, begin is only used for expressions; lambdas and
;; letrecs always have a single body expression and no internal
;; definitions; and defines are always of the simple (define
;; <variable> <expression>) form.  If you want even simpler output,
;; with no letrecs, see expand-program-to-simple.

;; Any uses or definitions in the original program of a top-level
;; variable whose name begins with "_", or whose name is one of the
;; eight primitives in the output language, will be renamed.  This
;; will only cause a problem if the program is trying to use some
;; nonstandard library variable that starts with "_".  That is, even
;; though some of a program's top-level variable names may get
;; changed, any r5rs-conformant program will still be translated to an
;; equivalent macro-free r5rs program.


;; INTERNALS

;; [NOTE: this documentation is certainly not complete, and it kind of
;; dissolves after a few pages from verbose paragraphs into cryptic
;; sentence fragments.  Nonetheless, it might be enough to help
;; someone figure out the code.]

;; ENVIRONMENTS AND STORES

;; The two principal data structures are the environment and the
;; store.

;; These work similarly to the runtime environment and store described
;; in r5rs: in both that system and in ours, to determine the meaning
;; of an identifier, we lookup which location the environment
;; associates with the identifier, and then check what value the store
;; associates with that location.

;; In the runtime system, the identifiers mapped by the environment
;; are all variables, and the values in the store are the scheme
;; values the variables currently hold.  Environments may be locally
;; extended by LAMBDA to map some identifiers to new locations that
;; initially hold the values passed to the procedure.  Environments
;; may also be locally extended by internal DEFINE (a.k.a LETREC) to
;; map some identifiers to new locations that are empty and illegal to
;; access or SET! until the evaluation of all the initializers has
;; completed (at which time the results are stored into the
;; locations).  The store is modified when a SET! or top-level DEFINE
;; is evaluated, or when a set of internal DEFINE initializers'
;; evaluations completes, but environments are immutable.  The static
;; top-level environment maps every variable name to some location,
;; although most of these locations are illegal to access until the
;; evaluation of the initializer of the first top-level DEFINE of the
;; variable has completed.  (The exceptions are the locations to which
;; the standard procedure names are bound: these locations may be
;; accessed at any time, but they may not be SET! until after the
;; first top-level DEFINE of the procedure name.)

;; (R5rs actually does not completely specify how the top-level
;; environment works, and allows one to consider the top-level
;; environment to be dynamically extended, but the model I just
;; described fits within the r5rs parameters and plays well with our
;; macro system.  To recap: the difference between SET! and top-level
;; DEFINE is not that top-level DEFINE is able to create a new
;; binding, rather, the difference is that top-level DEFINE is allowed
;; to store into any location and SET! is not always allowed to store
;; into some locations.)

;; In our syntactic system, a value in the store may be either a
;; syntax (a builtin or a macro transformer), a variable name, or the
;; expanded code for an expression.  When we encounter a use of an
;; identifier, we go through the environment and the store to fetch
;; its value.  If the value is a variable name, we emit that variable
;; name.  If the value is some code, we emit that code.  If the value
;; is a syntax, we proceed according to the rules of that syntax.  As
;; in the runtime system, environments are immutable and the static
;; top-level environment is infinite.  Environments may be locally
;; extended by LAMBDA or internal DEFINE to map some identifiers to
;; new locations that hold variable names.  Environments may also be
;; extended by LET-SYNTAX to map some identifiers to new locations
;; that initially hold the syntaxes and/or code resulting from the
;; expansion of the initializers.  Lastly, environments may be
;; extended by internal DEFINE-SYNTAX (a.k.a LETREC-SYNTAX) to map
;; some identifiers to new locations that are empty and illegal to
;; access until the expansion of their initializers has completed (at
;; which time the resulting syntaxes and/or code are stored into the
;; locations).  The store is modified by top-level DEFINE and
;; DEFINE-SYNTAX, and when a set of internal DEFINE-SYNTAX
;; initializers' expansions completes.  The store is not altered by a
;; SET!, because a SET! does not change the fact that the identifier
;; is a variable: from our perspective a SET! of a variable is simply
;; a use of the variable.  A top-level DEFINE only alters the store if
;; an identifier whose location previously held a syntax is now being
;; defined as a variable.

;; The static top-level environment maps every name to some location.
;; Initially, the locations to which the environment maps the names of
;; the ten builtins (BEGIN DEFINE DEFINE-SYNTAX IF LAMBDA QUOTE SET!
;; DELAY LET-SYNTAX SYNTAX-RULES) hold as their values those builtin
;; syntaxes.  All other names are bound to locations that hold the
;; corresponding top-level variable name.

;; I said the top-level environment contains a binding for "every
;; name" rather than for "every identifier", because the new
;; identifiers created by a syntax-rules macro expansion are given
;; numbers rather than names, and the top-level environment has no
;; bindings for these.  If such an identifier is used in an
;; environment that does not bind it to any location, then the
;; location to which the template literal was bound in the environment
;; of the macro is used instead.  (To be prepared for such a
;; contingency, this location is stored along with the numeric id in
;; the "renamed-sid" (see below) that a macro expansion inserts into
;; the code.)

;; REPRESENTATION OF ENVIRONMENTS AND STORES

;; An environment is represented by an alist mapping ids to local
;; (non-top-level) locations.  All environments are derived from the
;; top-level environment, so any symbolic id not in the alist is
;; implicitly mapped to the corresponding top-level location.

;; An id (identifier) is what we bind to a location in an environment.
;; Original ids are the symbols directly occuring in the source code.
;; Renamed ids are created by macro expansions and are represented by
;; integers.

;; id: original-id | renamed-id
;; original-id: symbol
;; renamed-id: integer

;; The static top-level environment maps every symbol to a location.
;; For simplicity, each of those locations is represented by the
;; symbol that is bound to it.  All other locations (those created by
;; lambda, let-syntax, and internal definitions) are represented by
;; integers.

;; env: ((id . local-location) ...)
;; store: ((location . val) ...)
;; location: toplevel-location | local-location  ;; a.k.a. symloc and intloc.
;; toplevel-location: symbol
;; local-location: integer
;; val: variable | syntax | code
;; variable: #(toplevel-location) | #(symbol local-location)
;; code: (output) ; output is the expanded code for an expression.
;; syntax: builtin | transformer
;; builtin: symbol
;; transformer: (synrules env)
;; synrules: the unaltered sexp of the syntax-rules form.

;; REPRESENTATION OF THE CODE UNDERGOING EXPANSION (SEXPS).

;; Any variable named SEXP in the expander code holds a representation
;; of some code undergoing expansion.  It mostly looks like the
;; ordinary representation of scheme code, but it may contain some
;; identifiers that are encoded as two- or three-element vectors
;; called renamed-sids.  Any actual vector in the code will be
;; represented as a one-element vector whose element is a list of the
;; actual elements, i.e., each vector #(elt ...) is mapped to #((elt
;; ...)), so that we can distinguish these vectors from renamed-sids.

;; In contrast, a variable named OUTPUT is a bit of almost-finished
;; code.  In this format, symbols and vectors within a quote
;; expression are represented normally.  All variable names are
;; represented as vectors of the form #(symbol) or #(symbol integer).
;; These vectors are converted to suitable, non-clashing symbols by
;; the symbolize function, which is the final step of expansion.

;; A sid is the representation of an id within a sexp.
;; sid: original-id | renamed-sid

;; A renamed-sid includes the id's original name, which we will need
;; if the id gets used in a QUOTE expression.  The renamed-sid also
;; includes the location of the local binding (if any) of the template
;; literal that created the id: this is the location to use if the id
;; gets used freely (i.e., in an environment with no binding for it). 
;; renamed-sid: #(original-id renamed-id)
;;            | #(original-id renamed-id local-location)

;; Procedures that take a SEXP argument usually also take an ID-N
;; argument, which is the next higher number after the largest
;; renamed-id that occurs in the SEXP argument.  (This is to enable
;; adding new ids without conflict.)
;;
;; Similarly, a STORE argument is usually accompanied by a LOC-N
;; argument, which is the next higher number after the largest
;; local-location in the STORE argument.

;; SUMMARY OF MAJOR FUNCTIONS:

;; (lookup-sid sid env) => location
;; (lookup-location location store) => val | #f  ;; #f means letrec violation.
;; (lookup2 sid env store) => val ;; lookup-sid + lookup-location + fail if #f.
;; (extend-env env id location) => env
;; (extend-store store intloc val) => store
;; (substitute-in-store store loc val) => store
;; (compile-syntax-rules synrules env) => transformer
;; (apply-transformer trans sexp id-n env k) => (k sexp id-n)
;; (expand-any sexp id-n env store loc-n lsd? ek sk dk bk)
;;    => (ek output)
;;     | (sk syntax sexp store loc-n)
;;     | (dk builtin sexp id-n env store loc-n)
;;     | (bk sexp id-n env store loc-n)
;; (expand-expr sexp id-n env store loc-n) => output
;; (expand-val sexp id-n env store loc-n k) => (k val store loc-n)
;; (expand-top-level-sexps sexps store loc-n k)
;;   => (k outputs store loc-n)
;; (expand-body sexps id-n env store loc-n lsd? ek sk dk bk)
;;    => same as expand-any
;; (expand-syntax-bindings bindings id-n syntax-env ienv store loc-n k)
;;   => (k store loc-n)


(define (sid? sexp)          (or (symbol? sexp) (renamed-sid? sexp)))
(define (renamed-sid? sexp)  (and (vector? sexp) (< 1 (vector-length sexp))))
(define (svector? sexp)      (and (vector? sexp) (= 1 (vector-length sexp))))
(define (svector->list sexp) (vector-ref sexp 0))
(define (list->svector l) (vector l))

(define (make-sid name renamed-id location)
  (if (eq? name location)
      (vector name renamed-id)
      (vector name renamed-id location)))

(define (sid-name sid) (if (symbol? sid) sid (vector-ref sid 0)))
(define (sid-id sid)   (if (symbol? sid) sid (vector-ref sid 1)))
(define (sid-location sid)
  (if (symbol? sid) sid (vector-ref sid (if (= 2 (vector-length sid)) 0 2))))

(define (list1? x) (and (pair? x) (null?  (cdr x))))
(define (list2? x) (and (pair? x) (list1? (cdr x))))

;; Map-vecs does a deep map of x, replacing any vector v with (f v).
;; We assume that f never returns #f.
;; If a subpart contains no vectors, we don't waste space copying it.
;; (Yes, this is grossly premature optimization.)
(define (map-vecs f x)
  ;; mv2 returns #f if there are no vectors in x.
  (define (mv2 x)
    (if (vector? x)
	(f x)
	(and (pair? x)
	     (let ((a (car x)) (b (cdr x)))
	       (let ((a-mapped (mv2 a)))
		 (if a-mapped
		     (cons a-mapped (mv b))
		     (let ((b-mapped (mv2 b)))
		       (and b-mapped (cons a b-mapped)))))))))
  (define (mv x) (or (mv2 x) x))
  (mv x))

(define (wrap-vec v) (list->svector (wrap-vecs (vector->list v))))
(define (wrap-vecs input) (map-vecs wrap-vec input))
(define (unwrap-vec v-sexp)
  (if (= 1 (vector-length v-sexp))
      (list->vector (unwrap-vecs (svector->list v-sexp)))
      (vector-ref v-sexp 0)))
(define (unwrap-vecs sexp) (map-vecs unwrap-vec sexp))

;; The store maps locations to vals.
;; vals are variables, syntaxes, or code.

(define (make-code output) (list output))
(define (make-builtin name) name)
(define (make-transformer synrules env) (list synrules env))

(define (var? val) (vector? val))
(define (code? val) (list1? val))
(define (code-output code) (car code))

(define (syntax? val) (or (symbol? val) (list2? val)))

(define (builtin? syntax) (symbol? syntax))
(define (builtin-name builtin) builtin)

(define (transformer? syntax) (not (builtin? syntax)))
(define (transformer-synrules trans) (car trans))
(define (transformer-env trans) (cadr trans))

(define (acons key val alist) (cons (cons key val) alist))

(define empty-env '())
(define empty-store '())

;; Lookup-sid looks up a sid in an environment.
;; If there is no binding in the environment, then:
;;   1. For an original-id, we return the like-named location, because
;;      the static top-level environment maps every name to a location.
;;   2. For a renamed id, we return the location to which the template
;;      literal that created it was bound.
(define (lookup-sid sid env)
  (cond ((assv (sid-id sid) env) => cdr)
	;; This works for both cases 1 and 2 above.
	(else (sid-location sid))))

;; Lookup-location looks up a location in the store.
;; If there is no value explictly listed in the store, then:
;;   1. For a top-level (named) location, return a top-level variable.
;;   2. For a local location, return #f.  This can only happen for a
;;      location allocated by letrec-syntax or internal define-syntax
;;      and used before it is initialized,
;;      e.g. (letrec-syntax ((x x)) 1).
(define (lookup-location location store)
  (cond ((assv location store) => cdr)
	((symbol? location) (symloc->var location))
	(else #f)))

(define (lookup2 sid env store)
  (or (lookup-location (lookup-sid sid env) store)
      (error (string-append "Premature use of keyword bound by letrec-syntax"
			    " (or an internal define-syntax): ")
	     sid)))

(define (extend-env env id location) (acons id location env))
(define (extend-store store loc val) (acons loc val store))

;; Extend-store just adds to the front of the alist, whereas
;; substitute-in-store actually bothers to remove the old entry, and
;; to not add a new entry if it is just the default.
;; Substitute-in-store is only used by top-level define and
;; define-syntax.  Because nothing is ever mutated, we could just use
;; extend-store all the time, but we are endeavoring to keep down the
;; size of the store to make it more easily printed and examined.
(define (substitute-in-store store loc val)
  (let ((store (if (assv loc store)
		   (let loop ((store store))
		     (let ((p (car store)))
		       (if (eqv? loc (car p))
			   (cdr store)
			   (cons p (loop (cdr store))))))
		   store)))
    (if (and (symbol? loc) (eq? val (symloc->var loc)))
	store
	(acons loc val store))))

(define (make-var1 name/loc)
  (vector name/loc))
(define (make-var2 name loc)
  (vector name loc))
(define (var-name var)
  (vector-ref var 0))
(define (var-loc var)
  (vector-ref var (- (vector-length var) 1)))

(define (symloc->var sym)
  (make-var1 sym))

(define (intloc->var intloc sid)
  (make-var2 (sid-name sid) intloc))

(define (loc->var loc sid)
  (if (symbol? loc)
      (symloc->var loc)
      (intloc->var loc sid)))

(define (make-begin outputs)
  (if (list1? outputs) (car outputs) (cons 'begin outputs)))

(define (make-letrec bindings expr)
  (if (null? bindings) expr (list 'letrec bindings expr)))

(define (expand-lambda formals expr id-n env store loc-n)
  ;; (a b . c) => (a b c)
  (define (flatten-dotted x)
    (if (pair? x) (cons (car x) (flatten-dotted (cdr x))) (list x)))
  ;; (a b c) => (a b . c)
  (define (dot-flattened x)
    (if (null? (cdr x)) (car x) (cons (car x) (dot-flattened (cdr x)))))
  (let* ((dotted? (not (list? formals)))
	 (flattened (if dotted? (flatten-dotted formals) formals)))
    (define (check x)
      (or (sid? x) (error "Non-identifier: " x " in lambda formals: " formals))
      (if (member x (cdr (member x flattened)))
	  (error "Duplicate variable: " x " in lambda formals: " formals)))
    (begin
      (for-each check flattened)
      (let loop ((formals flattened) (rvars '())
		 (env env) (store store) (loc-n loc-n))
	(if (not (null? formals))
	    (let* ((var (intloc->var loc-n (car formals)))
		   (env (extend-env env (sid-id (car formals)) loc-n))
		   (store (extend-store store loc-n var)))
	      (loop (cdr formals) (cons var rvars) env store (+ 1 loc-n)))
	    (let* ((vars (reverse rvars))
		   (vars (if dotted? (dot-flattened vars) vars)))
	      (list vars (expand-expr expr id-n env store loc-n))))))))

(define (check-syntax-bindings bindings)
  (or (list? bindings) (error "Non-list syntax bindings list: " bindings))
  (for-each (lambda (b) (or (and (list2? b) (sid? (car b)))
			    (error "Malformed syntax binding: " b)))
	    bindings)
  (do ((bs bindings (cdr bs)))
      ((null? bs))
    (let ((dup (assoc (caar bs) (cdr bs))))
      (if dup (error "Duplicate bindings for a keyword: "
		     (car bs) " and: " dup)))))

;; returns (k store loc-n)
(define (expand-syntax-bindings bindings id-n syntax-env ienv store loc-n k)
  (let loop ((bs bindings) (vals '()) (store store) (loc-n loc-n))
    (if (not (null? bs))
	(expand-val (cadar bs) id-n syntax-env store loc-n
	  (lambda (val store loc-n)
	    (loop (cdr bs) (cons val vals) store loc-n)))
	(let loop ((store store) (vals (reverse vals)) (bs bindings))
	  (if (not (null? vals))
	      (let* ((loc (lookup-sid (caar bs) ienv))
		     (store (extend-store store loc (car vals))))
		(loop store (cdr vals) (cdr bs)))
	      (k store loc-n))))))


;; (expand-any sexp id-n env store loc-n lsd? ek sk dk bk)
;;
;; Ek, sk, dk, and bk are continuations for expressions, syntaxes,
;; definitions and begins:
;;
;; If sexp is an expression, returns (ek output).
;;
;; If sexp is a syntax, returns (sk syntax error-sexp store loc-n).
;;   The error-sexp is just for use in error messages if the syntax is
;;   subsequently misused.  It is the sid that was bound to the
;;   syntax, unless the syntax is an anonymous transformer, as in
;;   ((syntax-rules () ((_ x) 'x)) foo), in which case the error-sexp
;;   will be the entire syntax-rules form.
;;
;; If sexp is a definition, returns (dk builtin sexp id-n env store
;;   loc-n), where builtin is define or define-syntax.
;;
;; If sexp is a begin, returns (bk sexp id-n env store loc-n).
;;
;; The car of the sexp passed to dk or bk is just for error reporting:
;; it is the sid that was bound to begin, define, or define-syntax.
;;
;; Expand-any signals an error if a malformed e, s, d, or b is
;; encountered.  It also signals an error if ek, sk, dk, or bk is #f
;; and the corresponding thing is encountered; however, if a begin is
;; encountered and bk is #f, the begin is expanded as an expression
;; and passed to ek.
;;
;; lsd? == Let-Syntax around Definitions is okay.  If lsd? is #f and a
;; let-syntax is encountered, it is assumed to start an expression or
;; syntax, so if ek and sk are #f an error will be signalled.  lsd? is
;; only true at top-level.  (Let-syntax around internal definitions is
;; just too semantically bizarre.)
(define (expand-any sexp id-n env store loc-n lsd? ek sk dk bk)
  (define (get-k k sexp name)
    (or k (error (string-append name " used in bad context: ")
		 sexp)))
  (define (get-ek sexp) (get-k ek sexp "Expression"))
  (define (get-sk sexp) (get-k sk sexp "Syntax"))
  (define (get-dk sexp) (get-k dk sexp "Definition"))
  (define (get-bk sexp) (get-k bk sexp "Begin"))
  (let again ((sexp sexp) (id-n id-n) (store store) (loc-n loc-n))
    (define (expand-subexpr sexp) (expand-expr sexp id-n env store loc-n))
    (define (handle-syntax-use syntax head store loc-n)
      (let* ((tail (cdr sexp)) (sexp (cons head tail)))
	(if (transformer? syntax)
	    (apply-transformer syntax sexp id-n env
	      (lambda (sexp id-n) (again sexp id-n store loc-n)))
	    (let ((builtin (builtin-name syntax)) (len (length tail)))
	      (define (handle-macro-block)
		(or ek sk lsd?
		    (error "Macro block used in bad context: " sexp))
		(or (>= len 2) (error "Malformed macro block: " sexp))
		(let ((bindings (car tail)) (body (cdr tail)))
		  (check-syntax-bindings bindings)
		  (let loop ((bs bindings) (loc-n loc-n) (ienv env))
		    (if (not (null? bs))
			(loop (cdr bs) (+ loc-n 1)
			      (extend-env ienv (sid-id (caar bs)) loc-n))
			(expand-syntax-bindings
			  bindings id-n env ienv store loc-n
			  (lambda (store loc-n)
			    (expand-body body id-n ienv store loc-n
					 lsd? ek sk
					 (and lsd? dk) (and lsd? bk))))))))
	      (define (handle-expr-builtin)
		(define (expr-assert test)
		  (or test (error "Malformed " builtin " expression: " sexp)))
		(cons builtin
		      (case builtin
			((lambda)
			 (expr-assert (= len 2))
			 (expand-lambda (car tail) (cadr tail)
					id-n env store loc-n))
			((quote)
			 (expr-assert (= len 1))
			 (list (unwrap-vecs (car tail))))
			((set!)
			 (expr-assert (and (= len 2) (sid? (car tail))))
			 (let ((var (lookup2 (car tail) env store)))
			   (or (var? var)
			       (error "Attempt to set a keyword: " sexp))
			   (list var (expand-subexpr (cadr tail)))))
			((if)
			 (expr-assert (<= 2 len 3))
			 (map expand-subexpr tail))
			((delay)
			 (expr-assert (= len 1))
			 (list (expand-subexpr (car tail)))))))
	      (case builtin
		((let-syntax) (handle-macro-block))
		((syntax-rules)
		 (if (< len 1) (error "Empty syntax-rules form: " sexp))
		 (let ((syn (compile-syntax-rules sexp env)))
		   ((get-sk sexp) syn sexp store loc-n)))
		((begin)
		 (or ek (get-bk sexp))
		 (cond (bk (bk sexp id-n env store loc-n))
		       ((null? tail) (error "Empty begin expression: " sexp))
		       (else (ek (make-begin (map expand-subexpr tail))))))
		((define define-syntax)
		 (or (and (= 2 len) (sid? (car tail)))
		     (and (= 1 len) (eq? builtin 'define))
		     (error "Malformed definition: " sexp))
		 ((get-dk sexp) builtin sexp id-n env store loc-n))
		(else (get-ek sexp) (ek (handle-expr-builtin))))))))
    (define (handle-combination output)
      (ek (if (and (pair? output) (eq? 'lambda (car output))
		   (null? (cadr output)) (null? (cdr sexp)))
	      ;; simplifies ((lambda () <expr>)) to <expr>
	      (caddr output)
	      (cons output (map expand-subexpr (cdr sexp))))))
    ;;(pretty-print `(expand-any/again ,sexp))
    (cond ((sid? sexp)
	   (let ((val (lookup2 sexp env store)))
	     (if (syntax? val)
		 ((get-sk sexp) val sexp store loc-n)
		 ((get-ek sexp) (if (code? val) (code-output val) val)))))
	  ((and (pair? sexp) (list? sexp))
	   (expand-any (car sexp) id-n env store loc-n #f
	     (and ek handle-combination) handle-syntax-use #f #f))
	  ((or (number? sexp) (boolean? sexp) (string? sexp) (char? sexp))
	   ((get-ek sexp) sexp))
	  (else (error (cond ((pair? sexp) "Improper list: ")
			     ((null? sexp) "Empty list: ")
			     ((vector? sexp) "Vector: ")
			     (else "Non-S-Expression: "))
		       sexp
		       " used as an expression, syntax, or definition.")))))

;; Expands an expression or syntax and returns (k val store loc-n).
(define (expand-val sexp id-n env store loc-n k)
  (expand-any sexp id-n env store loc-n #f
    (lambda (output) (k (make-code output) store loc-n))
    (lambda (syn error-sexp store loc-n) (k syn store loc-n))
    #f #f))

(define (expand-expr sexp id-n env store loc-n)
  (expand-any sexp id-n env store loc-n #f (lambda (x) x) #f #f #f))

;; args and return are as in expand-any.
(define (expand-body sexps id-n env store loc-n lsd? ek sk dk bk)
  ;; Expand-def expands a definition or begin sequence, adds entries
  ;; to the vds and sds lists of variable and syntax definitons, adds
  ;; entries to the exprs list of expressions from (define <expr>)
  ;; forms, extends env, and returns (k vds sds exprs id-n env store
  ;; loc-n).
  ;; If sexp is an expression, we just return (dek output) instead.
  (define (expand-def sexp vds sds exprs id-n env store loc-n k dek)
    (define (dk builtin sexp id-n env store loc-n)
      (or ek (eq? builtin 'define-syntax)
	  (error "Non-syntax definition in a syntax body: " sexp))
      (if (list2? sexp) ;; A (define <expression>) form.
	  (k vds sds (cons (cadr sexp) exprs) id-n env store loc-n)
          (let* ((sid (cadr sexp))
		 (id (sid-id sid))
		 (env (extend-env env id loc-n)))
	    (define (check def)
	      (if (eqv? id (sid-id (cadr def)))
		  (error "Duplicate internal definitions: "
			 def " and: " sexp)))
	    (begin
	      (for-each check sds)
	      (for-each check vds)
	      (case builtin
		((define-syntax)
		 (k vds (cons sexp sds) exprs id-n env store (+ loc-n 1)))
		((define)
		 (let* ((var (intloc->var loc-n sid))
			(store (extend-store store loc-n var))
			(loc-n (+ loc-n 1)))
		   (k (cons sexp vds) sds exprs id-n env store loc-n))))))))
    (define (bk sexp id-n env store loc-n)
      (let loop ((sexps (cdr sexp)) (vds vds) (sds sds) (exprs exprs)
		 (id-n id-n) (env env) (store store) (loc-n loc-n) (dek dek))
	(if (null? sexps)
	    (k vds sds exprs id-n env store loc-n)
	    (expand-def (car sexps) vds sds exprs id-n env store loc-n
	      (lambda (vds sds exprs id-n env store loc-n)
		(loop (cdr sexps) vds sds exprs id-n env store loc-n #f))
	      (and dek (lambda (out)
			 (define (expand-one sexp)
			   (expand-expr sexp id-n env store loc-n))
			 (let ((rest (map expand-one (cdr sexps))))
			   (dek (make-begin (cons out rest))))))))))
    (expand-any sexp id-n env store loc-n #f dek #f dk bk))
  (let loop ((first (car sexps)) (rest (cdr sexps))
	     (vds '()) (sds '()) (exprs '())
	     (id-n id-n) (env env) (store store) (loc-n loc-n))
    (define (finish-body boundary-exp-output)
      (expand-syntax-bindings (map cdr sds) id-n env env store loc-n
	(lambda (store loc-n)
	  (define (iexpand sexp) (expand-expr sexp id-n env store loc-n))
	  (define (expand-vd vd)
	    (list (lookup2 (cadr vd) env store) (iexpand (caddr vd))))
	  (if (and (null? rest) (null? vds) (null? exprs))
	      (expand-any first id-n env store loc-n lsd? ek sk dk bk)
	      (ek (make-letrec
		    (map expand-vd (reverse vds))
		    (let ((body-exprs-output
			   (if (null? rest)
			       (list (iexpand first))
			       (cons boundary-exp-output
				     (map iexpand rest)))))
		      (make-begin (append (map iexpand (reverse exprs))
					  body-exprs-output)))))))))
    (if (null? rest)
	(finish-body #f)
	(expand-def first vds sds exprs id-n env store loc-n
	  (lambda (vds sds exprs id-n env store loc-n)
	    (loop (car rest) (cdr rest) vds sds exprs id-n env store loc-n))
	  (and ek finish-body)))))


;; Returns (k outputs store loc-n).
(define (expand-top-level-sexps sexps store loc-n k)
  (define (finish store loc-n acc)
    (k (reverse acc) store loc-n))
  ;; expand adds stuff to acc and returns (k store loc-n acc)
  (let expand ((sexps sexps) (id-n 0) (env empty-env)
	       (store store) (loc-n loc-n) (acc '()) (k finish))
    (if (null? sexps)
	(k store loc-n acc)
	(let ((rest (cdr sexps)))
	  (define (ek output)
	    (expand rest id-n env store loc-n (cons output acc) k))
	  (define (dk builtin sexp id-n* env* store loc-n)
	    (if (list2? sexp) ;; A (define <expression>) form.
		(ek (expand-expr (cadr sexp) id-n* env* store loc-n))
	        (let* ((tail (cdr sexp))
		       (sid (car tail))
		       (loc (lookup-sid sid env*))
		       (init (cadr tail)))
		  (if (eq? builtin 'define)
		      (let* ((expr (expand-expr init id-n* env* store loc-n))
			     (var (loc->var loc sid))
			     (acc (cons (list 'define var expr) acc))
			     (store (substitute-in-store store loc var)))
			(expand rest id-n env store loc-n acc k))
		      (expand-val init id-n* env* store loc-n
			(lambda (val store loc-n)
			  (let ((store (substitute-in-store store loc val)))
			    (expand rest id-n env store loc-n acc k))))))))
	  (define (bk sexp id-n* env* store loc-n)
	    (expand (cdr sexp) id-n* env* store loc-n acc
		    (lambda (store loc-n acc)
		      (expand rest id-n env store loc-n acc k))))
	  (expand-any (car sexps) id-n env store loc-n #t ek #f dk bk)))))

;; Returns (k expanded-forms store loc-n).
(define (expand-top-level-forms forms store loc-n k)
  (define (finish outputs store loc-n)
    (define (finish1 output)
      ;; You can leave out the unrename-locals call if you want to.
      (symbolize (unrename-locals output)))
    (k (map finish1 outputs) store loc-n))
  (expand-top-level-sexps (wrap-vecs forms) store loc-n finish))

;; Compile-syntax-rules:
;; This doesn't actually compile, it just does verification.
;; Detects all possible errors:
;;   pattern literals list is not a list of identifiers
;;   ellipsis in literals list
;;   rule is not a two-element list
;;   missing pattern keyword (pattern is not a pair whose car is an identifier)
;;   duplicate pattern variable
;;   ellipsis not preceded by a pattern or template.
;;   list or vector pattern with multiple ellipses.
;;   improper list pattern with an ellipsis.
;;   variable instance in template not at sufficient ellipsis depth.
;;   template ellipsis closes no variables.
(define (compile-syntax-rules synrules env)
  (define ellipsis-id (and (pair? (cddr synrules))
			   (sid? (cadr synrules))
			   (sid-id (cadr synrules))))
  (define (ellipsis? x)
    (and (sid? x)
	 (if ellipsis-id
	     (eqv? ellipsis-id (sid-id x))
	     (eq? '... (lookup-sid x env)))))

  (define (check-lit lit)
    (or (sid? lit)
	(error "Non-id: " lit " in literals list of: " synrules))
    (if (ellipsis? lit)
	(error "Ellipsis " lit " in literals list of: " synrules)))

  (let* ((rest (if ellipsis-id (cddr synrules) (cdr synrules)))
	 (pat-literal-sids (car rest))
	 (rules (cdr rest))
	 (pat-literals
	  (begin (or (list? pat-literal-sids)
		     (error "Pattern literals list is not a list: "
			    pat-literal-sids))
		 (for-each check-lit pat-literal-sids)
		 (map sid-id pat-literal-sids))))

    (define (ellipsis-pair? x)
      (and (pair? x) (ellipsis? (car x))))

    (define (check-ellipses pat/tmpl in-template?)
      (define (bad-ellipsis x reason)
	(error (string-append reason ": ")
	       x
	       (if in-template? " in template: " " in pattern: ")
	       pat/tmpl))

      (define (multi-ellipsis-error x)
	(bad-ellipsis x "List or vector pattern with multiple ellipses"))

      (define (ellipsis/tail-error x)
	(bad-ellipsis x "Improper list pattern with an ellipsis"))

      (define (ellipsis-follows x thing)
	(bad-ellipsis x (string-append "Ellipsis following " thing)))
      
      (let ((x (if in-template? pat/tmpl (cdr pat/tmpl))))
	(if in-template?
	    (if (ellipsis? x)
		(ellipsis-follows x "nothing"))
	    (cond ((ellipsis? x)
		   (ellipsis-follows pat/tmpl "a '.'"))
		  ((ellipsis-pair? x)
		   (ellipsis-follows pat/tmpl "the pattern keyword"))))
	(let check ((x x))
	  (cond ((pair? x)
		 (if (ellipsis? (car x)) (ellipsis-follows x "a '('"))
		 (check (car x))
		 (if (ellipsis? (cdr x)) (ellipsis-follows x "a '.'"))
		 (if (ellipsis-pair? (cdr x))
		     (cond ((ellipsis? (cddr x))
			    (ellipsis-follows (cdr x) "a '.'"))
			   ((ellipsis-pair? (cddr x))
			    (ellipsis-follows (cdr x) "an ellipsis"))
			   (in-template? (check (cddr x)))
			   (else (or (list? x) (ellipsis/tail-error x))
				 (for-each (lambda (y)
					     (if (ellipsis? y)
						 (multi-ellipsis-error x))
					     (check y))
				  (cddr x))))
			
		     (check (cdr x))))
		((svector? x)
		 (let ((elts (svector->list x)))
		   (if (ellipsis-pair? elts)
		       (ellipsis-follows x "a '#('")
		       (check elts))))))))

    ;; Returns an alist: ((pat-var . depth) ...)
    (define (make-pat-env pat)
      (let collect ((x (cdr pat)) (depth 0) (l '()))
	(cond ((sid? x)
	       (let ((id (sid-id x)))
		 (cond ((memv id pat-literals) l)
		       ((assv id l)
			(error "Duplicate pattern var: " x
			       " in pattern: " pat))
		       (else (acons id depth l)))))
	      ((vector? x) (collect (svector->list x) depth l))
	      ((pair? x)
	       (if (ellipsis-pair? (cdr x))
		   (collect (car x) (+ 1 depth) (collect (cddr x) depth l))
		   (collect (car x) depth (collect (cdr x) depth l))))
	      (else l))))

    ;; Checks var depths.
    (define (check-var-depths tmpl pat-env)
      (define (depth-error x)
	(error "Pattern var used at bad depth: " x " in template: " tmpl))
      (define (close-error x)
	(error "Template ellipsis closes no variables: " x
	       " in template: " tmpl))
      ;; collect returns #t if any vars occurred at DEPTH
      (let collect ((x tmpl) (depth 0))
	(cond ((sid? x)
	       (let ((p (assv (sid-id x) pat-env)))
		 (and p
		      (let* ((pat-depth (cdr p))
			     (same-depth? (= depth pat-depth)))
			(if (and (positive? pat-depth) (not same-depth?))
			    (depth-error x))
			same-depth?))))
	      ((vector? x) (collect (svector->list x) depth))
	      ((pair? x)
	       (let* ((ellip? (ellipsis-pair? (cdr x)))
		      (car-closed? (collect (car x)
					    (if ellip? (+ 1 depth) depth)))
		      (cdr-closed? (collect ((if ellip? cddr cdr) x)
					    depth)))
		 (and ellip? (not car-closed?) (close-error x))
		 (or car-closed? cdr-closed?)))
	      (else #f))))

			 
    ;; Checks rule and returns a list of the template literal ids.
    (define (check-rule rule)
      (or (list2? rule) (error "Malformed syntax rule: " rule))
      (let ((pat (car rule)) (tmpl (cadr rule)))
	(or (and (pair? pat) (sid? (car pat)))
	    (error "Malformed pattern: " pat))
	(check-ellipses pat #f)
	(check-ellipses tmpl #t)
	(let ((pat-env (make-pat-env pat)))
	  (check-var-depths tmpl pat-env)
	  (let collect ((x tmpl) (lits '()))
	    (cond ((ellipsis? x) lits)
		  ((sid? x) (if (assv (sid-id x) pat-env)
				lits
				(cons (sid-id x) lits)))
		  ((vector? x) (collect (svector->list x) lits))
		  ((pair? x) (collect (car x) (collect (cdr x) lits)))
		  (else lits))))))

    ;; Reduce-env: this optional hack cuts down on the clutter when
    ;; manually examining the store.  Returns an environment with only
    ;; the bindings we need: those of pattern or template literals,
    ;; and those of identifiers named "..." that prevent a "..." from
    ;; being treated as an ellipsis, e.g. in
    ;; (let ((... 1)) ((syntax-rules () ((_) ...)))) => 1.
    (define (reduce-env lits)
      (define (list-dots-ids x ids)
	(cond ((sid? x) (if (eq? '... (sid-location x))
			    (cons (sid-id x) ids)
			    ids))
	      ((vector? x) (list-dots-ids (svector->list x) ids))
	      ((pair? x) (list-dots-ids (car x) (list-dots-ids (cdr x) ids)))
	      (else ids)))
      (let loop ((ids (if ellipsis-id lits (list-dots-ids rules lits)))
		 (reduced-env empty-env))
	(if (null? ids)
	    reduced-env
	    (loop (cdr ids)
		  (let ((id (car ids)))
		    (cond ((and (not (assv id reduced-env)) (assv id env))
			   => (lambda (binding) (cons binding reduced-env)))
			  (else reduced-env)))))))

    (let* ((lits (apply append pat-literals (map check-rule rules)))
	   (env (reduce-env lits)))
      (make-transformer synrules env))))


;; returns (k sexp id-n)
(define (apply-transformer transformer sexp id-n env k)
  (let* ((synrules (transformer-synrules transformer))
	 (mac-env (transformer-env transformer))
	 (ellipsis-id (and (sid? (cadr synrules))
			   (sid-id (cadr synrules))))
	 (rest (if ellipsis-id (cddr synrules) (cdr synrules)))
	 (pat-literals (map sid-id (car rest)))
	 (rules (cdr rest)))

    (define (pat-literal? id)     (memv id pat-literals))
    (define (not-pat-literal? id) (not (pat-literal? id)))
    (define (ellipsis-pair? x)    (and (pair? x) (ellipsis? (car x))))
    (define (ellipsis? x)
      (and (sid? x)
	   (if ellipsis-id
	       (eqv? ellipsis-id (sid-id x))
	       (eq? '... (lookup-sid x mac-env)))))

    ;; List-ids returns a list of the non-ellipsis ids in a
    ;; pattern or template for which (pred? id) is true.  If
    ;; include-scalars is false, we only include ids that are
    ;; within the scope of at least one ellipsis.
    (define (list-ids x include-scalars pred?)
      (let collect ((x x) (inc include-scalars) (l '()))
	(cond ((sid? x) (let ((id (sid-id x)))
			  (if (and inc (pred? id)) (cons id l) l)))
	      ((vector? x) (collect (svector->list x) inc l))
	      ((pair? x)
	       (if (ellipsis-pair? (cdr x))
		   (collect (car x) #t (collect (cddr x) inc l))
		   (collect (car x) inc (collect (cdr x) inc l))))
	      (else l))))
    
    
    (define (matches? pat)
      (let match ((pat pat) (sexp (cdr sexp)))
	(cond ((sid? pat)
	       (or (not (pat-literal? (sid-id pat)))
		   (and (sid? sexp)
			(eqv? (lookup-sid pat mac-env)
			      (lookup-sid sexp env)))))
	      ((svector? pat)
	       (and (svector? sexp)
		    (match (svector->list pat) (svector->list sexp))))
	      ((not (pair? pat)) (equal? pat sexp))
	      ((ellipsis-pair? (cdr pat))
	       (let skip ((p (cddr pat)) (s sexp))
		 (if (pair? p)
		     (and (pair? s) (skip (cdr p) (cdr s)))
		     (let match-cars ((sexp sexp) (s s))
		       (if (pair? s)
			   (and (match (car pat) (car sexp))
				(match-cars (cdr sexp) (cdr s)))
			   (match (cddr pat) sexp))))))
	      (else (and (pair? sexp)
			 (match (car pat) (car sexp))
			 (match (cdr pat) (cdr sexp)))))))

    ;; Returns an alist binding pattern variables to parts of the input.
    ;; An ellipsis variable is bound to a list (or a list of lists, etc.).
    (define (make-bindings pat)
      (let collect ((pat pat) (sexp (cdr sexp)) (bindings '()))
	(cond ((and (sid? pat) (not (pat-literal? (sid-id pat))))
	       (acons (sid-id pat) sexp bindings))
	      ((svector? pat)
	       (collect (svector->list pat) (svector->list sexp) bindings))
	      ((not (pair? pat)) bindings)
	      ((ellipsis-pair? (cdr pat))
	       (let* ((tail-len (length (cddr pat)))
		      (tail (list-tail sexp (- (length sexp) tail-len)))
		      (matches (reverse (list-tail (reverse sexp) tail-len)))
		      (vars (list-ids (car pat) #t not-pat-literal?)))
		 (define (collect1 match)
		   (map cdr (collect (car pat) match '())))
		 (append (apply map list vars (map collect1 matches))
			 (collect (cddr pat) tail bindings))))
	      (else (collect (car pat) (car sexp)
			     (collect (cdr pat) (cdr sexp) bindings))))))

    ;; Remove duplicates from a list, using eqv?.
    (define (remove-dups l)
      (let loop ((l l) (result '()))
	(if (null? l)
	    result
	    (loop (cdr l)
		  (let ((elt (car l)))
		    (if (memv elt result) result (cons elt result)))))))

    (define (expand-template pat tmpl top-bindings)
      (define tmpl-literals
	(remove-dups (list-ids tmpl #t
			       (lambda (id) (not (assv id top-bindings))))))
      (define ellipsis-vars (list-ids pat #f not-pat-literal?))
      (define (list-ellipsis-vars subtmpl)
	(list-ids subtmpl #t (lambda (id) (memv id ellipsis-vars))))
      (define (expand tmpl bindings)
	(let expand-part ((tmpl tmpl))
	  (cond
	   ((sid? tmpl)
	    (let ((id (sid-id tmpl)))
	      (cond ((assv id bindings) => cdr)
		    ((assv id top-bindings) => cdr)
		    (else
		     (let ((index (+ -1 (length (memv id tmpl-literals))))
			   (location (lookup-sid tmpl mac-env)))
		       (make-sid (sid-name tmpl) (+ id-n index) location))))))
	   ((vector? tmpl)
	    (list->svector (expand-part (svector->list tmpl))))
	   ((pair? tmpl)
	    (if (ellipsis-pair? (cdr tmpl))
		(let ((vars-to-iterate (list-ellipsis-vars (car tmpl))))
		  (define (lookup var) (cdr (assv var bindings)))
		  (define (expand-using-vals . vals)
		    (expand (car tmpl) (map cons vars-to-iterate vals)))
		  (let ((val-lists (map lookup vars-to-iterate)))
		    (if (or (null? (cdr val-lists))
			    (apply = (map length val-lists)))
			(append (apply map expand-using-vals val-lists)
				(expand-part (cddr tmpl)))
			(error "Unequal sequence lengths for pattern vars: "
			       vars-to-iterate " in macro call: " sexp))))
		(cons (expand-part (car tmpl)) (expand-part (cdr tmpl)))))
	   (else tmpl))))
      (k (expand tmpl top-bindings) (+ id-n (length tmpl-literals))))

    (let loop ((rules rules))
      (if (null? rules)
	  (error "No matching rule for macro use: " sexp)
	  (let* ((rule (car rules)) (pat (cdar rule)) (tmpl (cadr rule)))
	    (if (matches? pat)
		(expand-template pat tmpl (make-bindings pat))
		(loop (cdr rules))))))))

;; Unrename-locals: undoes most of the unnecessary renamings of local
;; variables.
;;
;; When the expander generates variables for lambdas and letrecs, it
;; generates variables with integer locations that are unique
;; throughout the region of the variable.  These numbers in effect
;; rename all the variables so that no variable ever shadows another.
;; This may be necessary if hygienic macro expansion caused some
;; variable named "foo" to be accessed from inside the region of
;; another binding named "foo".
;;
;; However, in most instances, this renaming is unnecessary.
;; Unrename-locals converts variables of the form #(foo n) to plain
;; #(foo) wherever this can be done.
;;
;; (This step is strictly optional.  It just makes the final expansion
;; more readable.)
(define (unrename-locals output)
  ;; Some operations on sets represented as lists with no duplicates.
  (define (subtract-lists a b) (a-minus-b-plus-c a b '()))
  (define (merge-lists a b) (a-minus-b-plus-c a b b))
  ;; a-minus-b-plus-c returns the union of (A - B) and C.
  ;; Assumes that (A - B) and C are disjoint.
  (define (a-minus-b-plus-c a b c)
    (if (null? a)
	c
	(let ((x (car a))
	      (y (a-minus-b-plus-c (cdr a) b c)))
	  (if (member x b) y (cons x y)))))

  ;; (a b . c) => (a b c)
  (define (flatten-dotted x)
    (if (pair? x) (cons (car x) (flatten-dotted (cdr x))) (list x)))

  (define (flatten-vars x) (if (list? x) x (flatten-dotted x)))

  ;; Compute-free-vars computes the free variables of an expression
  ;; and annotates all the local binding forms within the expression
  ;; with lists of their free variables.
  ;;
  ;; Specifically, (compute-free-vars expr k) returns (k free
  ;; annexpr), where FREE is a list of the variables that occur freely
  ;; in EXPR, and ANNEXPR is like EXPR, but with every (lambda formals
  ;; body) or (letrec bindings body) in EXPR replaced by (lambda free*
  ;; formals body) or (letrec free* formals body), where FREE* is a
  ;; list of the free variables of the lambda or letrec expression as
  ;; a whole (i.e., the free variables of the body and initializers,
  ;; minus any that are bound by the bindings or formals).  Example:

  ;; (compute-free-vars
  ;;   '(#(f 5) (lambda (#(a 7) #(b 8)) (#(g) #(a 7) #(b 2))))
  ;;   list)
  ;; => ((#(f 5) #(g) #(b 2))
  ;;     (#(f 5) (lambda (#(g) #(b 2)) (#(a 7) #(b 8)) (#(g) #(a 7) #(b 2)))))
  (define (compute-free-vars expr k)
    (cond ((var? expr)
	   (k (list expr) expr))
	  ((pair? expr)
	   (case (car expr)
	     ((quote) (k '() expr))
	     ((lambda)
	      (compute-free-vars
	       (cddr expr)
	       (lambda (free annexpr)
		 (let* ((vars (cadr expr))
			(free (subtract-lists free (flatten-vars vars))))
		   (k free `(lambda ,free ,vars . ,annexpr))))))
	     ((letrec)
	      (compute-free-vars
	       `(lambda ,(map car (cadr expr)) ,(cdr expr))
	       (lambda (free annexpr)
		 (k free `(letrec ,free . ,(cadddr annexpr))))))
	     (else (compute-free-vars
		    (car expr)
		    (lambda (free1 annexpr1)
		      (compute-free-vars
		       (cdr expr)
		       (lambda (free2 annexpr2)
			 (k (merge-lists free1 free2)
			    (cons annexpr1 annexpr2)))))))))
	  (else (k '() expr))))

  ;; Unrename: (unrename annexpr changes)
  ;;
  ;; The ANNEXPR argument must be annotated with free-variable lists
  ;; for all the lambdas and letrecs.  CHANGES is an alist of
  ;; unrenamings that we've made in the environment of ANNEXPR.  The
  ;; return value is a non-annotated expression with most of the local
  ;; variables unrenamed.
  ;;
  ;; When processing a lambda form and deciding whether to unrename
  ;; one of the variables that it binds, there are two kinds of
  ;; unrenamings we must avoid:
  ;;
  ;; 1. Avoid unrenamings that conflict with one of the free variables
  ;;    and thereby improperly shadow the binding to which the free
  ;;    variable is supposed to refer.  That is, don't convert (lambda
  ;;    (#(x 1)) #(x)) to (lambda (#(x)) #(x)).
  ;;
  ;; 2. Avoid unrenaming a variable to the same name as one of the
  ;;    other variables in the same set of bindings.  That is, even
  ;;    though converting (lambda (#(x 1) #(x 2)) 'foo) to (lambda
  ;;    (#(x) #(x)) 'foo) would not shadow any binding that is needed
  ;;    by the body, it would still cause an error.

  (define (unrename annexpr changes)
    (define (unrename-var var)
      (if (symbol? (var-loc var)) var (make-var1 (var-name var))))
    (cond ((var? annexpr)
	   (cond ((assoc annexpr changes) => cdr)
		 (else annexpr)))
	  ((pair? annexpr)
	   (case (car annexpr)
	     ((quote) annexpr)
	     ((lambda)
	      (let* ((vars (flatten-vars (caddr annexpr)))
		     (avoid (unrename (cadr annexpr) changes)))
		(let scan-vars ((vars vars) (avoid avoid) (changes changes))
		  (if (null? vars)
		      (cons 'lambda (unrename (cddr annexpr) changes))
		      (let* ((var (car vars))
			     (urvar (unrename-var var)))
			(if (member urvar avoid)
			    (scan-vars (cdr vars) (cons var avoid) changes)
			    (scan-vars (cdr vars)
				       (cons urvar avoid)
				       (acons var urvar changes))))))))
	     ((letrec)
	      (let ((foo `(lambda ,(cadr annexpr) ,(map car (caddr annexpr))
				  ,(cddr annexpr))))
		`(letrec . ,(caddr (unrename foo changes)))))
	     (else (cons (unrename (car annexpr) changes)
			 (unrename (cdr annexpr) changes)))))
	  (else annexpr)))
  
  (compute-free-vars output (lambda (free annexpr) (unrename annexpr '()))))

;; Some tests to exercise the unrenamer:
;;
;; (let-syntax ((foo (syntax-rules () ((foo) x)))) (lambda (x y) (foo) x y))
;; expands-to=> (lambda (_x_50 y) (begin x _x_50 y))
;; 
;; (let-syntax ((foo (syntax-rules () ((foo x) (lambda (x y) z))))) (foo y))
;; expands-to=> (lambda (y _y_51) z)
;; 
;; (let-syntax ((foo (syntax-rules () ((foo x) (lambda (y x) z))))) (foo y))
;; expands-to=> (lambda (y _y_51) z)


;; Symbolize uses var->symbol to convert all the variables in the
;; output from the expander into symbols.
(define (symbolize output)
  (cond ((var? output)
	 (var->symbol output))
	((pair? output)
	 (if (eq? 'quote (car output))
	     output
	     (cons (symbolize (car output))
		   (symbolize (cdr output)))))
	(else output)))

;; Var->symbol converts a variable to a symbol.
;;
;; Note that there are two parts to a variable (a name and a
;; location), and any two variables with the same location always have
;; the same name.  The location is either a symbol or an integer.  If
;; the location is a symbol, then the name is always the same symbol.
;; The variables are represented by one-element or two-element vectors
;; of the form #(symbolic-location) or #(name integer-location),
;; e.g. #(apple) or #(cherry 17).
;;
;; In other words, the location is what identifies a variable, and the
;; name is just additional information that was retained in order to
;; help us choose, for the final expansion, a symbol that bears some
;; relationship to the symbol that was used in the original code.
;;
;; The requirements for the var->symbol mapping are:
;;
;; 1. All variables must be mapped to distinct symbols.
;;
;; 2. No variable may be mapped to the name of a builtin.
;;
;; 3. Variables with symbolic locations that are the name of a
;;    standard procedure must be mapped to that name, i.e. #(CAR) must
;;    map to CAR.
;;
;; Desired additional properties are:
;;
;; 4. As many variables as possible should be simply mapped to their
;;    names.
;;
;; 5. Any variables not mapped to their names should at least be
;;    mapped to a symbol that includes the name as a substring of the
;;    chosen name.
;;
;; The scheme we use is to follow these rules:
;;
;;    1. #(foo number) => _foo_number
;;
;;    2. #(foo) => foo
;;       Except that:
;;          (a) if foo is a builtin; or
;;          (b) if foo starts with an underscore;
;;       then #(foo) => _foo_
;;
;; Without rule 2(a), we would violate requirement 2, and without rule
;; 2(b), we would violate requirement 1 (because #(foo 1) and
;; #(_foo_1) would both map to _foo_1).

(define (var->symbol var)
  (let* ((sym (var-name var))
	 (str (symbol->string sym))
	 (loc (var-loc var)))
    (if (number? loc)
	(let ((n (number->string loc)))
	  (string->symbol (string-append "_" str "_" n)))
	(if (case sym
	      ((begin define delay if lambda letrec quote set!) #t)
	      (else (and (positive? (string-length str))
			 (char=? #\_ (string-ref str 0)))))
	    (string->symbol (string-append "_" str "_"))
	    sym))))

(define builtins-store
  (let loop ((bs '(begin define define-syntax if lambda quote set! delay
			 let-syntax syntax-rules))
	     (store empty-store))
    (if (null? bs)
	store
	(loop (cdr bs)
	      (extend-store store (car bs) (make-builtin (car bs)))))))

;; null-prog is the preamble that defines all the standard macros that
;; are in the null-store.  (The "null-" name prefix was chosen to
;; correspond to the name of r5rs's null-environment procedure, even
;; though the null-store is far from empty.)
(define null-prog
  '((define-syntax letrec-syntax
      (let-syntax ((let-syntax let-syntax) (define-syntax define-syntax))
	(syntax-rules ()
	  ((_ ((kw init) ...) . body)
	   (let-syntax ()
	     (define-syntax kw init) ... (let-syntax () . body))))))
    (let-syntax ()
      (define-syntax multi-define
	(syntax-rules ()
	  ((_ definer (id ...) (init ...))
	   (begin (definer id init) ...))))
      ;; Define-protected-macros defines a set of macros with a
      ;; private set of bindings for some keywords and variables.  If
      ;; any of the keywords or variables are later redefined at
      ;; top-level, the macros will continue to work.  The first
      ;; argument to define-protected-macros is let-syntax or
      ;; letrec-syntax; if it is letrec-syntax, then the macros will
      ;; also have a private set of bindings for one another, and
      ;; recursive calls made by the macros to themselves or to one
      ;; another will not be affected by later top-level
      ;; redefinitions.
      ;;
      ;; The private binding for a saved variable is created by a
      ;; let-syntax, using a dummy syntax as the initializer.  We
      ;; later assign a value to it using a top-level define (and thus
      ;; change the status of the binding from keyword to variable).
      (define-syntax dummy (syntax-rules ()))
      (define-syntax define-protected-macros
	(syntax-rules (define-syntax)
	  ((_ let/letrec-syntax (saved-kw ...) (saved-var ...)
	      (define-syntax kw syntax) ...)
	   ((let-syntax ((saved-kw saved-kw) ... (saved-var dummy) ...)
	      (let/letrec-syntax ((kw syntax) ...)
		(syntax-rules ()
		  ((_ top-level-kws top-level-vars)
		   (begin
		     (multi-define define (saved-var ...) top-level-vars)
		     (multi-define define-syntax top-level-kws (kw ...)))))))
	    (kw ...) (saved-var ...)))))
      (begin
	;; Prototype-style define and lambda with internal definitions
	;; are implemented in define-protected-macros with let-syntax
	;; scope so that they can access the builtin define and lambda.
	(define-protected-macros let-syntax (lambda define let-syntax) ()
	  (define-syntax lambda
	    (syntax-rules ()
	      ((lambda args . body)
	       (lambda args (let-syntax () . body)))))
	  (define-syntax define
	    (syntax-rules ()
	      ((_ expr) (define expr))
	      ((_ (var . args) . body)
	       (define var (lambda args (let-syntax () . body))))
	      ((_ var init) (define var init))))
	  ;; We put letrec here so that it can use the builtin define,
	  ;; and won't accidentally allow things like:
	  ;; (letrec (((f) 1)) (f)) => 1
	  (define-syntax letrec
	    (syntax-rules ()
	      ((_ ((var init) ...) . body)
	       ;; The lambda ensures letrec is only used for expressions.
	       ((lambda ()
		  (let-syntax ()
		    (define var init) ... (let-syntax () . body))))))))
	(define-protected-macros letrec-syntax
	    (if lambda quote begin define letrec) (eqv?)
	  (define-syntax let
	    (syntax-rules ()
	      ((_ ((var init) ...) . body)
	       ((lambda (var ...) . body)
		init ...))
	      ((_ name ((var init) ...) . body)
	       ((letrec ((name (lambda (var ...) . body)))
		  name)
		init ...))))
	  (define-syntax let*
	    (syntax-rules ()
	      ((_ () . body) (let () . body))
	      ((let* ((var init) . bindings) . body)
	       (let ((var init)) (let* bindings . body)))))
	  (define-syntax do
	    (let-syntax ((do-step (syntax-rules () ((_ x) x) ((_ x y) y))))
	      (syntax-rules ()
		((_ ((var init step ...) ...)
		    (test expr ...)
		    command ...)
		 (let loop ((var init) ...)
		   (if test
		       (begin (if #f #f) expr ...)
		       (begin command ...
			      (loop (do-step var step ...) ...))))))))
	  (define-syntax case
	    (letrec-syntax
		((compare
		  (syntax-rules ()
		    ((_ key ()) #f)
		    ((_ key (datum . data))
		     (if (eqv? key 'datum) #t (compare key data)))))
		 (case
		  (syntax-rules (else)
		    ((case key) (if #f #f))
		    ((case key (else result1 . results))
		     (begin result1 . results))
		    ((case key ((datum ...) result1 . results) . clauses)
		     (if (compare key (datum ...))
			 (begin result1 . results)
			 (case key . clauses))))))
	      (syntax-rules ()
		((_ expr clause1 clause ...)
		 (let ((key expr))
		   (case key clause1 clause ...))))))
	  (define-syntax cond
	    (syntax-rules (else =>)
	      ((_) (if #f #f))
	      ((_ (else . exps)) (let () (begin . exps)))
	      ((_ (x) . rest) (or x (cond . rest)))
	      ((_ (x => proc) . rest)
	       (let ((tmp x)) (cond (tmp (proc tmp)) . rest)))
	      ((_ (x . exps) . rest)
	       (if x (begin . exps) (cond . rest)))))
	  (define-syntax and
	    (syntax-rules ()
	      ((_) #t)
	      ((_ test) (let () test))
	      ((_ test . tests) (if test (and . tests) #f))))
	  (define-syntax or
	    (syntax-rules ()
	      ((_) #f)
	      ((_ test) (let () test))
	      ((_ test . tests) (let ((x test)) (if x x (or . tests)))))))
	;; Quasiquote uses let-syntax scope so that it can recognize
	;; nested uses of itself using a syntax-rules literal (that
	;; is, the quasiquote binding that is visible in the
	;; environment of the quasiquote transformer must be the same
	;; binding that is visible where quasiquote is used).
	(define-protected-macros let-syntax
	    (lambda quote let) (cons append list vector list->vector map)
	  (define-syntax quasiquote
	    (let-syntax
		((tail-preserving-syntax-rules
		  (syntax-rules ()
		    ((tail-preserving-syntax-rules literals
			((subpattern ...) (subtemplate ...))
			...)
		     (syntax-rules literals
		       ((subpattern ... . tail) (subtemplate ... . tail))
		       ...)))))

	      (define-syntax qq
		(tail-preserving-syntax-rules
		    (unquote unquote-splicing quasiquote)
		  ((_ ,x        ())      (do-next x))
		  ((_ (,@x . y) ())      (qq y () make-splice x))
		  ((_ `x         depth)  (qq x (depth) make-list 'quasiquote))
		  ((_ ,x        (depth)) (qq x  depth  make-list 'unquote))
		  ((_ (,x  . y) (depth)) (qq-nested-unquote (,x  . y) (depth)))
		  ((_ (,@x . y) (depth)) (qq-nested-unquote (,@x . y) (depth)))
		  ((_ ,@x        depth)  (unquote-splicing-error ,@x))
		  ((_ (x . y)    depth)  (qq x depth qq-cdr y depth make-pair))
		  ((_ #(x y ...) depth)  (qq (x) depth qq-cdr #(y ...) depth
					     make-vector-splice))
		  ((_ x          depth)  (do-next 'x))))

	      (define-syntax do-next
		(syntax-rules ()
		  ((_ expr original-template) expr)
		  ((_ expr next-macro . tail) (next-macro expr . tail))))

	      (define-syntax unquote-splicing-error
		(syntax-rules ()
		  ((_ ,@x stack ... original-template)
		   (unquote-splicing-error (,@x in original-template)))))
	      
	      (define-syntax qq-cdr
		(tail-preserving-syntax-rules ()
		  ((_ car cdr depth combiner) (qq cdr depth combiner car))))
	      
	      (define-syntax qq-nested-unquote
		(tail-preserving-syntax-rules ()
		  ((_ ((sym x) . y) (depth))
		   (qq (x) depth make-map sym qq-cdr y (depth) make-splice))))
	      
	      (define-syntax make-map
		(tail-preserving-syntax-rules (quote list map lambda)
		  ((_ '(x) sym) (do-next '((sym x))))
	          ((_ (list x) sym) (do-next (list (list 'sym x))))
		  ((_ (map (lambda (x) y) z) sym)
		   (do-next (map (lambda (x) (list 'sym y)) z)))
		  ((_ expr sym)
		   (do-next (map (lambda (x) (list 'sym x)) expr)))))
								     
	      (define-syntax make-pair
		(tail-preserving-syntax-rules (quote list)
		  ((_ 'y 'x) (do-next '(x . y)))
		  ((_ '() x) (do-next (list x)))
		  ((_ (list . elts) x) (do-next (list x . elts)))
		  ((_ y x) (do-next (cons x y)))))
						  
	      (define-syntax make-list
		(tail-preserving-syntax-rules (quote)
		  ((_ y x) (make-pair '() y make-pair x))))
							   
	      (define-syntax make-splice
		(tail-preserving-syntax-rules ()
		  ((_ '() x) (do-next x))
		  ((_ y x) (do-next (append x y)))))
						    
	      (define-syntax make-vector-splice
		(tail-preserving-syntax-rules (quote list vector list->vector)
		  ((_ '#(y ...) '(x))     (do-next '#(x y ...)))
		  ((_ '#(y ...) (list x)) (do-next (vector x 'y ...)))
		  ((_ '#()      x)        (do-next (list->vector x)))
		  ((_ '#(y ...) x)        (do-next (list->vector
						     (append x '(y ...)))))
		  ((_ y '(x))             (make-vector-splice y (list 'x)))
		  ((_ (vector y ...) (list x)) (do-next (vector x y ...)))
		  ((_ (vector y ...) x)   (do-next (list->vector
						     (append x (list y ...)))))
		  ((_ (list->vector y) (list x)) (do-next (list->vector
							    (cons x y))))
		  ((_ (list->vector y) x) (do-next (list->vector
						     (append x y))))))
							   
	      (syntax-rules ()
		((_ template) (let () (qq template () template)))))))))))

(define null-stuff (expand-top-level-forms null-prog builtins-store 0 list))
(define null-output (car null-stuff))
(define null-store  (cadr null-stuff))
(define null-loc-n  (caddr null-stuff))

(define (expand-program forms)
  (expand-top-level-forms forms null-store null-loc-n
    (lambda (outputs store loc-n) (append null-output outputs))))

;; an mstore is a mutable store.
(define (null-mstore) (cons null-store null-loc-n))

(define (expand-top-level-forms! forms mstore)
  (expand-top-level-forms forms (car mstore) (cdr mstore)
    (lambda (outputs store loc-n)
      (set-car! mstore store)
      (set-cdr! mstore loc-n)
      outputs)))

(define repl-mstore (null-mstore))

;; alexpander-repl: a read-expand-print loop.
;; If called with an argument, resumes a previous session.
;; Top-level vectors are interpreted as directives to the repl:
;;   #(show loc ...) shows values stored in the locations.
;;   #(dump) dumps the whole store.
;;   #(restart) restarts.

(define (alexpander-repl . resume?)
  (define (pp x) (pretty-print x))
  (define (restart)
    (set! repl-mstore (null-mstore))
    (for-each pp null-output))
  (define (repl)
;;    (display "expander> ")
    (let ((form (read)))
      (if (not (eof-object? form))
	  (begin
	    (if (vector? form)
		(let ((l (vector->list form)))
		  (case (car l)
		    ((dump) (pp (car repl-mstore)))
		    ((show)
		     (for-each (lambda (loc)
				 (pp (assv loc (car repl-mstore))))
			       (cdr l)))
		    ((restart) (restart))))
		(for-each pp (expand-top-level-forms! (list form)
						      repl-mstore)))
	    (repl)))))
  (begin
    (if (null? resume?) (restart))
    (repl)))

;; If you don't have a pretty-print, here's an unsatisfying substitute:
;;(define (pretty-print x) (write x) (newline))


;; If you want to target an even simpler output language, with no
;; LETREC, DELAY, or BEGIN, then one inefficient way to do so is to
;; use a second pass, like this:
(define (expand-program-to-simple forms)
  (define startup
    '(begin
       (define-syntax letrec
	 (syntax-rules ()
	   ((letrec ((var init) ...) expr)
	    (let ((var #f) ...)
	      (let ((var (let ((tmp init)) (lambda () (set! var tmp))))
		    ...
		    (thunk (lambda () expr)))
		(begin (var) ... (thunk)))))))
       (define-syntax delay
	 (syntax-rules ()
	   ((delay expr)
	    (let ((result #f) (thunk (lambda () expr)))
	      (lambda ()
		(if thunk (let ((x (thunk)))
			    (if thunk (begin (set! result x)
					     (set! thunk #f)))))
		result)))))
       (define (force x) (x))
       (define-syntax begin
	 (syntax-rules ()
	   ((begin x) x)
	   ((begin x . y)
	    ((lambda (ignore) (begin . y)) x))))))
  (expand-program (cons startup (expand-program forms))))


;; Rest of file is a junkyard of thoughts.

'
(begin
  (define (file->list file)
    (define (f) (let ((x (read))) (if (eof-object? x) '() (cons x (f)))))
    (with-input-from-file file f))

  (define (evali expr) (eval expr (interaction-environment)))

  (define (check-expander filename)
    (let* ((src (file->list filename))
	   (out1 (begin (for-each evali src) (expand-program src))))
      (begin (for-each evali out1) (equal? out1 (expand-program src)))))
)

;; r2rs-style currying define.
'(define-syntax define
   (let-syntax ((old-define define))
     (letrec-syntax
	 ((new-define
	   (syntax-rules ()
	     ((_ (var-or-prototype . args) . body)
	      (new-define var-or-prototype (lambda args . body)))
	     ((_ var expr) (old-define var expr)))))
       new-define)))

'(define-syntax define
   (let-syntax ((old-define define))
     (define-syntax new-define
       (syntax-rules ()
	 ((_ (var-or-prototype . args) . body)
	  (new-define var-or-prototype (lambda args . body)))
	 ((_ var expr) (old-define var expr))))
     new-define))

'(let ((multiplier 2))
   (define ((curried-* x) y) (* x y))
   (map (curried-* multiplier) '(3 4 5)))

;; Notes:

;; TODO:
;;
;; * fluid-let-syntax
;; * revamp error handling:
;;     add an error continuation to expand-top-level-forms.
;;     keep a backtrace.


;; Are these legal in r5rs?
;;   (let () (define if 1) (+ if) if) => 1
;;   (let () (define if 1) (set! if 2) if) => 2
;; The 2002 version of the expander didn't allow them.
;;
;; First solution to above problem didn't fix this:
;;   (let () (define if 1) ((let-syntax () if +)) if) => 1
;;
;; 2003-10-05:
;; New semantics:
;;
;;   (let ((a 1) (b 1))
;;     (define a 2)
;;     ((syntax-rules () ((_ m) (define (m) (list a b)))) m)
;;     (define b 2)
;;     (m))
;;   => (1 2)
;;
;;   (let ((a 1) (b 1))
;;     (define a 2)
;;     (define-syntax m (syntax-rules () ((_) (list a b))))
;;     (define b 2)
;;     (m))
;;   => (2 2)


;; Idea for syntax-rules extension to provide automatic gensym sequences:

;; (syntax-rules with-id-sequence ()
;;   ((letrec ((var init) ... (with-id-sequence temp)) . body)
;;    (let ((var 'undefined) ...)
;;      (let ((temp init) ...)
;;        (set! var temp) ... (let () . body)))))

;; (syntax-rules with-ids ()
;;   ((letrec-values (((var ... (with-ids tmp)) init) ... (with-ids thunk))
;;      . body)
;;    (let ()
;;      (begin (define var 'undefined) ...)
;;      ...
;;      (define thunk (call-with-values (lambda () init)
;; 		        (lambda (tmp ...) (lambda () #f (set! var tmp) ...))))
;;      ...
;;      (thunk) ... (let () . body))))

;; (syntax-rules with-ids ()
;;   ((set!-values (var ... (with-ids tmp)) expr)
;;    (call-with-values (lambda () expr)
;;      (lambda (tmp ...) (if #f #f) (set! var tmp) ...))))

;; (let ()
;;   (define n 0)
;;   (define (inc) (set! n (+ n 1)) n)
;;   (define-syntax x (inc))
;;   (begin x x x x x))
;; => 5

;; (let-syntax ((real-x #f)
;;              (real-set! set!))
;;   (begin
;;     (define real-x #f)
;;     (define-syntax x
;;       (begin (display "x accessed")
;;              real-x))
;;     (define-syntax set!
;;       (syntax-rules (x)
;;         ((set! x foo)
;;          (begin (display "x set")
;;                 (real-set! real-x foo)))
;;         ((set! . whatever)
;;          (real-set! . whatever))))))

;; At top-level, (define-identifier-syntax id arg ...)
;; is equivalent to chez's (define-syntax id (identifier-syntax id arg ...))
'
(define-syntax define-identifier-syntax
  (syntax-rules (set!)
    ((_ id e)
     (define-syntax id e))
    ((_ id (id* e1) ((set! id** pat) tmpl)) ;; id* and id** are ignored
     (begin
       (define-syntax id e1)
       (define-syntax set!
         (let-syntax ((real-set! set!))
           (syntax-rules (id)
             ((set! id pat) tmpl)
             ((set! . whatever)
              (real-set! . whatever)))))))))


;; Safe-letrec is a letrec that does its own run-time error-checking
;; for premature variable accesses.  It signals errors by calling
;; letrec-set!-error or letrec-access-error.
;; We're careful not to signal an error for cases like:
;; (call/cc (lambda (k) (letrec ((x (set! x (k 1)))) 2))) => 1
'(define-syntax safe-letrec
  (let-syntax ()
    (define-syntax safe-letrec-with-alt-names
      (syntax-rules ()
	((safe-letrec-with-alt-names (var* ...) ((var init) ...) . body)
	 (let ((var #f) ... (ready? #f))
	   (define (set-em! var* ...)
	     (set! var var*) ... (set! ready? #t))
	   (define (var* new-val)
	     (if ready? (set! var new-val) (letrec-set!-error 'var new-val)))
	   ...
	   (begin
	     (let-syntax ((var (if ready? var (letrec-access-error 'var))) ...)
	       (define-syntax new-set!
		 (let-syntax ((set! set!))
		   (syntax-rules no-ellipsis (var ...)
		     ((new-set! var x) (var* x)) ...
		     ((new-set! . other) (set! . other)))))
	       (fluid-let-syntax ((set! new-set!))
		 (set-em! init ...)))
	     (let () . body))))))
    (syntax-rules ()
      ((safe-letrec ((var init) ...) . body)
       ((syntax-rules no-ellipsis ()
	  ((_ . args) (safe-letrec-with-alt-names (var ...) . args)))
	((var init) ...) . body)))))

;; Similarly, an error-checking letrec*.
'(define-syntax safe-letrec*
  (let-syntax ()
    (define-syntax process-bindings
      (syntax-rules ()
	((process-bindings) #f)
	((process-bindings binding ... (var init))
	 (let ((ready? #f))
	   (define (finish val) (set! var val) (set! ready? #t))
	   (define (safe-setter new-val)
	     (if ready? (set! var new-val) (letrec-set!-error 'var new-val)))
	   (let-syntax ((var (if ready? var (letrec-access-error 'var))))
	     (define-syntax new-set!
	       (let-syntax ((set! set!))
		 (syntax-rules no-ellipsis (var)
		   ((new-set! var x) (safe-setter x))
		   ((new-set! . other) (set! . other)))))
	     (fluid-let-syntax ((set! new-set!))
	       (process-bindings binding ...)
	       (finish init)))))))
    (syntax-rules ()
      ((safe-letrec* ((var init) ...) . body)
       (let ((var #f) ...)
	 (process-bindings (var init) ...)
	 (let () . body))))))


;; If we wanted to simplify the primitive let-syntax by saying the
;; first argument must by (), then the full let-syntax could be
;; written this way:
'(define-syntax let-syntax
  (let-syntax ()
    (define-syntax original-let-syntax let-syntax)
    (define-syntax original-define-syntax define-syntax)
    (define-syntax original-syntax-rules syntax-rules)
    (define-syntax let-syntax-with-temps
      (syntax-rules ()
	((let-syntax-with-temps (temp ...) ((kw syn) ...) . body)
	 (original-let-syntax ()
	   (original-define-syntax temp syn) ...
	   (original-let-syntax ()
	     (original-define-syntax kw temp) ...
	     (original-let-syntax () . body))))))
    (syntax-rules ()
      ((let-syntax ((kw syn) ...) . body)
       ((original-syntax-rules no-ellipsis ()
	  ((_ . args) (let-syntax-with-temps (kw ...) . args)))
	((kw syn) ...) . body)))))


;; Example of how an error-checking letrec would be written if we
;; support an extra slot in keyword bindings such that if we bind a
;; keyword with (foo expr/syntax1 expr/syntax2) then the form (set!
;; foo datum ...) gets expanded as (expr/syntax2 datum ...).
'
(define-syntax letrec
  (syntax-rules ()
    ((_ ((var init) ...) . body)
     (let ((var #f) ... (ready? #f))
       (let-syntax
	   ((var (if ready? var (letrec-access-error 'var))
		 (syntax-rules ()
		   ((set!_var expr)
		    (let ((val expr))
		      (if ready?
			  (set! var val)
			  (letrec-set!-error 'var val))))))
	    ...)
	 (let ((var (let ((tmp init)) (lambda () (set! var tmp))))
	       ...)
	   (var) ... (set! ready? #t)))
       (let () . body)))))


;; Nested unquote-splicing:
;;
;; (define x '(a b c))
;; (define a 1) (define b 2) (define c 3)
;;
;; scheme:
;; ``(,,@x)
;; expands=>   (list 'quasiquote (map (lambda (y) (list 'unquote y)) x))
;; evaluates=> `(,a ,b ,c)
;; expands=>   (list a b c)
;; evaluates=> (1 2 3)
;;
;; lisp:
;; ``(,,@x)
;; expands=>   `(list ,@x)
;; expands=>   (cons 'list x)
;; evaluates=> (list a b c)
;; evaluates=> (1 2 3)

;; ;; Ways to convert a program to an expression suitable for eval:
;; (expand-program-to-an-expression
;;   '((define x (- 1))
;;     (write x)
;;     (define f (lambda () (if (read) x (set! y 2))))
;;     (define y (f))
;;     (define - *)
;;     (write (+ (-) (-)))))
;; =>
;; ;; Simple answer requires knowing which top-level vars are in the
;; ;; standard environment:
;; ((lambda (x f y -)
;;      (begin (set! x (- 1))
;;             (write x)
;;             (set! f (lambda () (if (read) x (set! y 2))))
;;             (set! y (f))
;;             (set! - *)
;;             (write (+ (-) (-)))))
;;    #f #f #f -)
;; 
;; ;; More sophisticated version:
;; ((lambda (x f y - _x_setter _f_setter _y_setter _-_setter)
;;    ((lambda (_x_inner-setter _f_inner-setter _y_inner-setter _-_inner-setter)
;;       ((lambda (_x_definer _f_definer _y_definer _-_definer)
;;          (begin (_x_definer ((-) 1))
;;                 (write (x))
;;                 (_f_definer (lambda () (if (read) (x) (_y_setter 2))))
;;                 (_y_definer ((f)))
;;                 (_-_definer *)
;;                 (write (+ ((-)) ((-))))))
;;        (lambda (_) (begin (set! _x_setter _x_inner-setter) (_x_setter _)))
;;        (lambda (_) (begin (set! _f_setter _f_inner-setter) (_f_setter _)))
;;        (lambda (_) (begin (set! _y_setter _y_inner-setter) (_y_setter _)))
;;        (lambda (_) (begin (set! _-_setter _-_inner-setter) (_-_setter _)))))
;;     (lambda (_) (set! x (lambda () _)))
;;     (lambda (_) (set! f (lambda () _)))
;;     (lambda (_) (set! y (lambda () _)))
;;     (lambda (_) (set! - (lambda () _)))))
;;  (lambda () x) (lambda () f) (lambda () y) (lambda () -)
;;  (lambda (_) (set! x (lambda () _)))
;;  (lambda (_) (set! f (lambda () _)))
;;  (lambda (_) (set! y (lambda () _)))
;;  (lambda (_) (set! - (lambda () _))))
