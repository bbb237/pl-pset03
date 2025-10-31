#lang plai-typed
(require "ps3-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps3-ast.rkt and README.md for more information.

;; Note that as in lecture 6, you probably want to implement a version
;; of eval that returns a result that can be an arbitrary value (not just
;; a BaseValue) and also returns a store.  Your eval-base would then be a
;; wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be.
;;
;; For grading, the test cases all result in values that can be converted to base values.

;(define (parse (s : s-expression)) : Expr
;  (error 'parse "Not yet implemented.")
;  )

(define (parse (s : s-expression)) : Expr
  (cond
    ;; ======== ATOMS ========
    [(s-exp-number? s)
     ;; TODO: numbers → (numC (s-exp->number s))
     (numC (s-exp->number s))]

    [(s-exp-boolean? s)
     ;; TODO: booleans → (boolC (s-exp->boolean s))
     (boolC (s-exp->boolean s))]

    [(s-exp-symbol? s)
     ;; TODO: identifiers → (idC (s-exp->symbol s))
     (idC (s-exp->symbol s))]

    ;; ======== COMPOUND FORMS ========
    [(s-exp-list? s)
     (let* ([l  (s-exp->list s)])
;       (unless (and (pair? l) (s-exp-symbol? (first l)))
;         (error 'parse "bad syntax: ~a" s))
       (case (s-exp->symbol (first l))

         ;; ----- arithmetic / equality / conditionals -----
         [(+)      ;; ( + e1 e2 )
          ;; TODO: (plusC (parse (second l)) (parse (third l)))
          (plusC (parse (second l)) (parse (third l)))]
         [(*)      ;; ( * e1 e2 )
          ;; TODO: (timesC (parse (second l)) (parse (third l)))
          (timesC (parse (second l)) (parse (third l)))]
         [(equal?) 
          ;; TODO: (equal?C (parse (second l)) (parse (third l)))
          (equal?C (parse (second l)) (parse (third l)))]
         [(if)
          ;; TODO: (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))
          (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]

         ;; ----- let -----
         [(let)
          ;; surface: (let x e1 e2)
             ;; TODO letC
             (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]

         ;; ------------------------------------------------
         [(lambda)
          ;; surface: (lambda x body)
          ;; TODO lambdaC
          (lambdaC (s-exp->symbol (second l)) (parse (third l)))]

         ;; ----- begin (multi-statement sequencing) -----
         [(begin)
          ;; TODO: (beginC (map parse (rest l)))
          (error 'parse "TODO begin")]

         ;; ======== PAIRS (cons/car/cdr) ========
         [(cons)
          ;; TODO: (consC (parse (second l)) (parse (third l)))
          (error 'parse "TODO cons")]
         [(car)
          ;; TODO: (carC (parse (second l)))
          (error 'parse "TODO car")]
         [(cdr)
          ;; TODO: (cdrC (parse (second l)))
          (error 'parse "TODO cdr")]

         ;; ======== BOXES (allocation / read / write) ========
         ;; choose names that match lecture/tests:
         [(box)        ;; or (newbox)
          ;; TODO: (newboxC (parse (second l)))
          (error 'parse "TODO box/newbox")]
         [(unbox)
          ;; TODO: (openboxC (parse (second l)))
          (error 'parse "TODO unbox")]
         [(set-box!)   ;; or (setbox!)
          ;; TODO: (setboxC (parse (second l)) (parse (third l)))
          (error 'parse "TODO set-box!")]

         ;; ======== VECTORS (heap-allocated) ========
         [(make-vector)
          ;; TODO: (makevecC (parse (second l)) (parse (third l)))
          (error 'parse "TODO make-vector")]
         [(vector-ref)
          ;; TODO: (vecrefC (parse (second l)) (parse (third l)))
          (error 'parse "TODO vector-ref")]
         [(vector-set!)
          ;; TODO: (vecsetC (parse (second l)) (parse (third l)) (parse (fourth l)))
          (error 'parse "TODO vector-set!")]

         ;; (optional) vector-length if your tests include it
         [(vector-length)
          ;; TODO: (veclenC (parse (second l)))
          (error 'parse "TODO vector-length")]

         ;; ======== TRANSACTIONS (Lecture 8) ========
         ;; pick the keyword that matches your notes/tests: (transaction e) or (atomic e)
         [(transaction)
          ;; TODO: (txC (parse (second l)))
          (error 'parse "TODO transaction")]
         [(atomic)
          ;; TODO: (txC (parse (second l)))  ; alias, if your tests use 'atomic'
          (error 'parse "TODO atomic")]

         ;; ======== FUNCTIONAL FORMS (if PS3 includes these) ========
         ;; If lecture 7 lambdas/apps are in scope for PS3, include:
         [(lambda)
          ;; TODO:
          ;;   params := (map s-exp->symbol (s-exp->list (second l)))
          ;;   body   := (parse (third l))
          ;;   (lamC params body)
          (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
         [(@)  ;; if your concrete syntax uses @ for application, else fall through to generic app
          ;; TODO: (appC (parse (second l)) (map parse (s-exp->list (third l))))
          (error 'parse "TODO app-@")]

         ;; ======== FALLBACK: generic application ========
         [else
          ;; If it's not a special form, treat it as application:
          ;; head is the operator; rest are operands
          ;; TODO: (appC (parse (first l)) (map parse (rest l)))
          (error 'parse "TODO application")]))]

    [else (error 'parse "bad syntax")]))


(define (eval-base (e : Expr)) : BaseValue
  (error 'eval-base "Not yet implemented.")
  )



;; ============================================================
;; ps3.rkt
;;
;; You'll implement:
;;   - parse : s-expression -> Expr
;;   - eval-base : Expr -> BaseValue
;;
;; See README.md and ps3-ast.rkt for spec of each construct.
;; ============================================================


;; ------------------------------------------------------------
;; ENV / STORE / VALUES / RESULT
;; (You will flesh these out to match lecture6-style store passing.)
;; ------------------------------------------------------------

;; TODO: define types for runtime Values, Store, Locations, etc.
;; Similar to lecture6 / lecture7d:
;;   - numbers, booleans
;;   - closures (for lambda)
;;   - boxes
;;   - pairs
;;   - vectors / subvectors (must support shared mutation!)
;;   - probably a Result type like (res Value Store)

;; (define-type-alias Location number)
;; (define-type Storage
;;   [cell (location : Location) (val : Value)])
;; (define-type-alias Store (listof Storage))
;; (define empty-store ...)
;; (define override-store ...)

;; (define-type Value
;;   [numV ...]
;;   [boolV ...]
;;   [closV ...]        ; closure (env x body)
;;   [boxV ...]         ; box location
;;   [pairV ...]        ; pair of Values
;;   [vectorV ...]      ; vector representation (backed by store/shared data)
;;   ...)
;;
;; (define-type Binding
;;   [bind (name : symbol) (val : Value)])
;; (define-type-alias Env (listof Binding))
;; (define empty-env ...)
;; (define extend-env ...)
;;
;; lookup : symbol Env -> Value
;; TODO: implement like lecture6 lookup


;; ------------------------------------------------------------
;; PARSER
;; ------------------------------------------------------------

;; Helper: recognize booleans in surface syntax (#true / #false)
(define (boolean-symbol? (s : symbol)) : boolean
  (or (equal? s '#true)
      (equal? s '#false)))

(define (symbol->bool (s : symbol)) : boolean
  (cond
    [(equal? s '#true) #true]
    [(equal? s '#false) #false]
    [else (error 'symbol->bool "not a boolean literal")]))

;; parse-list helper for begin/vector which have arbitrary arity
(define (parse-list-of-sexps (ls : (listof s-expression))) : (listof Expr)
  (cond
    [(empty? ls) empty]
    [else (cons (parse (first ls))
                (parse-list-of-sexps (rest ls)))]))

;; Main parser
(define (parse (s : s-expression)) : Expr
  (cond
    ;; number literal
    [(s-exp-number? s)
     ;; TODO numC
     (numC (s-exp->number s))]

    ;; symbol literal:
    ;;   could be boolean (#true/#false) or identifier
    [(s-exp-symbol? s)
     (let ([sym (s-exp->symbol s)])
       (cond
         ;; TODO boolC
         [(boolean-symbol? sym)
          (boolC (symbol->bool sym))]
         ;; TODO idC
         [else
          (idC sym)]))]

    ;; list form (application or special form)
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         ;; sanity: empty list is invalid in our language
         [(empty? l)
          (error 'parse "empty application is not allowed")]

         ;; first position is a symbol? -> maybe a special form
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))

            ;; ------------------------------------------------
            ;; arithmetic
            [(+)
             ;; surface: (+ e1 e2)
             ;; TODO plusC
             (plusC (parse (second l))
                    (parse (third l)))]

            [(*)
             ;; surface: (* e1 e2)
             ;; TODO timesC
             (timesC (parse (second l))
                     (parse (third l)))]

            ;; ------------------------------------------------
            ;; equal?
            [(equal?)
             ;; surface: (equal? e1 e2)
             ;; TODO equal?C
             (equal?C (parse (second l))
                      (parse (third l)))]

            ;; ------------------------------------------------
            ;; if
            [(if)
             ;; surface: (if guard e1 e2)
             ;; maps to ifC
             ;; TODO ifC
             (ifC (parse (second l))
                  (parse (third l))
                  (parse (fourth l)))]

            ;; ------------------------------------------------
            ;; let
            [(let)
             ;; surface: (let x e1 e2)
             ;; TODO letC
             (letC (s-exp->symbol (second l))
                   (parse (third l))
                   (parse (fourth l)))]

            ;; ------------------------------------------------
            ;; lambda
            [(lambda)
             ;; surface: (lambda x body)
             ;; TODO lambdaC
             (lambdaC (s-exp->symbol (second l))
                      (parse (third l)))]

            ;; ------------------------------------------------
            ;; pairs
            [(pair)
             ;; surface: (pair e1 e2)
             ;; TODO pairC
             (pairC (parse (second l))
                    (parse (third l)))]

            [(fst)
             ;; surface: (fst e)
             ;; TODO fstC
             (fstC (parse (second l)))]

            [(snd)
             ;; surface: (snd e)
             ;; TODO sndC
             (sndC (parse (second l)))]

            ;; ------------------------------------------------
            ;; boxes / mutation
            [(box)
             ;; surface: (box e)
             ;; TODO boxC
             (boxC (parse (second l)))]

            [(unbox)
             ;; surface: (unbox e)
             ;; TODO unboxC
             (unboxC (parse (second l)))]

            [(set-box!)
             ;; surface: (set-box! e1 e2)
             ;; TODO setboxC
             (setboxC (parse (second l))
                      (parse (third l)))]

            ;; ------------------------------------------------
            ;; vectors
            [(vector)
             ;; surface: (vector e1 e2 ... en)
             ;; This should build a vector value with all the parsed elements.
             ;; ps3-ast.rkt will have some constructor for literal vectors.
             ;; We didn't yet see its name in the snippet, but we KNOW we need:
             ;;   - vectorC OR something equivalent
             ;; TODO vector literal constructor
             (error 'parse "TODO: vector literal constructor not implemented")]

            [(vector-ref)
             ;; surface: (vector-ref vec-expr idx-expr)
             ;; TODO vector-refC
             (error 'parse "TODO: vector-refC not implemented")]

            [(vector-set!)
             ;; surface: (vector-set! vec-expr idx-expr val-expr)
             ;; TODO vector-set!C
             (vector-set!C (parse (second l))
                           (parse (third l))
                           (parse (fourth l)))]

            [(vector-length)
             ;; surface: (vector-length vec-expr)
             ;; TODO vector-lengthC
             (error 'parse "TODO: vector-lengthC not implemented")]

            [(vector-make)
             ;; surface: (vector-make n-expr init-expr)
             ;; TODO vector-makeC
             (vector-makeC (parse (second l))
                           (parse (third l)))]

            [(subvector)
             ;; surface: (subvector vec-expr offset-expr len-expr)
             ;; TODO subvectorC
             (subvectorC (parse (second l))
                         (parse (third l))
                         (parse (fourth l)))]

            ;; ------------------------------------------------
            ;; begin
            [(begin)
             ;; surface: (begin e1 e2 ... en)
             ;; In ps3-ast.rkt this is (beginC (listof Expr))【turn1file7†file_000000001df062098a73751c281da8de†L32-L35】
             ;; TODO beginC
             (beginC (parse-list-of-sexps (rest l)))]

            ;; ------------------------------------------------
            ;; transact
            [(transact)
             ;; surface: (transact e)
             ;; TODO transactC
             (transactC (parse (second l)))]

            ;; ------------------------------------------------
            ;; default: treat it like function application
            [else
             ;; surface: (e1 e2)
             ;; application is binary in lecture style: appC fun arg【turn1file11†file_00000000a4d461f6a0390e9e54b71ed5†L54-L58】
             ;; TODO appC
             (appC (parse (first l))
                   (parse (second l)))]
            )]

         ;; If the first element is NOT a symbol, we also treat it like application
         [else
          ;; TODO appC
          (appC (parse (first l))
                (parse (second l)))]))]

    ;; otherwise, not valid surface syntax
    [else
     (error 'parse "unrecognized s-expression")]))

;; ------------------------------------------------------------
;; eval-base
;; ------------------------------------------------------------

;; eval-base : Expr -> BaseValue
;; Runs the interpreter, returns only BaseValue (numBV/boolBV/pairBV),
;; like the test expects【turn1file3†file_000000001df062098a73751c281da8de†L30-L37】【turn1file10†file_000000001df062098a73751c281da8de†L63-L65】.
;;
;; TODO:
;; 1. call your full evaluator (which will thread a store and return some Value)
;; 2. convert that Value to a BaseValue (numbers, booleans, nested pairs),
;;    error on non-base things like closures, boxes, etc.

(define (eval-base (e : Expr)) : BaseValue
  (error 'eval-base "Not yet implemented."))

;; End of file
