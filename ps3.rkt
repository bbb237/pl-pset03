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
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]

    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
;         [(empty? l)
;          (error 'parse "empty list is invalid syntax")]

         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))

            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(* ) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            
            ;; ======== PAIRS ========
            [(pair)
             ;; (pair e1 e2)
             (pairC (parse (second l)) (parse (third l)))]

            [(fst)
             ;; (fst e)
             (fstC (parse (second l)))]

            [(snd)
             ;; (snd e)
             (sndC (parse (second l)))]

            ;; ======== BOXES ========
            [(box)
             ;; (box e)
             ;; TODO boxC
             (boxC (parse (second l)))]

            [(unbox)
             ;; (unbox e)
             ;; TODO unboxC
             (unboxC (parse (second l)))]

            [(set-box!)
             ;; (set-box! e1 e2)
             ;; TODO setboxC
             (setboxC (parse (second l)) (parse (third l)))]

            ;; ======== VECTORS ========
            [(vector)
             ;; (vector e1 e2 ... en)
             ;; This should build a vector value with all the parsed elements.
             ;; ps3-ast.rkt will have some constructor for literal vectors.
             ;; We didn't yet see its name in the snippet, but we KNOW we need:
             ;;   - vectorC OR something equivalent
             ;; TODO vector literal constructor
             (error 'parse "TODO: vector literal constructor not implemented")]

            [(vector-ref)
             ;; (vector-ref vec-expr idx-expr)
             ;; TODO vector-refC
             (error 'parse "TODO: vector-refC not implemented")]

            [(vector-set!)
             ;; (vector-set! vec-expr idx-expr val-expr)
             (vector-set!C (parse (second l)) (parse (third l)) (parse (fourth l)))]

            [(vector-length)
             ;; (vector-length vec-expr)
             ;; TODO vector-lengthC
             (error 'parse "TODO: vector-lengthC not implemented")]

            [(vector-make)
             ;; (vector-make n-expr init-expr)
             (vector-makeC (parse (second l)) (parse (third l)))]

            [(subvector)
             ;; (subvector vec-expr offset-expr len-expr)
             (subvectorC (parse (second l)) (parse (third l)) (parse (fourth l)))]

            ;; ------------------------------------------------
            [(begin)
             ;; (begin e1 e2 ... en) -> (beginC (list (parse e1) ... (parse en))).
             ;;
             ;; We'll inline the "map parse over rest of l" logic here
             ;; without a top-level helper, by defining a local recursive
             ;; function in this branch only.
             (beginC (map parse (rest l)))]

            ;; ======== TRANSACTIONS ========
            [(transact)
             ;; (transact e)
             (transactC (parse (second l)))]

            ;; ------------------------------------------------
            ;; default: treat it like function application
            [else
             ;; (e1 e2)
             (appC (parse (first l)) (parse (second l)))]
            )]

         ;; If the first element is NOT a symbol, we also treat it like application
         [else
          (appC (parse (first l)) (parse (second l)))]
         ))]
    ))
            
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
