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
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]

    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
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
             (boxC (parse (second l)))]

            [(unbox)
             (unboxC (parse (second l)))]

            [(set-box!)
             (setboxC (parse (second l)) (parse (third l)))]

            ;; ======== VECTORS ========
            [(vector)
             ;; (vector e1 e2 ... en)
             (vectorC (map parse (rest l)))]

            [(vector-ref)
             ;; (vector-ref vec-expr idx-expr)
             (vector-refC (parse (second l)) (parse (third l)))]

            [(vector-set!)
             ;; (vector-set! vec-expr idx-expr val-expr)
             (vector-set!C (parse (second l)) (parse (third l)) (parse (fourth l)))]

            [(vector-length)
             ;; (vector-length vec-expr)
             (vector-lengthC (parse (second l)))]

            [(vector-make)
             ;; (vector-make n-expr init-expr)
             (vector-makeC (parse (second l)) (parse (third l)))]

            [(subvector)
             ;; (subvector vec-expr offset-expr len-expr)
             (subvectorC (parse (second l)) (parse (third l)) (parse (fourth l)))]

            ;; ------------------------------------------------
            [(begin)
             ;; (begin e1 e2 ... en) -> (beginC (list (parse e1) ... (parse en))).
             (beginC (map parse (rest l)))]

            ;; ======== TRANSACTIONS ========
            [(transact)
             ;; (transact e)
             (transactC (parse (second l)))]

            ;; ------------------------------------------------
            [else
             ;; (e1 e2)
             (appC (parse (first l)) (parse (second l)))]
            )]

         [else
          (appC (parse (first l)) (parse (second l)))]
         ))]
    ))


;; ============================================================
;; Runtime values, environment, store, results
;; ============================================================

;; A Location will represent an index into the store.
(define-type-alias Location number)

;; Value = runtime values.
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [pairV (left : Value) (right : Value)]
  [closV (arg : symbol) (body : Expr) (env : Env)]
  [boxV (loc : Location)]
  [vectorV (vec-data : (listof Value))]
  )

;; Environment mapping identifiers to Values.
(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))

(define empty-env empty)

(define (extend-env (env : Env) (x : symbol) (v : Value)) : Env
  (cons (bind x v) env))

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(empty? env)
     (error 'lookup "unbound identifier")]
    [else
     (if (equal? x (bind-name (first env)))
         (bind-val (first env))
         (lookup x (rest env)))]))

;; Store: list of Cells
(define-type Cell
  [cell (where : Location) (val : Value)])

(define-type-alias Store (listof Cell))

(define empty-store empty)

;; fresh location (classic lecture6 trick: next index is (length sto))
(define (alloc-location (sto : Store)) : Location
  (length sto))

;; look up the Value at a given Location
(define (store-ref (sto : Store) (loc : Location)) : Value
  (cond
    [(empty? sto)
     (error 'store-ref "invalid location")]
    [else
     (if (= loc (cell-where (first sto)))
         (cell-val (first sto))
         (store-ref (rest sto) loc))]))

;; update a given Location in the store, returning a new store
(define (store-set (sto : Store) (loc : Location) (v : Value)) : Store
  (cond
    [(empty? sto)
     (error 'store-set "invalid location")]
    [else
     (if (= loc (cell-where (first sto)))
         (cons (cell loc v) (rest sto))
         (cons (first sto)
               (store-set (rest sto) loc v)))]))

;; allocate a new location to hold v, return loc + new store
(define-type AllocResult
  [alloc-res (loc : Location) (store : Store)])

(define (store-alloc (sto : Store) (v : Value)) : AllocResult
  (let ([newloc (alloc-location sto)])
    (alloc-res newloc
               (cons (cell newloc v) sto))))

;; The result of evaluating an Expr is a Value plus updated Store.
(define-type Res
  [res (v : Value) (s : Store)])

;; eval-expr : Expr Env Store -> Res
;; returns a runtime Value and the new Store
(define (eval-expr (e : Expr) (env : Env) (sto : Store)) : Res
  (type-case Expr e

    ;; --------------------------------------------------------
    ;; literals
    [numC (n) (res (numV n) sto)]
    [boolC (b) (res (boolV b) sto)]

    ;; --------------------------------------------------------
    ;; arithmetic
    [plusC (e1 e2)
           ;; (+ e1 e2)
           ;; TODO:
           ;; 1. eval e1
           ;; 2. eval e2 with new store
           ;; 3. ensure both are numV
           ;; 4. return numV of their sum
           (let* ([r1 (eval-expr e1 env sto)]
                  [v1 (res-v r1)]
                  [sto1 (res-s r1)]
                  [r2 (eval-expr e2 env sto1)]
                  [v2 (res-v r2)]
                  [sto2 (res-s r2)]) (res (numV (+ (numV-n v1) (numV-n v2))) sto2))]

    [timesC (e1 e2)
            ;; (* e1 e2)
            ;; TODO similar to plusC but multiply
            (let* ([r1 (eval-expr e1 env sto)]
                   [v1 (res-v r1)]
                   [sto1 (res-s r1)]
                   [r2 (eval-expr e2 env sto1)]
                   [v2 (res-v r2)]
                   [sto2 (res-s r2)]) (res (numV (* (numV-n v1) (numV-n v2))) sto2))]

    ;; --------------------------------------------------------
    ;; equal? and if
    [equal?C (e1 e2)
             ;; (equal? e1 e2)
             ;; TODO:
             ;; 1. eval e1/e2
             ;; 2. compare them appropriately
             ;; 3. return boolV
             (let* ([r1 (eval-expr e1 env sto)]
                    [v1 (res-v r1)]
                    [sto1 (res-s r1)]
                    [r2 (eval-expr e2 env sto1)]
                    [v2 (res-v r2)]
                    [sto2 (res-s r2)]) (res (boolV (equal? v1 v2)) sto2))]

    [ifC (testE thenE elseE)
         ;; (if testE thenE elseE)
         ;; TODO:
         ;; 1. eval testE
         ;; 2. ensure boolV
         ;; 3. depending on bool, eval thenE or elseE
         (let* ([rtest (eval-expr testE env sto)]
                [vtest (res-v rtest)]
                [stest (res-s rtest)])
           ;; TODO type-check boolV
           (if (boolV-b vtest)
               (eval-expr thenE env stest)
               (eval-expr elseE env stest)))]

    ;; --------------------------------------------------------
    ;; variables and let
    [idC (x)
         ;; variable reference
         ;; TODO: lookup x
         (res (lookup x env) sto)]

    [letC (x e1 body)
          ;; (let x e1 body)
          ;; TODO:
          ;; 1. eval e1
          ;; 2. extend env
          ;; 3. eval body under that env
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)]
                 [env2 (extend-env env x v1)])
            (eval-expr body env2 sto1))]

    ;; --------------------------------------------------------
    ;; lambda / application
    [lambdaC (x body) (res (closV x body env) sto)]

    [appC (funE argE)
      ;; (funE argE)
      ;; TODO:
      ;; 1. eval funE -> should be closV
      ;; 2. eval argE
      ;; 3. extend closure env with (param = argVal)
      ;; 4. eval closure body in that extended env, threading store
      (let* ([rfun (eval-expr funE env sto)]
             [vfun (res-v rfun)]
             [sfun (res-s rfun)]
             [rarg (eval-expr argE env sfun)]
             [varg (res-v rarg)]
             [sarg (res-s rarg)])
        ;; TODO: ensure vfun is closV, otherwise error
        (let ([param (closV-arg vfun)]
              [body  (closV-body vfun)]
              [fenv  (closV-env vfun)])
          (eval-expr body
                     (extend-env fenv param varg)
                     sarg)))]

    ;; --------------------------------------------------------
    ;; pairs
    [pairC (e1 e2)
           ;; (pair e1 e2)
           ;; TODO:
           ;; eval both, return pairV
           (let* ([r1 (eval-expr e1 env sto)]
                  [v1 (res-v r1)]
                  [sto1 (res-s r1)]
                  [r2 (eval-expr e2 env sto1)]
                  [v2 (res-v r2)]
                  [sto2 (res-s r2)])
             (res (pairV v1 v2) sto2))]

    [fstC (e1)
          ;; (fst e1)
          ;; TODO:
          ;; eval e1, ensure pairV, return left
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)])
            ;; TODO ensure pairV
            (res (pairV-left v1) sto1))]

    [sndC (e1)
          ;; (snd e1)
          ;; TODO:
          ;; eval e1, ensure pairV, return right
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)])
            ;; TODO ensure pairV
            (res (pairV-right v1) sto1))]

    ;; --------------------------------------------------------
    ;; boxes and mutation
    [boxC (e1)
          ;; (box e1)
          ;; TODO:
          ;; 1. eval e1
          ;; 2. allocate new location in store for that value
          ;; 3. return boxV(loc)
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)]
                 [a (store-alloc sto1 v1)])
            (res (boxV (alloc-res-loc a))
                 (alloc-res-store a)))]

    [unboxC (e1)
            ;; (unbox e1)
            ;; TODO:
            ;; 1. eval e1 -> boxV loc
            ;; 2. read from store at loc
            (let* ([r1 (eval-expr e1 env sto)]
                   [v1 (res-v r1)]
                   [sto1 (res-s r1)])
              ;; TODO ensure v1 is boxV
              (let ([loc (boxV-loc v1)])
                (res (store-ref sto1 loc) sto1)))]

    [setboxC (e1 e2)
             ;; (set-box! e1 e2)
             ;; TODO:
             ;; 1. eval e1 -> boxV loc
             ;; 2. eval e2 -> newval
             ;; 3. update store[loc] = newval
             ;; 4. result is newval (or whatever spec says)
             (let* ([r1 (eval-expr e1 env sto)]
                    [v1 (res-v r1)]
                    [sto1 (res-s r1)]
                    [r2 (eval-expr e2 env sto1)]
                    [v2 (res-v r2)]
                    [sto2 (res-s r2)])
               ;; TODO ensure v1 is boxV
               (let* ([loc (boxV-loc v1)]
                      [s3  (store-set sto2 loc v2)])
                 (res v2 s3)))]

    ;; --------------------------------------------------------
    ;; vectors
    ;; These are still TODO-heavy: you'll define how vectorV works.

    [vectorC (elems)
             ;; (vector e1 e2 ... en)
             ;; elems : (listof Expr) from parser
             ;; TODO:
             ;; 1. eval each elem left-to-right, threading store
             ;; 2. allocate a mutable backing structure
             ;; 3. wrap in vectorV
             (error 'eval-expr "TODO: vectorC evaluation")]

    [vector-refC (vec-expr idx-expr)
                 ;; (vector-ref vec idx)
                 ;; TODO:
                 ;; 1. eval vec-expr -> vectorV
                 ;; 2. eval idx-expr -> numV
                 ;; 3. read that element
                 (error 'eval-expr "TODO: vector-refC evaluation")]

    [vector-set!C (vec-expr idx-expr val-expr)
                  ;; (vector-set! vec idx val)
                  ;; TODO:
                  ;; 1. eval vec-expr -> vectorV
                  ;; 2. eval idx-expr -> numV
                  ;; 3. eval val-expr
                  ;; 4. mutate underlying storage
                  ;; 5. return (probably the value)
                  (error 'eval-expr "TODO: vector-set!C evaluation")]

    [vector-lengthC (vec-expr)
                    ;; (vector-length vec)
                    ;; TODO:
                    ;; compute logical length of the vector/subvector
                    (error 'eval-expr "TODO: vector-lengthC evaluation")]

    [vector-makeC (len-expr init-expr)
                  ;; (vector-make n init)
                  ;; TODO:
                  ;; 1. eval n -> numV
                  ;; 2. eval init
                  ;; 3. allocate new mutable vector of length n
                  (error 'eval-expr "TODO: vector-makeC evaluation")]

    [subvectorC (vec-expr off-expr len-expr)
                ;; (subvector vec off len)
                ;; TODO:
                ;; 1. eval vec-expr -> vectorV
                ;; 2. eval off, len as numbers
                ;; 3. return vectorV view sharing same underlying data
                (error 'eval-expr "TODO: subvectorC evaluation")]

    ;; --------------------------------------------------------
    ;; begin / sequencing
    [beginC (es)
        ;; (begin e1 e2 ... en)
        ;; Semantics:
        ;; - evaluate e1, then e2, ..., returning the last value/store
        ;; We do this structurally, by reducing (begin e1 e2 ... en)
        ;; to either just e1 (if it's the only one)
        ;; or: eval e1 for effects, then eval (begin e2 ... en).
        (cond
          [(empty? es)
           (error 'beginC "begin with no subexpressions")]

          [(empty? (rest es))
           ;; only one expression in the begin: just eval it
           (eval-expr (first es) env sto)]

          [else
           ;; there's at least e1 and e2.
           ;; Step 1: eval e1 for its effects on the store
           (let* ([r1 (eval-expr (first es) env sto)]
                  [sto1 (res-s r1)])
             ;; Step 2: evaluate (begin e2 ... en) starting from that new store
             ;; We literally construct a smaller beginC node and reuse eval-expr,
             ;; so we don't write our own recursion (so no letrec needed).
             (eval-expr (beginC (rest es)) env sto1))])]

    ;; --------------------------------------------------------
    ;; transact
    [transactC (body)
               ;; (transact body)
               ;; TODO (spec from ps3):
               ;; 1. snapshot original store
               ;; 2. eval body using a copy
               ;; 3. expect result to be (pair (bool commit?) result-val)
               ;; 4. if commit? is #true => keep new store
               ;;    if #false => discard new store, keep old
               ;; 5. return result-val with whichever store survived
               (let* ([original-store sto]
                      ;; TODO you may want a 'clone-store' helper if needed
                      [rbody (eval-expr body env original-store)]
                      [vbody (res-v rbody)]
                      [sbody (res-s rbody)])
                 ;; TODO: destructure vbody as pairV(boolV commit?) resultV
                 (error 'eval-expr "TODO: transactC evaluation"))]

    ))

(define (eval-base (e : Expr)) : BaseValue
  (let* ([r (eval-expr e empty-env empty-store)]
         [v (res-v r)])
    (letrec ([value->base
              (lambda (v)
                (type-case Value v
                  [numV (n)
                        ;; TODO: number -> numBV
                        (numBV n)]
                  [boolV (b)
                         ;; TODO: boolean -> boolBV
                         (boolBV b)]
                  [pairV (l r)
                         ;; TODO: recursively convert pair sides
                         (pairBV (value->base l)
                                 (value->base r))]
                  [closV (arg body env)
                         (error 'eval-base "function values are not base")]
                  [boxV (loc)
                        (error 'eval-base "box values are not base")]
                  [vectorV (vec-data)
                           (error 'eval-base "vector values are not base")]
                  ))
              ])
      (value->base v)))
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
