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
            [(pair) (pairC (parse (second l)) (parse (third l)))]
            [(fst) (fstC (parse (second l)))]
            [(snd) (sndC (parse (second l)))]

            ;; ======== BOXES ========
            [(box) (boxC (parse (second l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]

            ;; ======== VECTORS ========
            [(vector) (vectorC (map parse (rest l)))]
            [(vector-ref) (vector-refC (parse (second l)) (parse (third l)))]
            [(vector-set!) (vector-set!C (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(vector-length) (vector-lengthC (parse (second l)))]
            [(vector-make) (vector-makeC (parse (second l)) (parse (third l)))]
            [(subvector) (subvectorC (parse (second l)) (parse (third l)) (parse (fourth l)))]

            ;; ------------------------------------------------
            [(begin) (beginC (map parse (rest l)))]
            [(transact) (transactC (parse (second l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]

         [else
          (appC (parse (first l)) (parse (second l)))]
         ))]
    ))

(define-type-alias Location number)

;; used in eval-expr
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [pairV (fst : Value) (snd : Value)]
  [closV (arg : symbol) (body : Expr) (env : Env)]
  [boxV (loc : Location)]
  [vectorV (loc : (listof Location)) (offset : number) (len : number)]
  )

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
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Cell))
(define empty-store empty)

;; new location
(define (alloc-location (sto : Store)) : Location
  (length sto))

;; fetch the Value at a given Location
(define (store-fetch (sto : Store) (loc : Location)) : Value
  (cond
    [(empty? sto)
     (error 'store-fetch "invalid location")]
    [else
     (if (equal? loc (cell-location (first sto)))
         (cell-val (first sto))
         (store-fetch (rest sto) loc))]))

;; update a given Location in the store, returning a new store
(define (store-update (sto : Store) (loc : Location) (v : Value)) : Store
  (cond
    [(empty? sto)
     (error 'store-update "invalid location")]
    [else
     (if (equal? loc (cell-location (first sto)))
         (cons (cell loc v) (rest sto))
         (cons (first sto)
               (store-update (rest sto) loc v)))]))

(define-type AllocResult
  [alloc-res (loc : Location) (store : Store)])

;; Allocate a new cell in the store to hold v, returns loc + updated store.
(define (store-alloc (sto : Store) (v : Value)) : AllocResult
  (let ([newloc (alloc-location sto)])
    (alloc-res newloc (cons (cell newloc v) sto))))

(define-type Res
  [res (v : Value) (s : Store)] )

(define (eval-expr (e : Expr) (env : Env) (sto : Store)) : Res
  (type-case Expr e
    [numC (n) (res (numV n) sto)]
    [boolC (b) (res (boolV b) sto)]
    [plusC (e1 e2)
           (let* ([r1 (eval-expr e1 env sto)]
                  [v1 (res-v r1)]
                  [sto1 (res-s r1)]
                  [r2 (eval-expr e2 env sto1)]
                  [v2 (res-v r2)]
                  [sto2 (res-s r2)]) (res (numV (+ (numV-n v1) (numV-n v2))) sto2))]

    [timesC (e1 e2)
            (let* ([r1 (eval-expr e1 env sto)]
                   [v1 (res-v r1)]
                   [sto1 (res-s r1)]
                   [r2 (eval-expr e2 env sto1)]
                   [v2 (res-v r2)]
                   [sto2 (res-s r2)]) (res (numV (* (numV-n v1) (numV-n v2))) sto2))]

    [equal?C (e1 e2)
             (let* ([r1 (eval-expr e1 env sto)]
                    [v1 (res-v r1)]
                    [sto1 (res-s r1)]
                    [r2 (eval-expr e2 env sto1)]
                    [v2 (res-v r2)]
                    [sto2 (res-s r2)]) (res (boolV (equal? v1 v2)) sto2))]

    [ifC (guard e1 e2)
         (let* ([rtest (eval-expr guard env sto)]
                [vtest (res-v rtest)]
                [stest (res-s rtest)])
           (if (boolV-b vtest)
               (eval-expr e1 env stest)
               (eval-expr e2 env stest)))]

    [idC (x) (res (lookup x env) sto)]

    [letC (x e1 body)
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)]
                 [env2 (extend-env env x v1)])
            (eval-expr body env2 sto1))]

    [lambdaC (x body) (res (closV x body env) sto)]

    [appC (funE argE)
          (let* ([rfun (eval-expr funE env sto)]
                 [vfun (res-v rfun)]
                 [sfun (res-s rfun)]
                 [rarg (eval-expr argE env sfun)]
                 [varg (res-v rarg)]
                 [sto-arg (res-s rarg)])
            (let ([param (closV-arg vfun)]
                  [body  (closV-body vfun)]
                  [fenv  (closV-env vfun)])
              (eval-expr body (extend-env fenv param varg) sto-arg))
            )]
    
    [pairC (e1 e2)
           (let* ([r1 (eval-expr e1 env sto)]
                  [v1 (res-v r1)]
                  [sto1 (res-s r1)]
                  [r2 (eval-expr e2 env sto1)]
                  [v2 (res-v r2)]
                  [sto2 (res-s r2)])
             (res (pairV v1 v2) sto2))]

    [fstC (e1)
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)])
            (res (pairV-fst v1) sto1))]

    [sndC (e1)
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)])
            (res (pairV-snd v1) sto1))]

    ;; ======== BOXES ========
    [boxC (e1)
          (let* ([r1 (eval-expr e1 env sto)]
                 [v1 (res-v r1)]
                 [sto1 (res-s r1)]
                 [a (store-alloc sto1 v1)])
            (res (boxV (alloc-res-loc a))
                 (alloc-res-store a)))]

    [unboxC (e1)
            (let* ([r1 (eval-expr e1 env sto)]
                   [v1 (res-v r1)]
                   [sto1 (res-s r1)])
              (let ([loc (boxV-loc v1)])
                (res (store-fetch sto1 loc) sto1)))]

    [setboxC (e1 e2)
             (let* ([r1 (eval-expr e1 env sto)]
                    [v1 (res-v r1)]
                    [sto1 (res-s r1)]
                    [r2 (eval-expr e2 env sto1)]
                    [v2 (res-v r2)]
                    [sto2 (res-s r2)])
               (let* ([loc (boxV-loc v1)]
                      [sto3  (store-update sto2 loc v2)])
                 (res v2 sto3)))]

    ;; ======== VECTORS ========
    [vectorC (elems)
             (cond
               [(empty? elems) (res (vectorV empty 0 0) sto)]
               [else
                (let* ([r1 (eval-expr (first elems) env sto)]
                       [v1 (res-v r1)]
                       [sto1 (res-s r1)]
                       [ar1 (store-alloc sto1 v1)]
                       [loc1 (alloc-res-loc ar1)]
                       [sto2  (alloc-res-store ar1)]
                       [rtail (eval-expr (vectorC (rest elems)) env sto2)]
                       [vtail (res-v rtail)]
                       [stail (res-s rtail)])
                  (type-case Value vtail
                    [vectorV (loc-tail off-tail len-tail)
                             (let* ([new-loc (cons loc1 loc-tail)]
                                    [new-len (+ 1 len-tail)])
                               (res (vectorV new-loc 0 new-len) stail))]
                    [else (error 'eval-expr "internal vectorC invariant broken")])
                  )]
               )]

    [vector-refC (vecE idxE)
                 (let* ([rvec (eval-expr vecE env sto)]
                        [vvec (res-v rvec)]
                        [svec (res-s rvec)])
                   (type-case Value vvec
                     [vectorV (loc offset vlen)
                              (let* ([ridx (eval-expr idxE env svec)]
                                     [vidx (res-v ridx)]
                                     [sidx (res-s ridx)])
                                (type-case Value vidx
                                  [numV (n)
                                        (let* ([target-index (+ offset n)]
                                               [target-loc (list-ref loc target-index)]
                                               [elem-val (store-fetch sidx target-loc)])
                                          (res elem-val sidx))]
                                  [else (error 'eval-expr "vector-ref index not a number")])
                                )]
                     [else (error 'eval-expr "vector-ref on non-vector")])
                   )]

    [vector-set!C (vecE idxE val-expr)
                  (let* ([rvec (eval-expr vecE env sto)]
                         [vvec (res-v rvec)]
                         [svec (res-s rvec)])
                    (type-case Value vvec
                      [vectorV (loc offset vlen)
                               (let* ([ridx (eval-expr idxE env svec)]
                                      [vidx (res-v ridx)]
                                      [sidx (res-s ridx)])
                                 (type-case Value vidx
                                   [numV (n)
                                         (let* ([rval (eval-expr val-expr env sidx)]
                                                [vval (res-v rval)]
                                                [sval (res-s rval)]
                                                [target-index (+ offset n)]
                                                [target-loc (list-ref loc target-index)]
                                                [s-upd (store-update sval target-loc vval)])
                                           (res (vectorV loc offset vlen) s-upd))]
                                   [else (error 'eval-expr "vector-set! index not a number")])
                                 )]
                      [else (error 'eval-expr "vector-set! on non-vector")])
                    )]

    [vector-lengthC (vecE)
                    (let* ([rvec (eval-expr vecE env sto)]
                           [vvec (res-v rvec)]
                           [svec (res-s rvec)])
                      (type-case Value vvec
                        [vectorV (loc offset vlen)
                                 (res (numV vlen) svec)]
                        [else
                         (error 'eval-expr "vector-length on non-vector")]))]
    
    [vector-makeC (len-expr init-expr)
                  (let* ([rlen  (eval-expr len-expr env sto)]
                         [vlen  (res-v rlen)]
                         [slen  (res-s rlen)])
                    (type-case Value vlen
                      [numV (n)
                            (let* ([rinit (eval-expr init-expr env slen)]
                                   [vinit (res-v rinit)]
                                   [sinit (res-s rinit)])
                              (letrec ([build
                                        (lambda (k cur-store acc-rev)
                                          (if (zero? k)
                                              (let ([loc-list (reverse acc-rev)])
                                                (res (vectorV loc-list 0 n) cur-store))
                                              (let* ([ar        (store-alloc cur-store vinit)]
                                                     [new-loc   (alloc-res-loc ar)]
                                                     [new-store (alloc-res-store ar)])
                                                (build (- k 1)
                                                       new-store
                                                       (cons new-loc acc-rev))))
                                          )])
                                (build n sinit empty))
                              )]
                      [else (error 'eval-expr "vector-make length not a number")])
                    )]

    [subvectorC (vecE off-expr len-expr)
                (let* ([rvec (eval-expr vecE env sto)]
                       [vvec (res-v rvec)]
                       [svec (res-s rvec)])
                  (type-case Value vvec
                    [vectorV (loc base-off base-len)
                             (let* ([roff (eval-expr off-expr env svec)]
                                    [voff (res-v roff)]
                                    [soff (res-s roff)])
                               (type-case Value voff
                                 [numV (off-n)
                                       (let* ([rlen (eval-expr len-expr env soff)]
                                              [vlen (res-v rlen)]
                                              [slen (res-s rlen)])
                                         (type-case Value vlen
                                           [numV (len-n) (res (vectorV loc (+ base-off off-n) len-n) slen)]
                                           [else  (error 'eval-expr "subvector len not a number")])
                                         )]
                                 [else (error 'eval-expr "subvector offset not a number")])
                               )]
                    [else (error 'eval-expr "subvector on non-vector")])
                  )]

    [beginC (es)
            (cond
              [(empty? es)
               (error 'beginC "begin with no subexpressions")]

              [(empty? (rest es))
               (eval-expr (first es) env sto)]

              [else
               (let* ([r1 (eval-expr (first es) env sto)]
                      [sto1 (res-s r1)])
                 (eval-expr (beginC (rest es)) env sto1))]
              )]

    [transactC (body)
               (let ([start-store sto])
                 (let* ([rbody (eval-expr body env sto)]
                        [vbody (res-v rbody)]
                        [sbody (res-s rbody)])
                   (type-case Value vbody
                     [pairV (firstv secondv)
                            (type-case Value firstv
                              [boolV (b)
                                     (if b
                                         (res secondv sbody)      
                                         (res secondv start-store)
                                         )]
                              [else
                               (error 'eval-expr "transact did not return (pair boolean value)")])]
                     [else
                      (error 'eval-expr "transact did not return a pair")]))
                 )]

    ))

(define (eval-base (e : Expr)) : BaseValue
  (let* ([r (eval-expr e empty-env empty-store)]
         [v (res-v r)])
    (letrec ([value->base
              (lambda (v)
                (type-case Value v
                  [numV (n) (numBV n)]
                  [boolV (b) (boolBV b)]
                  [pairV (l r) (pairBV (value->base l) (value->base r))]
                  [closV (arg body env) (error 'eval-base "function values are not baseValues")]
                  [boxV (loc) (error 'eval-base "box values are not baseValues")]
                  [vectorV (loc offset len) (error 'eval-base "vector values are not baseValues")]
                  ))
              ])
      (value->base v)))
  )
