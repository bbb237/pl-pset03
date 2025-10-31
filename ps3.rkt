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
             ;; (vector-ref vecE idxE)
             (vector-refC (parse (second l)) (parse (third l)))]

            [(vector-set!)
             ;; (vector-set! vecE idxE val-expr)
             (vector-set!C (parse (second l)) (parse (third l)) (parse (fourth l)))]

            [(vector-length)
             ;; (vector-length vecE)
             (vector-lengthC (parse (second l)))]

            [(vector-make)
             ;; (vector-make n-expr init-expr)
             (vector-makeC (parse (second l)) (parse (third l)))]

            [(subvector)
             ;; (subvector vecE offset-expr len-expr)
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
  [pairV (fst : Value) (snd : Value)]
  [closV (arg : symbol) (body : Expr) (env : Env)]
  [boxV (loc : Location)]
  [vectorV (loc : (listof Location)) (offset : number) (len : number)]
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
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Cell))
(define empty-store empty)

;; fresh location (classic lecture6 trick: next index is (length sto))
(define (alloc-location (sto : Store)) : Location
  (length sto))

;; look up the Value at a given Location
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

;; allocate a new location to hold v, return loc + new store
(define-type AllocResult
  [alloc-res (loc : Location) (store : Store)])

;; Allocate a new cell in the store to hold v,
;; returning the fresh location and the updated store.
(define (store-alloc (sto : Store) (v : Value)) : AllocResult
  (let ([newloc (alloc-location sto)])
    (alloc-res newloc (cons (cell newloc v) sto))))

;; The result of evaluating an Expr is a Value plus updated Store.
(define-type Res
  [res (v : Value) (s : Store)] )

;; eval-expr : Expr Env Store -> Res
;; returns a runtime Value and the new Store
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
                 [sto-arg (res-s rarg)])
            ;; TODO: ensure vfun is closV, otherwise error
            (let ([param (closV-arg vfun)]
                  [body  (closV-body vfun)]
                  [fenv  (closV-env vfun)])
              (eval-expr body (extend-env fenv param varg) sto-arg))
            )]

    ;; --------------------------------------------------------
    ;; pairs
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

    ;; --------------------------------------------------------
    ;; boxes
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
              (let ([loc (boxV-loc v1)])
                (res (store-fetch sto1 loc) sto1)))]

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
               (let* ([loc (boxV-loc v1)]
                      [sto3  (store-update sto2 loc v2)])
                 (res v2 sto3)))]

    ;; --------------------------------------------------------
    ;; vectors
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
                 ;; (vector-ref v i)
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
                                        ;; find location of element (offset + n) in loc
                                        (let* ([target-index (+ offset n)]
                                               [target-loc (list-ref loc target-index)]
                                               [elem-val (store-fetch sidx target-loc)])
                                          (res elem-val sidx))]
                                  [else (error 'eval-expr "vector-ref index not a number")])
                                )]
                     [else (error 'eval-expr "vector-ref on non-vector")])
                   )]

    [vector-set!C (vecE idxE val-expr)
                  ;; (vector-set! vec idx val)
                  ;; TODO:
                  ;; 1. eval vecE -> vectorV
                  ;; 2. eval idxE -> numV
                  ;; 3. eval val-expr
                  ;; 4. mutate list-of-Values and build a new list with the index replaced.
                  ;; 5. return (probably the value)
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

;    [vector-makeC (len-expr init-expr)
;                  (let* ([rlen  (eval-expr len-expr env sto)]
;                         [vlen  (res-v rlen)]
;                         [slen  (res-s rlen)])
;                    (type-case Value vlen
;                      [numV (n)
;                            (let* ([rinit (eval-expr init-expr env slen)]
;                                   [vinit (res-v rinit)]
;                                   [sinit (res-s rinit)])
;                              (letrec ([build
;                                        (lambda (k cur-store acc-locs)
;                                          (if (zero? k)
;                                              (values acc-locs cur-store)
;                                              (let* ([ar (store-alloc cur-store vinit)]
;                                                     [new-loc    (alloc-res-loc ar)]
;                                                     [new-store  (alloc-res-store ar)])
;                                                (build (- k 1) new-store (cons new-loc acc-locs)))
;                                              ))
;                                        ])
;                                (let-values ([(locs-rev final-store) (build n sinit empty)])
;                                  (let ([locs-in-order (reverse locs-rev)])
;                                    (res (vectorV locs-in-order 0 n) final-store))))
;                              )]
;                      [else (error 'eval-expr "vector-make length not a number")])
;                    )]
    
    [vector-makeC (len-expr init-expr)
                  ;; (vector-make n v)
                  ;; 1. evaluate n -> must be a number
                  ;; 2. evaluate v -> initial element value
                  ;; 3. allocate n cells in the store, each holding that value
                  ;; 4. build (vectorV loc 0 n), where loc is the list of those Locations
                  ;; 5. return (res that-vector final-store)
                  (let* ([rlen  (eval-expr len-expr env sto)]
                         [vlen  (res-v rlen)]
                         [slen  (res-s rlen)])
                    (type-case Value vlen
                      [numV (n)
                            ;; evaluate the initializer in the updated store from length
                            (let* ([rinit (eval-expr init-expr env slen)]
                                   [vinit (res-v rinit)]
                                   [sinit (res-s rinit)])
                              ;; build : number Store (listof Location) -> Res
                              ;; allocates k copies of vinit, threading the store.
                              ;; acc-rev is the list of element locations we've allocated so far,
                              ;; in reverse order (we'll reverse once at the end).
                              (letrec ([build
                                        (lambda (k cur-store acc-rev)
                                          (if (zero? k)
                                              ;; done:
                                              ;; reverse acc-rev to get locations in index order
                                              ;; index 0 should correspond to the first allocated location
                                              (let ([loc-list (reverse acc-rev)])
                                                ;; vectorV wants:
                                                ;;   loc    : (listof Location) = loc-list
                                                ;;   offset : number            = 0
                                                ;;   len    : number            = n
                                                (res (vectorV loc-list 0 n) cur-store))
                                              ;; otherwise allocate one more cell for vinit
                                              (let* ([ar        (store-alloc cur-store vinit)]
                                                     [new-loc   (alloc-res-loc ar)]
                                                     [new-store (alloc-res-store ar)])
                                                (build (- k 1)
                                                       new-store
                                                       (cons new-loc acc-rev))))
                                          )])
                                ;; kick off the allocator with k = n, starting store = sinit,
                                ;; and an empty accumulator of locations
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

    ;; --------------------------------------------------------
    ;; begin / sequencing
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
               (let ([start-store sto])
                 (let* ([rbody (eval-expr body env sto)]
                        [vbody (res-v rbody)]
                        [sbody (res-s rbody)])
                   (type-case Value vbody
                     [pairV (firstv secondv)
                            (type-case Value firstv
                              [boolV (b)
                                     (if b
                                         (res secondv sbody)       ;; commit
                                         (res secondv start-store) ;; rollback
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
                  [vectorV (loc offset len)
                           (error 'eval-base "vector values are not base")]
                  ))
              ])
      (value->base v)))
  )
