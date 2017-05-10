(library
  (silly-k)
  (export compile-silly-k
          compile-silly-k-and-run
          compile-to-malfunction
          compile-to-scheme
          passes
          parse-silly-k                  id
          ast-to-Lsrc                    unparse-Lsrc
          differentiate-scalars          unparse-L1
          translate-to-primfuns          unparse-L2
          introduce-lambda-abstractions  unparse-L3
          type-scalars-and-vectors       unparse-L4
          introduce-fresh-typevars       unparse-L5
          type-lambda-abstractions       unparse-L6
          derive-type-constraints        unparse-L7
          unify-and-substitute-types     unparse-L8
          expand-idioms
          type-check
          untype                         unparse-L9
          output-malfunction
          output-scheme)
  (import (nanopass)
          (chezscheme)
          (silly-k tools)
          (lalr))

  (define lex
    (lambda ()
      (letrec ([skip-spaces
                 (lambda ()
                   (let ([c (peek-char)])
                     (cond
                       [(eof-object? c)]
                       [(or (char=? c #\space)
                            (char=? c #\tab))
                        (read-char) (skip-spaces)]
                       )))]
               [read-number
                 (lambda (l)
                   (let ([c (peek-char)])
                     (cond
                       [(and (char? c) (char-numeric? c))
                        (read-number (cons (read-char) l))]
                       [else (string->number (list->string (reverse l)))])))]
               [read-atom
                 (lambda (l)
                   (let ([c (peek-char)])
                     (cond
                       [(and (char? c) (char-alphabetic? c))
                        (read-atom (cons (read-char) l))]
                       [else (string->symbol (list->string (reverse l)))])))])
        (skip-spaces)
        (let ([location (make-source-location
                          (port-name (current-input-port))
                          (if (port-has-port-position? (current-input-port))
                            (port-position (current-input-port))
                            #f)
                          #f
                          -1
                          (if (port-has-port-length? (current-input-port))
                            (port-length (current-input-port))
                            -1))]
              [c (read-char)])
          (cond
            [(eof-object? c) '*eoi*]
            [(char=? c #\+)       (make-lexical-token 'PLUS location #f)]
            [(char=? c #\newline) (make-lexical-token 'NEWLINE location #f)]
            [(char=? c #\:)       (make-lexical-token 'COLON location #f)]
            [(char=? c #\-)       (make-lexical-token 'MINUS location #f)]
            [(char=? c #\()       (make-lexical-token 'LPAREN location #f)]
            [(char=? c #\))       (make-lexical-token 'RPAREN location #f)]
            [(char=? c #\{)       (make-lexical-token 'LBRACE location #f)]
            [(char=? c #\})       (make-lexical-token 'RBRACE location #f)]
            [(char=? c #\])       (make-lexical-token 'RBRACKET location #f)]
            [(char=? c #\/)       (make-lexical-token 'SLASH location #f)]
            [(char=? c #\')       (make-lexical-token 'QUOTE location #f)]
            [(char-numeric? c)    (make-lexical-token 'NUM location (read-number `(,c)))]
            [(char-alphabetic? c) (make-lexical-token 'ATOM location (read-atom `(,c)))]
            [else (error 'lex "Unrecognized character" c)])))))

  (define parse-silly-k
    (lambda ()
      (let ([parser
              (lalr-parser
                (expect: 0)
                (PLUS (left: NUM) MINUS LPAREN RPAREN SLASH QUOTE COLON NEWLINE LBRACE RBRACE ATOM RBRACKET)
                (statement (expr) : $1
                           (expr NEWLINE) : $1)
                (expr (expr verb expr) : `(apply ,$2 ,$1 ,$3)
                      (expr expr) : `(apply ,$1 #f ,$2)
                      (LPAREN expr RPAREN) : $2
                      (verb) : $1
                      (num) : $1
                      (ATOM) : $1)
                (verb (PLUS) : 'plus
                      (MINUS) : 'minus
                      (NUM COLON) : `(system ,$1)
                      (RBRACKET) : '(system 3)
                      (LBRACE expr RBRACE) : `(dfn ,$2)
                      (verb adverb) : `(adverb ,$2 ,$1))
                (adverb (SLASH) : 'over
                        (QUOTE) : 'each)
                (num (num NUM) : (append $1 (list $2))
                     (NUM) : (list $1)))]
            [error-handler (lambda (message . args) (error 'parse-silly-k message args))])
        (parser lex error-handler))))

  (define num-vector?
    (lambda (l)
      (and (list? l)
           (fold-right (lambda (x y) (and x y)) #t (map number? l)))))

  (define verb?
    (lambda (x)
      (not (not (memq x '(plus minus colon))))))

  (define adverb?
    (lambda (x)
      (not (not (memq x '(each over))))))

  (define-language
    Lsrc
    (terminals
      (verb (v))
      (symbol (s))
      (adverb (a))
      (number (n))
      (num-vector (nv)))
    (Expr (e)
          nv
          s
          v
          (system n)
          (adverb a e)
          (dfn e)
          (apply e)
          (apply e0 e1)
          (apply e0 e1 e2)))

  (define-pass ast-to-Lsrc : * (e) -> Lsrc ()
    (Expr : * (e) -> Expr ()
      (cond
        [(num-vector? e) e]
        [(verb? e) e]
        [(and (list? e) (eq? (car e) 'system))
         `(system ,(cadr e))]
        [(and (list? e) (eq? (car e) 'adverb))
         (let ([v (cadr e)]
               [e^ (Expr (caddr e))])
           `(adverb ,v ,e^))]
        [(and (list? e) (eq? (car e) 'dfn))
         `(dfn ,[Expr (cadr e)])]
        [(and (list? e) (eq? (car e) 'dfn))
         `(dfn ,[Expr (cadr e)])]
        [(and (list? e) (eq? (car e) 'apply))
         (let ([v (Expr (cadr e))]
               [a (caddr e)]
               [b (cadddr e)])
           (cond
             [(and a b) `(apply ,v ,(Expr a) ,(Expr b))]
             [(and (not a) b) `(apply ,v ,(Expr b))]
             [(and (not a) (not b)) `(apply ,v)]
             [(and a (not b)) (error 'ast-to-Lsrc "unsupported fuction application!")]))]
        [else e]
        ))
    (Expr e))

  (define-language
    L1
    (extends Lsrc)
    (Expr (e)
          (- nv)
          (+ (scalar n))
          (+ (vector nv))))

  (define-pass differentiate-scalars : Lsrc (e) -> L1 ()
    (Expr : Expr (e) -> Expr ()
          [,nv (if (= 1 (length nv))
                 `(scalar ,[car nv])
                 `(vector ,nv))]))

  (define-language
    L2
    (extends L1)
    (terminals
      (- (adverb (a)))
      (- (verb (v)))
      (+ (primfun (pf))))
    (Expr (e)
      (- v)
      (- (system n))
      (- (adverb a e))
      (+ (primfun pf))))

  (define to-primfun-table
    '((plus . plus)
      (minus . minus)
      (each . map)
      (over . reduce)
      (0 . input-vector)
      (1 . input-scalar)
      (3 . display)
      (4 . output-vector)
      (5 . output-scalar)))

  (define primfun?
    (lambda (x)
      (not (not (memq x (append (map cdr to-primfun-table) '(zip)))))))

  (define (translate-to-primfun f)
    (cond
      [(assq f to-primfun-table) => cdr]
      [else (error 'translate-to-primfun "unsupported function" f)]))

  (define-pass translate-to-primfuns : L1 (e) -> L2 ()
    (Expr : Expr (e) -> Expr ()
      [(adverb ,a ,e) `(apply (primfun ,[translate-to-primfun a]) ,[Expr e])]
      [(system ,n) `(primfun ,[translate-to-primfun n])]
      [,v `(primfun ,[translate-to-primfun v])]))

  (define-language
    L3
    (extends L2)
    (entry Program)
    (Program (p)
      (+ (program e (s* ...))))
    (Expr (e)
      (- (dfn e))
      (- (apply e))
      (- (apply e0 e1 e2))
      (+ (lambda s e))))

   (define-pass introduce-lambda-abstractions : L2 (e) -> L3 ()
      (Expr : Expr (e) -> Expr (fv)
        [,s (values s (list s))]
        [(primfun ,pf) (values `(primfun ,pf) '())]
        [(scalar ,n) (values `(scalar ,n) '())]
        [(vector ,nv) (values `(vector ,nv) '())]
        [(apply ,e)
         (let-values ([(e^ fv) (Expr e)])
           (values `(apply ,e^ unit)))]
        [(apply ,e0 ,e1)
         (let-values ([(e0^ fv0) (Expr e0)]
                      [(e1^ fv1) (Expr e1)])
           (let ([fv (append fv0 fv1)])
             (values `(apply ,e0^ ,e1^) fv)))]
        [(apply ,e0 ,e1 ,e2)
         (let-values ([(e0^ fv0) (Expr e0)]
                      [(e1^ fv1) (Expr e1)]
                      [(e2^ fv2) (Expr e2)])
           (let ([fv (append fv0 fv1 fv2)])
             (values `(apply (apply ,e0^ ,e2^) ,e1^) fv)))]
        [(dfn ,e)
         (let-values ([(e^ fv) (Expr e)])
           (let ([has-a (memv 'a fv)]
                 [has-w (memv 'w fv)])
             (cond
               [(and has-w (not has-a))
                (values `(lambda w ,e^) (remove 'w fv))]
               [(and has-w has-a)
                (values `(lambda w (lambda a ,e^)) (remove 'a (remove 'w fv)))]
               [(and (not has-w) has-a)
                (values `(lambda a ,e^) (remove 'a fv))]
               [(and (not has-w) (not has-a))
                (error 'introduce-lambda-abstractions "nullary dfns not supported")]
             )))])
      (let-values ([(e^ fv) (Expr e)])
        `(program ,e^ (,fv ...))))

  (define (int? e) (eqv? e 'int))

  (define-language
    L4
    (extends L3)
    (terminals
      (+ (int (int))))
    (Type (t)
      (+ int)
      (+ (vector t)))
    (Expr (e)
      (- (vector nv))
      (- (scalar n))
      (+ (vector nv t))
      (+ (scalar n t))
      ))

  (define-pass type-scalars-and-vectors : L3 (e) -> L4 ()
    (Expr : Expr (e) -> Expr ()
      [(vector ,nv) `(vector ,nv (vector int))]
      [(scalar ,n) `(scalar ,n int)]))

  (define (typevar? e) (symbol? e))

  (define-language
    L5
    (extends L4)
    (terminals
      (+ (typevar (tv))))
    (Type (t)
      (+ (typevar tv)))
    (Expr (e)
      (- s)
      (+ (s t))
      (- (primfun pf))
      (+ (primfun pf t))
      (- (apply e0 e1))
      (+ (apply e0 e1 t))))

  (define-pass introduce-fresh-typevars : L4 (e) -> L5 ()
    (definitions
      (define typevar-counter 0)
      (with-output-language (L5 Type)
        (define fresh-typevar
          (lambda ()
            (let ([c typevar-counter])
              (set! typevar-counter (+ c 1))
              `(typevar ,(string->symbol (format "T~s" c)))))))
      (define (type-symbol s env)
        (cond
          [(assq s env) => (lambda (st) (values (cdr st) env))]
          [else (let ([t (fresh-typevar)])
                  (values t (cons `(,s . ,t) env)))]))
      (define (abstract-symbol s env)
        (cond
          [(assq s env) => (lambda (st) (remove st env))]
          [else env])))
    (Expr : Expr (e env) -> Expr (env)
      [,s
       (let-values ([(t env^) (type-symbol e env)])
         (values `(,s ,t) env^))]
      [(primfun ,pf)
       (values `(primfun ,pf ,[fresh-typevar]) env)]
      [(apply ,e0 ,e1)
       (let*-values ([(e0^ env0) (Expr e0 env)]
                     [(e1^ env1) (Expr e1 env0)])
         (values `(apply ,e0^ ,e1^ ,[fresh-typevar]) env1))]
      [(lambda ,s ,e)
       (let-values ([(e^ env^) (Expr e env)])
         (values `(lambda ,s ,e^) (abstract-symbol s env^)))])
    (Program : Program (p) -> Program ()
      [(program ,e (,s ...))
       (let-values ([(e^ env) (Expr e '())])
         `(program ,e^ (,s ...)))]))

  (define-language
    L6
    (extends L5)
    (Type (t)
      (+ (lambda t0 t1)))
    (Expr (e)
      (- (lambda s e))
      (+ (lambda (s t0) e t1)))
    (Global (g)
      (+ (s t)))
    (Program (p)
      (- (program e (s* ...)))
      (+ (program e t (g* ...)))))

  (define-pass type-lambda-abstractions : L5 (e) -> L6 ()
    (definitions
      (define (abstract-symbol s env)
        (cond
          [(assq s env) => (lambda (st) (values (cdr st) (remove st env)))]
          [else (error 'type-lambda-abstractions "unbound symbol" s env)]))
      (with-output-language (L6 Type)
        (define (mk-lambda-type t0 t1) `(lambda ,t0 ,t1)))
      (with-output-language (L6 Global)
        (define (mk-global-binding st) `(,(car st) ,(cdr st)))))
    (Type : Type (t) -> Type ())
    (Expr : Expr (e env) -> Expr (t env)
      [(,s ,t)
       (let ([t^ (Type t)])
         (values `(,s ,t^) t^ (cons (cons s t^) env)))]
      [(primfun ,pf ,t)
       (let ([t^ (Type t)])
         (values `(primfun ,pf ,t^) t^ env))]
      [(vector ,nv ,t)
       (let ([t^ (Type t)])
         (values `(vector ,nv ,t^) t^ env))]
      [(scalar ,n ,t) (values `(scalar ,n ,[Type t]) t env)]
      [(apply ,e0 ,e1 ,t)
       (let*-values ([(e0^ t0 env0) (Expr e0 env)]
                     [(e1^ t1 env1) (Expr e1 env0)])
         (let ([t^ (Type t)])
           (values `(apply ,e0^ ,e1^ ,t^) t^ env1)))]
      [(lambda ,s ,e)
       (let*-values ([(e^ t1 env^) (Expr e env)]
                     [(t0 env^^) (abstract-symbol s env^)])
         (let ([t^ (mk-lambda-type t0 t1)])
           (values `(lambda (,s ,t0) ,e^ ,t^) t^ env^^)))])
    (Program : Program (p) -> Program ()
      [(program ,e (,s* ...))
       (let-values ([(e^ t env) (Expr e '())])
         (let ([g* (map mk-global-binding env)])
           `(program ,e^ ,t (,g* ...))))]))

  (define-language
    L7
    (extends L6)
    (Constraint (c)
      (+ (t0 t1))
      (+ (overloaded t0 (t* ...))))
    (Program (p)
      (- (program e t (g* ...)))
      (+ (program e t (g* ...) (c* ...)))))

  (define-pass derive-type-constraints : L6 (e) -> L7 ()
    (definitions
      ; TODO: move the introduction of these type-variables to earlier pass
      (define typevar-counter 0)
      (with-output-language (L7 Type)
        (define fresh-typevar
          (lambda ()
            (let ([c typevar-counter])
              (set! typevar-counter (+ c 1))
              `(typevar ,(string->symbol (format "S~s" c)))))))
      (with-output-language (L7 Type)
        (define (mk-lambda-type t0 t1) `(lambda ,t0 ,t1))
        (define primfun-types-table
          (list
            (cons 'plus         (list `(lambda int (lambda int int))
                                      `(lambda int (lambda (vector int) (vector int)))
                                      `(lambda (vector int) (lambda int (vector int)))
                                      `(lambda (vector int) (lambda (vector int) (vector int)))))
            (cons 'input-vector `(vector int))
            (cons 'input-scalar 'int)
            (cons 'display      (list `(lambda int int) `(lambda (vector int) (vector int))))
            (cons 'map          (lambda ()
                                  (let ([a (fresh-typevar)]
                                        [b (fresh-typevar)])
                                    `(lambda (lambda ,a ,b) (lambda (vector ,a) (vector ,b))))))
            (cons 'reduce       (lambda ()
                                  (let ([a (fresh-typevar)])
                                    `(lambda (lambda ,a (lambda ,a ,a)) (lambda (vector ,a) ,a)))))
            (cons 'minus        (list
                                  `(lambda int int)
                                  `(lambda int (lambda int int))
                                  `(lambda (vector int) (lambda (vector int) (vector int)))))
            )))
      (with-output-language (L7 Constraint)
        (define (mk-constraint t0 t1) `(,t0 ,t1))
        (define (mk-overloading t0 t*) `(overloaded ,t0 (,t* ...)))))
    (Expr : Expr (e cs) -> Expr (t cs)
      [(,s ,t) (let ([t^ (Type t)]) (values `(,s ,t^) t^ cs))]
      [(primfun ,pf ,t)
       (cond
         [(assq pf primfun-types-table) =>
          (lambda (pft)
            (let* ([types (cdr pft)]
                   [c (cond
                        [(null? types) (error 'derive-type-constraints "no types for primfun" pf)]
                        [(list? types) (mk-overloading (Type t) types)]
                        [(procedure? types) (mk-constraint (Type t) (types))]
                        [else (mk-constraint (Type t) types)])])
              (let ([t^ (Type t)])
                (values `(primfun ,pf ,t^) t^ (cons c cs)))))]
         [else (error 'derive-type-constraints "untypeable primfun" pf)]
         )
       ]
      [(vector ,nv ,t) (let ([t^ (Type t)]) (values `(vector ,nv ,t^) t^ cs))]
      [(scalar ,n ,t) (let ([t^ (Type t)]) (values `(scalar ,n ,t^) t^ cs))]
      [(apply ,e0 ,e1 ,t)
       (let*-values ([(e0^ t0 cs0) (Expr e0 cs)]
                     [(e1^ t1 cs1) (Expr e1 cs)])
         (let* ([t^ (Type t)]
                [cs^ (cons (mk-constraint t0 (mk-lambda-type t1 t^)) (append cs0 cs1))])
           (values `(apply ,e0^ ,e1^ ,t^) t^ cs^)))]
      [(lambda (,s ,t0) ,e ,t1)
       (let*-values ([(e^ et^ cs^) (Expr e cs)])
         (let ([t1^ (Type t1)])
           (values `(lambda (,s ,(Type t0)) ,e^ ,t1^) t1^ cs^)))])
    (Global : Global (g) -> Global ())
    (Type : Type (t) -> Type ())
    (Program : Program (p) -> Program ()
      [(program ,e ,t (,g* ...))
       (let-values ([(e^ t^ cs) (Expr e '())])
          `(program ,e^ ,t^ (,(map Global g*) ...) (,cs ...)))]))

  (define (typevar-type? x)
    (and (list? x) (= 2 (length x)) (equal? 'typevar (car x))))

  (define (lambda-type? x)
    (and (list? x) (= 3 (length x)) (equal? 'lambda (car x))))

  (define (vector-type? x)
    (and (list? x) (= 2 (length x)) (equal? 'vector (car x))))

  (define (overloaded-constraint? x)
    (and (list? x) (= 3 (length x)) (equal? 'overloaded (car x))))

  (define (occurs x xs)
    (cond
      [(lambda-type? xs) (fold-left (lambda (acc ys) (or acc (occurs x ys))) #f (cdr xs))]
      [else (equal? x xs)]))

  (define (substitute x y xs)
    (cond
      [(lambda-type? xs) (cons 'lambda (map (lambda (ys) (substitute x y ys)) (cdr xs)))]
      [(overloaded-constraint? xs)
       (list 'overloaded (substitute x y (cadr xs)) (map (lambda (ys) (substitute x y ys)) (caddr xs)))]
      [(and (typevar-type? xs) (equal? x xs)) y]
      [(list? xs) (map (lambda (ys) (substitute x y ys)) xs)]
      [else xs]))

  (define (unify cs)
    (if (null? cs)
      (lambda (x) x)
      (let* ([c (car cs)]
             [cs^ (cdr cs)]
             [go (lambda (s t)
                   (cond
                     [(equal? s t) (unify cs^)]
                     [(and (typevar-type? s) (not (occurs s t)))
                      (cond
                        [(unify (substitute s t cs^)) => (lambda (f)
                                                           (lambda (x) (f (substitute s t x))))]
                        [else #f])]
                     [(and (typevar-type? t) (not (occurs t s)))
                      (cond
                        [(unify (substitute t s cs^)) => (lambda (f)
                                                           (lambda (x) (f (substitute t s x))))]
                        [else #f])]
                     [(and (lambda-type? s) (lambda-type? t))
                      (unify (append cs^ (list (list (cadr s) (cadr t)) (list (caddr s) (caddr t)))))]
                     [(and (vector-type? s) (vector-type? t))
                      (unify (append cs^ (list (list (cadr s) (cadr t)))))]
                     [else #f]))])
        (if (overloaded-constraint? c)
          (let ([alts (filter (lambda (x) x) (map (lambda (s) (go (cadr c) s)) (caddr c)))])
            (if (= 1 (length alts))
              (car alts)
              #f))
          (let ([s (car c)] [t (cadr c)]) (go s t))))))

  (define-language
    L8
    (extends L7)
    (terminals
      (- (typevar (tv))))
    (Type (t)
      (- (typevar tv)))
    (Constraint (c)
      (- (t0 t1))
      (- (overloaded t0 (t* ...))))
    (Program (p)
      (- (program e t (g* ...) (c* ...)))
      (+ (program e t (g* ...)))))

   (define-pass unify-and-substitute-types : L7 (e) -> L8 ()
     (Global : Global (g sub) -> Global ()
       [(,s ,t) `(,s ,[Type t sub])])
     (Type : Type (t sub) -> Type ()
       [(lambda ,t0 ,t1) `(lambda ,(Type t0 sub) ,(Type t1 sub))]
       [(typevar ,tv) (mk-Type (sub (list 'typevar tv)))]
       [(vector ,t) `(vector ,(Type t sub))])
     (mk-Type : * (t) -> Type ()
       (cond
         [(lambda-type? t) `(lambda ,(mk-Type (cadr t)) ,(mk-Type (caddr t))) ]
         [(vector-type? t) `(vector ,(mk-Type (cadr t)))]
         [else t]))
     (Expr : Expr (e sub) -> Expr ()
       [(primfun ,pf ,t) `(primfun ,pf ,(Type t sub))]
       [(vector ,nv ,t) `(vector ,nv ,(Type t sub))]
       [(scalar ,n ,t) `(scalar ,n ,(Type t sub))]
       [(apply ,e0 ,e1 ,t) `(apply ,(Expr e0 sub) ,(Expr e1 sub) ,(Type t sub))]
       [(lambda (,s ,t0) ,e ,t1) `(lambda (,s ,(Type t0 sub)) ,(Expr e sub) ,(Type t1 sub))])
     (Program : Program (p) -> Program ()
       [(program ,e ,t (,g* ...) (,c* ...))
        (let ([cs (map unparse-L7 c*)])
         (cond
           [(unify cs) => (lambda (sub)
            `(program ,(Expr e sub) ,(Type t sub) (,(map (lambda (g) (Global sub g)) g*) ...)))]
           [else (error 'unify-and-substitute-types "unable to unify types" cs)]
          ))]))

  (define-pass expand-idioms : L8 (e) -> L8 ()
    (unwrap-Type : Type (t) -> * ()
      [(vector ,t) `(vector ,t)]
      [(lambda ,t0 ,t1) `(lambda ,(unwrap-Type t0) ,(unwrap-Type t1))]
      [,int int])
    (Expr : Expr (e) -> Expr ()
      [(primfun ,pf ,t)
       (let ([t^ (unwrap-Type t)])
         (cond
           ; -2 -> 0-2
           [(and (equal? pf 'minus) (equal? '(lambda int int) t^))
            `(lambda (x int)
               (apply
                 (apply
                   (primfun minus (lambda int (lambda int int))) (x int) (lambda int int))
                 (scalar 0 int) int) (lambda int int))]
           ; 1 2 3+4 -> {w+4}'1 2 3
           [(and (equal? pf 'plus) (equal? '(lambda int (lambda (vector int) (vector int))) t^))
            `(lambda (x int)
               (lambda (xs (vector int))
                 (apply
                   (apply
                     (primfun map (lambda (lambda int int) (lambda (vector int) (vector int))))
                     (lambda (y int)
                       (apply
                         (apply
                           (primfun plus (lambda int (lambda int int)))
                           (x int)
                           (lambda int int))
                         (y int)
                         int)
                     (lambda int int))
                     (lambda (vector int) (vector int)))
                   (xs (vector int))
                   (vector int))
                 (lambda (vector int) (vector int)))
               (lambda int (lambda (vector int) (vector int))))]
           ; 1+2 3 -> 3 4
           [(and (equal? pf 'plus) (equal? '(lambda (vector int) (lambda int (vector int))) t^))
            `(lambda (xs (vector int))
               (lambda (x int)
                 (apply
                   (apply
                     (primfun map (lambda (lambda int int) (lambda (vector int) (vector int))))
                     (lambda (y int)
                       (apply
                         (apply
                           (primfun plus (lambda int (lambda int int)))
                           (y int)
                           (lambda int int))
                         (x int)
                         int)
                       (lambda int int))
                     (lambda (vector int) (vector int)))
                   (xs (vector int))
                   (vector int))
                 (lambda int (vector int)))
               (lambda (vector int) (lambda int (vector int))))]
           ; 1 2+3 4 or 1 2-3 4
           [(and (or (equal? pf 'plus) (equal? pf 'minus))
                 (equal? '(lambda (vector int) (lambda (vector int) (vector int))) t^))
            `(lambda (ys (vector int))
               (lambda (xs (vector int))
                 (apply
                   (apply
                     (apply
                       (primfun zip (lambda (lambda int (lambda int int)) (lambda (vector int) (lambda (vector int) (vector int)))))
                       (primfun ,pf (lambda int (lambda int int)))
                       (lambda (vector int) (lambda (vector int) (vector int))))
                     (ys (vector int))
                     (lambda (vector int) (vector int)))
                   (xs (vector int))
                   (vector int))
                 (lambda (vector int) (vector int)))
               (lambda (vector int) (lambda (vector int) (vector int))))]
           [(equal? pf 'display)
            (cond
              [(equal? '(lambda int int) t^)
               `(primfun output-scalar (lambda int int))]
              [(equal? '(lambda (vector int) (vector int)) t^)
               `(primfun output-vector (lambda (vector int) (vector int)))]
              [else (error 'expand-idioms "displaying unsupported type" t^)])]
           [else `(primfun ,pf ,t)]))]
      ))

  (define-pass type-check  : L8 (e) -> L8 ()
    (unwrap-Type : Type (t) -> * ()
      [(vector ,t) `(vector ,t)]
      [(lambda ,t0 ,t1) `(lambda ,(unwrap-Type t0) ,(unwrap-Type t1))]
      [,int int])
    (Expr : Expr (e ctx) -> Expr (t)
      [(,s ,t)
       (let ([t^ (unwrap-Type t)])
         (cond
           [(assq s ctx) => (lambda (st)
                              (cond
                                [(equal? t^ (cdr st)) (values `(,s ,t) t^)]
                                [else (error 'type-check "type error for symbol" (cons s t^) ctx)]))]
           [else (error 'type-check "unbound symbol" s ctx)]))]
      [(primfun ,pf ,t) (values `(primfun ,pf ,t) (unwrap-Type t))]
      [(scalar ,n ,t) (values `(scalar ,n ,t) (unwrap-Type t))]
      [(vector ,nv ,t) (values `(vector ,nv ,t) (unwrap-Type t))]
      [(apply ,e0 ,e1 ,t)
       (let-values ([(e0^ t0^) (Expr e0 ctx)]
                    [(e1^ t1^) (Expr e1 ctx)]
                    [(ut) (unwrap-Type t)])
         (cond
           [(and (lambda-type? t0^)
                 (equal? (cadr t0^) t1^)
                 (equal? (caddr t0^) ut))
            (values `(apply ,e0^ ,e1^ ,t) ut)]
           [else (error 'type-check "type error in application"
                        (list (unparse-L8 e0^) t0^) (list (unparse-L8 e1^) t1^) ut)]))]
      [(lambda (,s ,t0) ,e ,t1)
       (let*-values ([(ut0) (unwrap-Type t0)]
                     [(e^ t^) (Expr e (cons (cons s ut0) ctx))]
                     [(ut1) (unwrap-Type t1)])
         (cond
           [(and (lambda-type? ut1)
                (equal? (cadr ut1) ut0)
                (equal? (caddr ut1) t^))
            (values `(lambda (,s ,t0) ,e^ ,t1) ut1)]
           [else (error 'type-check "type error in lambda"
                        (list s ut0) (list (unparse-L8 e^) t^) ut1)]))]
      [else (error 'type-check "yo")])
    (Program : Program (p) -> Program ()
      [(program ,e ,t (,g* ...))
       (let-values ([(e^ t^) (Expr e '())]) ; TOOD: populate the ctx using the globals (or refactor out the globals)
         (let ([ut (unwrap-Type t)])
           (cond
             [(equal? ut t^) `(program ,e^ ,t (,g* ...))]
             [else (error 'type-check "type error in program" (list e ut) (list e^ t^))])))]))

  (define-language
    L9
    (extends L8)
    (terminals
      (- (int (int))))
    (Type (t)
      (- (lambda t0 t1))
      (- int)
      (- (vector t)))
    (Expr (e)
      (- (s t))
      (+ s)
      (- (scalar n t))
      (+ (scalar n))
      (- (vector nv t))
      (+ (vector nv))
      (- (primfun pf t))
      (+ (primfun pf))
      (- (apply e0 e1 t))
      (+ (apply e0 e1))
      (- (lambda (s t0) e t1))
      (+ (lambda (s) e)))
    (Global (g)
      (- (s t)))
    (Program (p)
      (- (program e t (g* ...)))
      (+ (program e (s* ...)))))

  (define-pass untype : L8 (e) -> L9 ()
    (Global : Global (g) -> Expr ()
      [(,s ,t) s])
    (Expr : Expr (e) -> Expr ()
      [(,s ,t) s]
      [(scalar ,n ,t) `(scalar ,n)]
      [(vector ,nv ,t) `(vector ,nv)]
      [(primfun ,pf ,t) `(primfun ,pf)]
      [(apply ,e0 ,e1 ,t) `(apply ,[Expr e0] ,[Expr e1])]
      [(lambda (,s ,t0) ,e ,t1) `(lambda (,s) ,[Expr e])]
      )
    (Program : Program (p) -> Program ()
      [(program ,e ,t (,g* ...))
       `(program ,(Expr e) (,[map Global g*] ...))]))


  (define-pass output-scheme : L9 (e) -> * ()
    (Expr : Expr (e) -> * ()
      [,s s]
      [(scalar ,n) n]
      [(vector ,nv) `(quote ,nv)]
      [(primfun ,pf)
       (cond
         [(equal? pf 'minus) '(lambda (y) (lambda (x) (- x y)))]
         [(equal? pf 'plus) '(lambda (y) (lambda (x) (+ x y)))]
         [(equal? pf 'map) '(lambda (f) (lambda (xs) (map f xs)))]
         [(equal? pf 'reduce)
          '(lambda (f)
            (lambda (xs)
              (letrec ([go (lambda (acc xs)
                             (cond
                               [(null? xs) acc]
                               [else (go ((f acc) (car xs)) (cdr xs))]))])
                (let ([rs (reverse xs)])
                  (go (car rs) (cdr rs))))))]
         [(equal? pf 'zip)
          '(lambda (f)
             (lambda (ys)
               (lambda (xs)
                 (letrec ([go (lambda (xs ys)
                                (cond
                                  [(and (null? xs) (null? ys)) '()]
                                  [else (cons ((f (car ys)) (car xs)) (go (cdr xs) (cdr ys)))]))])
                   (go xs ys)))))]
         [(equal? pf 'input-scalar)
          '(string->number (get-line (current-input-port)))]
         [(equal? pf 'input-vector)
          '(with-input-from-string (format "(~a)" (get-line (current-input-port))) read)]
         [(equal? pf 'output-scalar) 'display]
         [(equal? pf 'output-vector)
          '(letrec ([print-vector (lambda (xs)
                                    (display (car xs))
                                    (let ([tail (cdr xs)])
                                      (cond
                                        [(null? tail) (void)]
                                        [else (display " ") (print-vector tail)])))])
             print-vector)]
         [else (error 'output-scheme "unsupported primitive function" pf)])]
      [(apply ,e0 ,e1) `(,(Expr e0) ,(Expr e1))]
      [(lambda (,s) ,e) `(lambda (,s) ,[Expr e])])
    (Program : Program (p) -> * ()
      [(program ,e (,s* ...)) (Expr e)]))


  (define-pass output-malfunction : L9 (e) -> * ()
    (definitions
      (define (mlf-symbol s) (string->symbol (format "$~s" s)))
      (define mlf-write-scalar-lambda
        '(global $Pervasives $print_int))
      (define mlf-unit '(block (tag 0)))
      (define malfunction-print-newline
        `(apply (global $Pervasives $print_newline) ,mlf-unit))
      (define malfunction-print-space
        `(apply (global $Pervasives $print_char) 32))
      (define (malfunction-error reason code)
        `(seq (apply (global $Pervasives $prerr_endline) ,reason)
              (apply (global $Pervasives $exit) ,code)))
      (define malfunction-length-error
        (malfunction-error "length error" 3))
      (define mlf-write-vector-lambda
        `(lambda ($l)
           (let ($n (length $l))
             (apply (global $Array $iteri)
                    (lambda ($i $x)
                      (seq
                        (apply $write_scalar $x)
                        (if (< (+ $i 1) $n)
                          ,malfunction-print-space
                          ,mlf-unit)))
                    $l))))
      (define malfunction-map-lambda
        `(lambda ($f $v)
           (apply (global $Array $map) $f $v)))
      (define malfunction-zip-lambda
        `(lambda ($f $v $w)
           (let
             ($n (length $v))
             ($m (length $w))
             (if (== $n $m)
               (apply (global $Array $map2) $f $v $w)
               ,malfunction-length-error))))
      (define malfunction-foldr-lambda
        `(lambda ($f $b $v)
           (apply (global $Array $fold_right) $f $v $b)))
      (define malfunction-reduce-lambda
        `(lambda ($f $v)
           (let
             ($n (length $v))
             (rec ($go (lambda ($acc $i)
                         (switch $i
                           (-1 $acc)
                           (_ (apply $go (apply $f $acc (load $v $i)) (- $i 1)))))))
             (apply $go (load $v (- $n 1)) (- $n 2)))))
      (define (make-malfunction-vector l)
        (letrec ([go (lambda (i k)
                       (if (null? k)
                         '()
                         (cons `(store $v ,i ,(car k)) (go (+ i 1) (cdr k)))))])
          (let ([body (append '(seq) (go 0 l) '($v))])
            `(let ($v (makevec ,(length l) 0)) ,body))))
      (define malfunction-read-scalar-lambda
        `(lambda ($x)
           (apply (global $Pervasives $read_int) ,mlf-unit)))
      (define malfunction-read-vector-lambda
        `(lambda ($x)
           (apply (global $Array $map)
                  (lambda ($s) (apply (global $Pervasives $int_of_string) $s))
                  (apply (global $Array $of_list)
                         (apply (global $Str $split) (apply (global $Str $regexp) " +")
                                (apply (global $Pervasives $read_line) ,mlf-unit)))))))
    (Expr : Expr (e) -> * ()
      [,s (mlf-symbol s)]
      [(scalar ,n) n]
      [(vector ,nv) (make-malfunction-vector nv)]
      [(primfun ,pf)
       (cond
         [(equal? pf 'minus)
          `(lambda ($y $x) (- $x $y))]
         [(equal? pf 'plus)
          `(lambda ($x $y) (+ $x $y))]
         [(equal? pf 'map)
           `(lambda ($f $xs) (apply $map $f $xs))]
         [(equal? pf 'reduce)
          `(lambda ($f $xs) (apply $reduce $f $xs))]
         [(equal? pf 'zip)
          `(lambda ($f $ys $xs) (apply $zip $f $ys $xs))]
         [(equal? pf 'input-vector)
          `(apply $read_vector ,mlf-unit)]
         [(equal? pf 'input-scalar)
          `(apply $read_scalar ,mlf-unit)]
         [(equal? pf 'output-scalar)
          '$write_scalar]
         [(equal? pf 'output-vector)
          '$write_vector]
         [else (error 'output-malfunction "unsupported primitive function" pf)])]
      [(apply ,e0 ,e1) `(apply ,(Expr e0) ,(Expr e1))]
      [(lambda (,s) ,e) `(lambda (,[mlf-symbol s]) ,[Expr e])]
      [else (error 'output-malfunction "unsupported expr" e)])
    (Program : Program (p) -> * ()
      [(program ,e (,s* ...))
       `(module
          ($map ,malfunction-map-lambda)
          ($zip ,malfunction-zip-lambda)
          ($foldr ,malfunction-foldr-lambda)
          ($reduce ,malfunction-reduce-lambda)
          ($read_scalar ,malfunction-read-scalar-lambda)
          ($read_vector ,malfunction-read-vector-lambda)
          ($write_scalar ,mlf-write-scalar-lambda)
          ($write_vector ,mlf-write-vector-lambda)
          (_ ,[Expr e])
          (_ ,malfunction-print-newline)
          (export))
       ]))

  (define (id x) x)

  (define passes
    '((parse-silly-k                 . id)
      (ast-to-Lsrc                   . unparse-Lsrc)
      (differentiate-scalars         . unparse-L1)
      (translate-to-primfuns         . unparse-L2)
      (introduce-lambda-abstractions . unparse-L3)
      (type-scalars-and-vectors      . unparse-L4)
      (introduce-fresh-typevars      . unparse-L5)
      (type-lambda-abstractions      . unparse-L6)
      (derive-type-constraints       . unparse-L7)
      (unify-and-substitute-types    . unparse-L8)
      (type-check                    . unparse-L8)
      (expand-idioms                 . unparse-L8)
      (type-check                    . unparse-L8)
      (untype                        . unparse-L9)
      (output-scheme                 . id)
      ))

  (define (compiler-frontend)
    (untype
      (expand-idioms
        (unify-and-substitute-types
          (derive-type-constraints
            (type-lambda-abstractions
              (introduce-fresh-typevars
                (type-scalars-and-vectors
                  (introduce-lambda-abstractions
                    (translate-to-primfuns
                      (differentiate-scalars
                        (ast-to-Lsrc
                          (parse-silly-k)))))))))))))

  (define (compile-to-malfunction)
    (output-malfunction (compiler-frontend)))

  (define (compile-to-scheme)
    (output-scheme (compiler-frontend)))

  (define compile-silly-k
    (lambda (o)
      (compile-malfunction o (compile-to-malfunction))))

  (define (compile-silly-k-and-run)
    (compile "a.out")
    (assert (= 0 (system "./a.out")))
    (void))

  (define compile-malfunction
    (lambda (out mlf)
      (let* ([pn (port-name (current-input-port))]
             [fn (string-append pn ".mlf")] )
        (with-output-to-file fn
          (lambda ()
            (write mlf)
            (flush-output-port)
            (assert (= 0 (system (format "exec malfunction cmx ~s" fn))))
            (assert (= 0 (system (format "exec ocamlopt.opt -o ~s str.cmxa ~s.cmx" out pn)))))
          '(replace)))))

  )
