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
          embedd-L3-into-L3-spine-intermediate
          introduce-let-spine            unparse-L3-spine-intermediate
          translate-L3-spine-intermediate-into-L3-spine  unparse-L3-spine
          remove-lets                    unparse-L3-without-let
          disable-let-binding
          type-scalars-and-vectors       unparse-L4
          pick-spine-values              unparse-L3-picked-spine-values
          introduce-fresh-typevars       unparse-L5
          type-lambda-abstractions       unparse-L6
          add-coercion-points            unparse-L6-with-coercion
          derive-type-constraints        unparse-L7
          unify-and-substitute-types     unparse-L8
          coerce-values                  unparse-L9
          expand-idioms
          type-check
          untype                         unparse-L10
          output-malfunction
          output-scheme
          )
  (import (nanopass)
          (chezscheme)
          (lalr))

  (define (id x) x)

  (define passes
    '((parse-silly-k                                 . id)
      (ast-to-Lsrc                                   . unparse-Lsrc)
      (differentiate-scalars                         . unparse-L1)
      (translate-to-primfuns                         . unparse-L2)
      (introduce-lambda-abstractions                 . unparse-L3)
      (embedd-L3-into-L3-spine-intermediate          . unparse-L3-spine-intermediate)
      (introduce-let-spine                           . unparse-L3-spine-intermediate)
      (translate-L3-spine-intermediate-into-L3-spine . unparse-L3-spine)
      (pick-spine-values                             . unparse-L3-picked-spine-values)
      (remove-lets                                   . unparse-L3-without-let)
      ;(disable-let-binding                           . unparse-L3-without-let)
      (type-scalars-and-vectors                      . unparse-L4)
      (introduce-fresh-typevars                      . unparse-L5)
      (type-lambda-abstractions                      . unparse-L6)
      (add-coercion-points                           . unparse-L6-with-coercion)
      (derive-type-constraints                       . unparse-L7)
      (unify-and-substitute-types                    . unparse-L8)
      (coerce-values                                 . unparse-L9)
      (type-check                                    . unparse-L9)
      (expand-idioms                                 . unparse-L9)
      (type-check                                    . unparse-L9)
      (untype                                        . unparse-L10)
      (output-scheme                                . id)
      ;(output-malfunction                            . id)
      ))


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
            [(char=? c #\;)       (make-lexical-token 'SEMICOLON location #f)]
            [(char=? c #\-)       (make-lexical-token 'MINUS location #f)]
            [(char=? c #\()       (make-lexical-token 'LPAREN location #f)]
            [(char=? c #\))       (make-lexical-token 'RPAREN location #f)]
            [(char=? c #\{)       (make-lexical-token 'LBRACE location #f)]
            [(char=? c #\})       (make-lexical-token 'RBRACE location #f)]
            [(char=? c #\])       (make-lexical-token 'RBRACKET location #f)]
            [(char=? c #\[)       (make-lexical-token 'LBRACKET location #f)]
            [(char=? c #\|)       (make-lexical-token 'PIPE location #f)]
            [(char=? c #\<)       (make-lexical-token 'LESS location #f)]
            [(char=? c #\>)       (make-lexical-token 'MORE location #f)]
            [(char=? c #\&)       (make-lexical-token 'AMPERSAND location #f)]
            [(char=? c #\~)       (make-lexical-token 'TILDE location #f)]
            [(char=? c #\*)       (make-lexical-token 'STAR location #f)]
            [(char=? c #\@)       (make-lexical-token 'AT location #f)]
            [(char=? c #\/)       (make-lexical-token 'SLASH location #f)]
            [(char=? c #\')       (make-lexical-token 'QUOTE location #f)]
            [(char=? c #\=)       (make-lexical-token 'EQUAL location #f)]
            [(char=? c #\_)       (make-lexical-token 'UNDERSCORE location #f)]
            [(char=? c #\!)       (make-lexical-token 'EXCLAMATION location #f)]
            [(char=? c #\#)       (make-lexical-token 'HASH location #f)]
            [(char-numeric? c)    (make-lexical-token 'NUM location (read-number `(,c)))]
            [(char-alphabetic? c) (make-lexical-token 'ATOM location (read-atom `(,c)))]
            [else (error 'lex "Unrecognized character" c)])))))

  (define parse-silly-k
    (lambda ()
      (let ([parser
              (lalr-parser
                (PLUS (left: NUM) MINUS LPAREN RPAREN
                 SLASH QUOTE COLON NEWLINE LBRACE RBRACE
                 ATOM LBRACKET RBRACKET AT EQUAL SEMICOLON
                 UNDERSCORE PIPE AMPERSAND TILDE STAR
                 LESS MORE EXCLAMATION HASH)
                (statement (expr) : $1
                           (expr NEWLINE) : $1)
                (expr (expr AT expr) : `(apply ,$1 #f ,$3)
                      (expr verb expr) : `(apply ,$2 ,$1 ,$3)
                      (verb expr) : `(apply ,$1 #f ,$2)
                      (ATOM COLON expr) : `(let ,$1 ,$3)
                      (LPAREN expr RPAREN) : $2
                      (LPAREN cond RPAREN) : `(cond . ,$2)
                      (num) : $1
                      (verb) : $1
                      (ATOM) : $1
                      )
                (cond (expr SEMICOLON expr SEMICOLON cond) :
                      `((,$1 ,$3) . ,$5)
                      (expr SEMICOLON expr SEMICOLON expr) :
                      `((,$1 ,$3) (else ,$5)))
                (verb (PLUS) : 'plus
                      (MINUS) : 'minus
                      (EQUAL) : 'equal
                      (PIPE) : 'max
                      (AMPERSAND) : 'min
                      (TILDE) : 'negation
                      (STAR) : 'star
                      (LESS) : 'less
                      (MORE) : 'more
                      (EXCLAMATION) : 'iota
                      (HASH) : 'shape
                      (NUM COLON) : `(system ,$1)
                      (RBRACKET) : '(system 3)
                      (LBRACE cond RBRACE) : `(dfn (cond . ,$2))
                      (LBRACE expr RBRACE) : `(dfn ,$2)
                      (verb adverb) : `(adverb ,$2 ,$1)
                      (UNDERSCORE ATOM) : (string->symbol (string-append "_" (symbol->string $2))))
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
      (not (not (memq x '(plus minus colon equal max min negation star less more iota shape))))))

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
    (entry Expr)
    (CondArm (ca)
      (e0 e1)
      (else e))
    (Expr (e)
      nv
      s
      v
      (system n)
      (adverb a e)
      (dfn e)
      (cond (ca* ...))
      (let s e)
      (apply e)
      (apply e0 e1)
      (apply e0 e1 e2)))

  (define-pass ast-to-Lsrc : * (e) -> Lsrc ()
    (CondArm : * (e) -> CondArm ()
      (if (equal? (car e) 'else)
        `(else ,[Expr (cadr e)])
        `(,[Expr (car e)] ,[Expr (cadr e)])))
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
        [(and (list? e) (eq? (car e) 'cond))
         (let ([ca* (map CondArm (cdr e))])
           `(cond (,ca* ...)))]
        [(and (list? e) (eq? (car e) 'let))
         `(let ,[Expr (cadr e)], [Expr (caddr e)])]
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
      (equal . equal)
      (each . map)
      (over . reduce)
      (min . min)
      (max . max)
      (less . less)
      (more . more)
      (iota . iota)
      (shape . shape)
      (negation . negation)
      (star . star)
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
      (+ (program e)))
    (Expr (e)
      (- (dfn e))
      (- (apply e))
      (- (apply e0 e1 e2))
      (+ (lambda s e))
      (+ (lambda-rec s e))))

  (define self '_f)

   (define-pass introduce-lambda-abstractions : L2 (e) -> L3 ()
      (CondArm : CondArm (ca) -> CondArm (fv)
        [(else ,e)
         (let-values ([(e^ fv) (Expr e)])
           (values `(else ,e^) fv))]
        [(,e0 ,e1)
         (let-values ([(e0^ fv0) (Expr e0)]
                      [(e1^ fv1) (Expr e1)])
           (let ([fv (append fv0 fv1)])
             (values `(,e0^ ,e1^) fv)))])
      (Expr : Expr (expr) -> Expr (fv)
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
        [(let ,s ,e)
         (let-values ([(e^ fv) (Expr e)])
           (values `(let ,s ,e^) fv))]
        [(dfn ,e)
         (let-values ([(e^ fv) (Expr e)])
           (let ([has-a (memv 'a fv)]
                 [has-w (memv 'w fv)]
                 [is-recursive (memv self fv)])
             (cond
               [(and has-w (not has-a) is-recursive)
                (values `(lambda-rec w ,e^) (remove 'w fv))]
               [(and has-w (not has-a))
                (values `(lambda w ,e^) (remove 'w fv))]

               [(and has-w has-a)
                (values `(lambda w (lambda a ,e^)) (remove 'a (remove 'w fv)))]
               [(and has-w has-a is-recursive)
                (values `(lambda-rec w (lambda-rec a ,e^)) (remove 'a (remove 'w fv)))]

               [(and (not has-w) has-a)
                (values `(lambda a ,e^) (remove 'a fv))]
               [(and (not has-w) has-a is-recursive)
                (values `(lambda-rec a ,e^) (remove 'a fv))]

               [(and (not has-w) (not has-a))
                (error 'introduce-lambda-abstractions "nullary dfns not supported" (unparse-L2 e))]
             )))]
        [(cond (,ca* ...))
         (let* ([cas (map (lambda (ca)
                            (let-values ([(ca^ fv) (CondArm ca)]) (list ca^ fv)))
                          ca*)]
                [ca*^ (map car cas)]
                [fv (fold-left append '() (map cadr cas))])
           (values `(cond (,ca*^ ...)) fv))])
      (let-values ([(e^ fv) (Expr e)])
        `(program ,e^)))

  (define-language
    L3-spine-intermediate
    (extends L3)
    (Spine (sp)
      (+ (spine s)))
    (Expr (e)
      (+ (let (sp e0) e1))
      (+ (apply-half sp e))
      (+ (apply-full sp0 sp1)))
    (Program (p)
      (- (program e))
      (+ (program e))))

  (define-pass embedd-L3-into-L3-spine-intermediate : L3 (e) -> L3-spine-intermediate ()
     (CondArm : CondArm (ca) -> CondArm ())
     (Expr : Expr (expr) -> Expr ()
       [(apply ,e0 ,e1) `(apply ,[Expr e0] ,[Expr e1])]
       [(lambda ,s ,e) `(lambda ,s ,[Expr e])]
       [(lambda-rec ,s ,e) `(lambda-rec ,s ,[Expr e])]
       [(cond (,ca* ...))
        `(cond (,[map CondArm ca*] ...))])
    (Program : Program (p) -> Program ()
      [(program ,e)
       `(program ,[Expr e])]))


  (define-pass introduce-let-spine : L3-spine-intermediate (e) -> L3-spine-intermediate ()
    (definitions
      (define spine-counter 0)
      (with-output-language (L3-spine-intermediate Spine)
        (define fresh-spine-symbol
          (lambda ()
            (let* ([c spine-counter]
                   [s (string->symbol (format "spine-~s" c))])
              (set! spine-counter (+ c 1))
              `(spine ,s))))))
     (CondArm : CondArm (ca) -> CondArm ())
     (Expr : Expr (expr) -> Expr ()
       [(apply-half ,sp0 ,e)
        (let* ([sp1 (fresh-spine-symbol)])
          (Expr `(let (,sp1 ,[Expr e]) (apply-full ,sp0 ,sp1))))]
       [(apply ,e0 ,e1)
        (let* ([sp (fresh-spine-symbol)])
          (Expr `(let (,sp ,e0) (apply-half ,sp ,e1))))]
       [(let (,sp (apply ,e0 ,e1)) ,e2)
        (let* ([sp0 (fresh-spine-symbol)])
          (Expr `(let (,sp0 ,e0) (let (,sp (apply-half ,sp0 ,e1)) ,e2))))]
       [(let (,sp (apply-half ,sp0 ,e1)) ,e2)
        (let* ([sp1 (fresh-spine-symbol)])
          (Expr `(let (,sp1 ,e1) (let (,sp (apply-full ,sp0 ,sp1)) ,e2))))]
       [(let (,sp (apply-full ,sp0 ,sp1)) ,e2)
        (let ([body (Expr e2)])
          `(let (,sp (apply-full ,sp0 ,sp1)) ,body))]))

  (define-language
    L3-spine
    (extends L3-spine-intermediate)
    (Spine (sp)
      (- (spine s)))
    (Expr (e)
      (- (let (sp e0) e1))
      (+ (let (s e0) e1))
      (- (apply-half sp e))
      (- (apply-full sp0 sp1))))

  (define-pass translate-L3-spine-intermediate-into-L3-spine : L3-spine-intermediate (e) -> L3-spine ()
    (Spine : Spine (sp) -> Expr ()
      [(spine ,s) s])
    (CondArm : CondArm (ca) -> CondArm ())
    (Expr : Expr (expr) -> Expr ()
      [(apply-full ,sp0 ,sp1) `(apply ,[Spine sp0] ,[Spine sp1])]
      [(apply-half ,sp0 ,e) (void)]
      [(apply ,e0 ,e1) (void)]
      [(let (,sp ,e1) ,e2)
       `(let (,(Spine sp) ,[Expr e1]) ,[Expr e2])]))

  (define-language
    L3-picked-spine-values
    (extends L3-spine)
    (Expr (e)
      (- (let s e))))

  (define-pass pick-spine-values : L3-spine (e) -> L3-picked-spine-values ()
    (Expr : Expr (expr env) -> Expr ()
      [(let (,s1 (let ,s2 ,e1)) ,e2)
       (let ([env^ (cons (cons s2 s1) env)])
       `(let (,s1 ,[Expr e1 env^]) ,[Expr e2 env^]))]
      [,s (cond [(assq s env) => cdr] [else s])]
      [(let ,s ,e) (Expr e env)])
    (Program : Program (p) -> Program ()
      [(program ,e)
       `(program ,[Expr e '()])]))

  (define-language
    L3-without-let
    (extends L3-picked-spine-values)
    (Expr (e)
      (- (let (s e0) e1))))

  (define-pass remove-lets : L3-picked-spine-values (e) -> L3-without-let ()
    (Expr : Expr (expr) -> Expr ()
      [(let (,s ,e1) ,e2)
       `(apply (lambda ,s ,[Expr e2]) ,[Expr e1])]))


  (define (int? e) (eqv? e 'int))

  (define-language
    L4
    (extends L3-without-let)
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

  (define-pass disable-let-binding : L3 (e) -> L3-without-let ())

  (define-pass type-scalars-and-vectors : L3-without-let (e) -> L4 ()
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
      (+ (apply e0 e1 t))
      (- (cond (ca* ...)))
      (+ (cond (ca* ...) t))
      ))


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
    (CondArm : CondArm (ca env) -> CondArm (env)
      [(,e0 ,e1)
       (let*-values ([(e0^ env0) (Expr e0 env)]
                     [(e1^ env1) (Expr e1 env0)])
         (values `(,e0^ ,e1^) env1))]
      [(else ,e)
       (let*-values ([(e^ env^) (Expr e env)])
         (values `(else ,e^) env^))])
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
         (values `(lambda ,s ,e^) (abstract-symbol s env^)))]
      [(lambda-rec ,s ,e)
       (let-values ([(e^ env^) (Expr e env)])
         (values `(lambda-rec ,s ,e^) (abstract-symbol self (abstract-symbol s env^))))]
      [(cond (,ca* ...))
       (let* ([cas-env (fold-left
                         (lambda (acc ca)
                           (let-values ([(ca^ env^) (CondArm ca (cadr acc))])
                                       (list (append (car acc) (list ca^))
                                             env^)))
                         (list '() env)
                         ca*)]
              [ca*^ (car cas-env)]
              [env^ (cadr cas-env)])
         (values `(cond (,ca*^ ...) ,[fresh-typevar]) env^))])
    (Program : Program (p) -> Program ()
      [(program ,e)
       (let-values ([(e^ env) (Expr e '())])
         `(program ,e^))]))

  (define-language
    L6
    (extends L5)
    (Type (t)
      (+ (lambda t0 t1)))
    (Expr (e)
      (- (lambda s e))
      (+ (lambda (s t0) e t1))
      (- (lambda-rec s e))
      (+ (lambda-rec (s t0) e t1 t2)))
    (Program (p)
      (- (program e))
      (+ (program e t))))

  (define-pass type-lambda-abstractions : L5 (e) -> L6 ()
    (definitions
      (define (abstract-symbol s env)
        (cond
          [(assq s env) => (lambda (st) (values (cdr st) (remove st env)))]
          [else (error 'type-lambda-abstractions "unbound symbol" s env)]))
      (define (type-symbol s t env)
        (cond
          [(assq s env) => (lambda (st) env)]
          [else (cons `(,s . ,t) env)]))
      (with-output-language (L6 Type)
        (define (mk-lambda-type t0 t1) `(lambda ,t0 ,t1))))
    (Type : Type (t) -> Type ())
    (CondArm : CondArm (ca env) -> CondArm (env)
      [(,e0 ,e1)
       (let*-values ([(e0^ t0 env0) (Expr e0 env)]
                     [(e1^ t1 env1) (Expr e1 env0)])
         (values `(,e0^ ,e1^) env1))]
      [(else ,e)
       (let*-values ([(e^ t env^) (Expr e env)])
         (values `(else ,e^) env^))])
    (Expr : Expr (e env) -> Expr (t env)
      [(,s ,t)
       (let ([t^ (Type t)])
         (values `(,s ,t^) t^ (type-symbol s t^ env)))]
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
           (values `(lambda (,s ,t0) ,e^ ,t^) t^ env^^)))]
      [(lambda-rec ,s ,e)
       (let*-values ([(e^ t1 env^) (Expr e env)]
                     [(t0 env^^) (abstract-symbol s env^)]
                     [(selft env^^^) (abstract-symbol self env^^)])
         (let ([t^ (mk-lambda-type t0 t1)])
           (values `(lambda-rec (,s ,t0) ,e^ ,t^ ,selft) t^ env^^^)))]
      [(cond (,ca* ...) ,t)
       (let* ([cas-env (fold-left
                         (lambda (acc ca)
                           (let-values ([(ca^ env^^) (CondArm ca (cadr acc))])
                                       (list (append (car acc) (list ca^))
                                             env^^)))
                         (list '() env)
                         ca*)]
              [ca*^ (car cas-env)]
              [env^ (cadr cas-env)]
              [t^ (Type t)])
         (values `(cond (,ca*^ ...) ,t^) t^ env^))])
    (Program : Program (p) -> Program ()
      [(program ,e)
       (let-values ([(e^ t env) (Expr e '())])
         `(program ,e^ ,t))]))


  (define-language
    L6-with-coercion
    (extends L6)
    (Expr (e)
      (+ (coerce e t))))

  (define-pass add-coercion-points : L6 (e) -> L6-with-coercion ()
    (definitions
      (define typevar-counter 0)
      (with-output-language (L6-with-coercion Type)
        (define fresh-typevar
          (lambda ()
            (let ([c typevar-counter])
              (set! typevar-counter (+ c 1))
              `(typevar ,(string->symbol (format "C~s" c))))))))
    (Type : Type (t) -> Type ())
    (Expr : Expr (expr) -> Expr ()
      [(apply ,e0 ,e1 ,t)
       `(apply (coerce ,[Expr e0] ,[fresh-typevar]) (coerce ,[Expr e1] ,[fresh-typevar]) ,[Type t])]))

  (define (bool? e) (eqv? e 'bool))

  (define-language
    L7
    (extends L6-with-coercion)
    (terminals
      (+ (bool (bool))))
    (Type (t)
      (+ bool))
    (Constraints (cs)
      (+ (c ...)))
    (Constraint (c)
      (+ (t0 t1))
      (+ (overloaded cs* ...)))
    (Program (p)
      (- (program e t))
      (+ (program e t (c* ...)))))

  (define-pass derive-type-constraints : L6-with-coercion (e) -> L7 ()
    (definitions
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
            (cons 'display      (list `(lambda int int)
                                      `(lambda (vector int) (vector int))))
            (cons 'map          (list (lambda ()
                                        (let ([a (fresh-typevar)]
                                              [b (fresh-typevar)])
                                          `(lambda (lambda ,a ,b)
                                             (lambda (vector ,a) (vector ,b)))))
                                      (lambda ()
                                        (let ([a (fresh-typevar)]
                                              [b (fresh-typevar)]
                                              [c (fresh-typevar)])
                                          `(lambda (lambda ,a (lambda ,b ,c))
                                             (lambda (vector ,a)
                                             (lambda ,b (vector ,c))))))))
            (cons 'reduce       (lambda ()
                                        (let ([a (fresh-typevar)])
                                          `(lambda (lambda ,a (lambda ,a ,a)) (lambda (vector ,a) ,a)))))
            (cons 'minus        (list
                                  `(lambda int int)
                                  `(lambda int (lambda int int))
                                  `(lambda int (lambda (vector int) (vector int)))
                                  `(lambda (vector int) (lambda int (vector int)))
                                  `(lambda (vector int) (lambda (vector int) (vector int)))))
            (cons 'star         (list `(lambda int (lambda int int))
                                      `(lambda int (lambda (vector int) (vector int)))
                                      `(lambda (vector int) (lambda int (vector int)))
                                      `(lambda (vector int) (lambda (vector int) (vector int)))
                                      (lambda ()
                                         (let ([a (fresh-typevar)])
                                           `(lambda (vector ,a) ,a)))))
            (cons 'equal        (list `(lambda int (lambda int bool))
                                      `(lambda (vector int) (lambda int (vector bool)))
                                      `(lambda int (lambda (vector int) (vector bool)))))
            (cons 'less         (list `(lambda int (lambda int bool))
                                      `(lambda (vector int) (lambda int (vector bool)))
                                      `(lambda int (lambda (vector int) (vector bool)))))
            (cons 'more         (list `(lambda int (lambda int bool))
                                      `(lambda (vector int) (lambda int (vector bool)))
                                      `(lambda int (lambda (vector int) (vector bool)))))
            (cons 'min          (list `(lambda (vector int) (vector int))
                                      `(lambda bool (lambda bool bool))
                                      `(lambda int (lambda int int))))
            (cons 'max          (list `(lambda bool (lambda bool bool))
                                      `(lambda int (lambda int int))))
            (cons 'negation     (list `(lambda bool bool)
                                      `(lambda int bool)
                                      `(lambda (vector bool) (vector bool))))
            (cons 'iota         `(lambda int (vector int)))
            (cons 'shape        (list (lambda ()
                                        (let ([a (fresh-typevar)])
                                          `(lambda (vector ,a) (lambda int (vector ,a)))))
                                      (lambda ()
                                        (let ([a (fresh-typevar)])
                                          `(lambda ,a (lambda int (vector ,a)))))
                                      (lambda ()
                                        (let ([a (fresh-typevar)])
                                             `(lambda (vector ,a) int)))))
            )))
      (define (fresh-type-instance t)
        (cond
          [(procedure? t) (t)]
          [else t]))
      (with-output-language (L7 Constraint)
        (define (mk-bool-constraint t) `(,t bool))
        (define (mk-constraint t0 t1) `(,t0 ,t1))
        (define (mk-overloading t0 t*)
          `(overloaded ,[map (lambda (t1) `((,t0 ,(fresh-type-instance t1)))) t*] ...))
        (define (mk-coerce-constraint t0 t1)
          (let ([a (fresh-typevar)])
            `(overloaded ((,t0 ,t1))
                         ((,t0 bool) (,t1 int))
                         ((,t0 (vector bool)) (,t1 (vector int)))
                         ((,t0 (vector ,a)) (,t1 (lambda int ,a)))
                         ((,t0 (vector ,a)) (,t1 (lambda (vector int) (vector ,a)))))))))
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
           (values `(lambda (,s ,(Type t0)) ,e^ ,t1^) t1^ cs^)))]
      [(lambda-rec (,s ,t0) ,e ,t1 ,t2)
       (let*-values ([(e^ et^ cs^) (Expr e cs)])
         (let ([t1^ (Type t1)] [t2^ (Type t2)])
           (values `(lambda-rec (,s ,(Type t0)) ,e^ ,t1^ ,t2^) t1^ (cons (mk-constraint t1^ t2^) cs^))))]
      [(coerce ,e ,t)
       (let-values ([(e^ et^ cs^) (Expr e cs)])
         (let* ([t^ (Type t)]
                [cs^^ (cons (mk-coerce-constraint et^ t^) cs^)])
           (values `(coerce ,e^ ,t^) t^ cs^^)))]
      [(cond (,ca* ...) ,t)
       (let* ([t^ (Type t)]
              [cas-cs (fold-left
                        (lambda (acc ca)
                          (let-values ([(ca^ cs^) (CondArm ca t^ (cadr acc))])
                            (list (append (car acc) (list ca^)) cs^)))
                        (list '() cs)
                        ca*)]
              [ca*^ (car cas-cs)]
              [env^ (cadr cas-cs)])
         (values `(cond (,ca*^ ...) ,t^) t^ env^))])
    (CondArm : CondArm (ca t cs) -> CondArm (cs)
      [(,e0 ,e1)
       (let*-values ([(e0^ t0 cs0) (Expr e0 cs)]
                     [(e1^ t1 cs1) (Expr e1 cs)])
         (let* ([cs^ (append (list (mk-bool-constraint t0)
                                   (mk-constraint t t1))
                             (append cs0 cs1))])
           (values `(,e0^ ,e1^) cs^)))]
      [(else ,e)
       (let*-values ([(e^ t^ cs^) (Expr e cs)])
         (let* ([cs^^ (cons (mk-constraint t t^) (append cs^ cs))])
           (values `(else ,e^) cs^^)))])
    (Type : Type (t) -> Type ())
    (Program : Program (p) -> Program ()
      [(program ,e ,t)
       (let-values ([(e^ t^ cs) (Expr e '())])
          `(program ,e^ ,t^ (,[deduplicate cs] ...)))]))

  (define (deduplicate xs)
    (fold-right
      (lambda (x acc)
        (cond
          [(member x acc) acc]
          [else (cons x acc)]))
      '()
      xs))

  (define (typevar-type? x)
    (and (list? x) (= 2 (length x)) (equal? 'typevar (car x))))

  (define (lambda-type? x)
    (and (list? x) (= 3 (length x)) (equal? 'lambda (car x))))

  (define (vector-type? x)
    (and (list? x) (= 2 (length x)) (equal? 'vector (car x))))

  (define (overloaded-constraint? x)
    (and (pair? x) (equal? 'overloaded (car x))))

  (define (occurs x xs)
    (cond
      [(lambda-type? xs) (fold-left (lambda (acc ys) (or acc (occurs x ys))) #f (cdr xs))]
      [else (equal? x xs)]))

  (define (substitute x y xs)
    (cond
      [(lambda-type? xs) (cons 'lambda (map (lambda (ys) (substitute x y ys)) (cdr xs)))]
      [(vector-type? xs) (list 'vector (substitute x y (cadr xs)))]
      [(overloaded-constraint? xs)
       (cons 'overloaded (map (lambda (cs)
                                (map
                                  (lambda (c) (list (substitute x y (car c)) (substitute x y (cadr c))))
                                  cs))
                              (cdr xs)))]
      [(and (typevar-type? xs) (equal? x xs)) y]
      [(list? xs) (map (lambda (ys) (substitute x y ys)) xs)]
      [else xs]))

  (define (unify cs)
    (if (null? cs)
      (lambda (x) x)
      (let* ([c (car cs)] [cs^ (cdr cs)])
        (if (overloaded-constraint? c)
          (letrec ([find-first (lambda (alts)
                                 (cond
                                   [(null? alts) #f]
                                   [(unify (append (car alts) cs^))]
                                   [else (find-first (cdr alts))]))])
            (find-first (cdr c)))
          (let ([s (car c)] [t (cadr c)])
            (cond
              [(equal? s t) (unify cs^)]
              [(and (typevar-type? s) (not (occurs s t)))
               (cond
                 [(unify (substitute s t cs^)) =>
                  (lambda (f) (lambda (x) (f (substitute s t x))))]
                 [else #f])]
              [(and (typevar-type? t) (not (occurs t s)))
               (cond
                 [(unify (substitute t s cs^)) =>
                  (lambda (f) (lambda (x) (f (substitute t s x))))]
                 [else #f])]
              [(and (lambda-type? s) (lambda-type? t))
               (unify (append (list (list (cadr s) (cadr t)) (list (caddr s) (caddr t))) cs^))]
              [(and (vector-type? s) (vector-type? t))
               (unify (append (list (list (cadr s) (cadr t))) cs^))]
              [else #f]))))))

  (define-language
    L8
    (extends L7)
    (terminals
      (- (typevar (tv))))
    (Type (t)
      (- (typevar tv)))
    (Constraints (cs)
      (- (c ...)))
    (Constraint (c)
      (- (t0 t1))
      (- (overloaded cs* ...)))
    (Program (p)
      (- (program e t (c* ...)))
      (+ (program e t))))

   (define-pass unify-and-substitute-types : L7 (ir) -> L8 ()
     (CondArm : CondArm (ca sub) -> CondArm ())
     (Type : Type (t sub) -> Type ()
       [(lambda ,t0 ,t1) `(lambda ,(Type t0 sub) ,(Type t1 sub))]
       [(typevar ,tv) (mk-Type (sub (list 'typevar tv)))]
       [(vector ,t) `(vector ,(Type t sub))])
     (mk-Type : * (t) -> Type ()
       (cond
         [(lambda-type? t) `(lambda ,(mk-Type (cadr t)) ,(mk-Type (caddr t))) ]
         [(vector-type? t) `(vector ,(mk-Type (cadr t)))]
         [else t]))
     (Expr : Expr (e sub) -> Expr ())
     (Program : Program (p) -> Program ()
       [(program ,e ,t (,c* ...))
        (let ([cs (map unparse-L7 c*)])
         (cond
           [(unify cs) => (lambda (sub)
            `(program ,(Expr e sub) ,(Type t sub)))]
           [else (error 'unify-and-substitute-types "unable to unify types" cs)]
          ))]))

  (define-language
    L9
    (extends L8)
    (Expr (e)
      (- (coerce e t))))

  (define-pass coerce-values : L8 (e) -> L9 ()
    (unwrap-Type : Type (t) -> * ()
      [(vector ,t) `(vector ,(unwrap-Type t))]
      [(lambda ,t0 ,t1) `(lambda ,(unwrap-Type t0) ,(unwrap-Type t1))]
      [,bool bool]
      [,int int])
    (mk-Type : * (t) -> Type ()
      (cond
        [(lambda-type? t) `(lambda ,(mk-Type (cadr t)) ,(mk-Type (caddr t))) ]
        [(vector-type? t) `(vector ,(mk-Type (cadr t)))]
        [else t]))
    (find-type : Expr (expr) -> * ()
      [(,s ,t) t]
      [(lambda (,s ,t0) ,e ,t1) t1]
      [(lambda-rec (,s ,t0) ,e ,t1 ,t3) t1]
      [(apply ,e0 ,e1 ,t) t]
      [(primfun ,pf ,t) t]
      [(cond (,ca* ...) ,t) t]
      [(vector ,nv ,t) t]
      [(scalar ,n ,t) t]
      [(coerce ,e ,t) t])
    (Expr : Expr (expr) -> Expr ()
      [(coerce ,e ,t)
       (let ([et (unwrap-Type (find-type e))]
             [ut (unwrap-Type t)])
         (cond
           [(equal? et ut) (Expr e)]
           [(and (equal? et 'bool) (equal? ut 'int))
            `(apply
               (primfun coerce-bool-int (lambda bool int))
               ,[Expr e]
               int)]
           [(and (equal? et '(vector bool)) (equal? ut '(vector int)))
            `(apply
               (apply
                 (primfun map (lambda (lambda bool int) (lambda (vector bool) (vector int))))
                 (primfun coerce-bool-int (lambda bool int))
                 (lambda (vector bool) (vector int)))
               ,[Expr e]
               (vector int))]
           [(and (vector-type? et) (lambda-type? ut) (equal? (cadr ut) 'int)
                 (equal? (cadr et) (caddr ut)))
            (let ([a (mk-Type (cadr et))])
              `(apply
                 (primfun at (lambda (vector ,a) (lambda int ,a)))
                 ,[Expr e]
                 (lambda int ,a)))]
           [(and (vector-type? et) (lambda-type? ut) (equal? (cadr ut) '(vector int))
                 (equal? (cadr et) (cadr (caddr ut))))
            (let ([a (mk-Type (cadr et))])
              `(apply
                 (primfun at-vector (lambda (vector ,a) (lambda (vector int) (vector ,a))))
                 ,[Expr e]
                 (lambda (vector int) (vector ,a))))]
           [else (error 'coerce-values "unsupported coercion" et ut)]))]))


  (define-pass expand-idioms : L9 (e) -> L9 ()
    (unwrap-Type : Type (t) -> * ()
      [(vector ,t) `(vector ,(unwrap-Type t))]
      [(lambda ,t0 ,t1) `(lambda ,(unwrap-Type t0) ,(unwrap-Type t1))]
      [,bool bool]
      [,int int])
    (mk-Type : * (t) -> Type ()
      (cond
        [(lambda-type? t) `(lambda ,(mk-Type (cadr t)) ,(mk-Type (caddr t))) ]
        [(vector-type? t) `(vector ,(mk-Type (cadr t)))]
        [else t]))
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
           [(and (equal? pf 'negation) (equal? '(lambda (vector bool) (vector bool)) t^))
            `(lambda (bs (vector bool))
               (apply
                 (apply
                   (primfun map (lambda (lambda bool bool) (lambda (vector bool) (vector bool))))
                   (primfun negation (lambda bool bool))
                   (lambda (vector bool) (vector bool)))
                 (bs (vector bool))
                 (vector bool))
               (lambda (vector bool) (vector bool)))]
           ; 1 2 3+4 -> {w+4}'1 2 3
           [(and (or (equal? pf 'plus) (equal? pf 'star) (equal? pf 'minus) (equal? pf 'equal)
                     (equal? pf 'less) (equal? pf 'more))
                 (unify (list (list '(lambda (typevar T0) (lambda (vector (typevar T0)) (vector (typevar T1))))
                                    t^)))) => (lambda (sub)
              (let ([pf^ (cond
                           [(equal? pf 'star) 'multiplication]
                           [else pf])]
                    [T0 (mk-Type (sub '(typevar T0)))]
                    [T1 (mk-Type (sub '(typevar T1)))])
                `(lambda (x ,T0)
                   (lambda (xs (vector ,T0))
                     (apply
                       (apply
                         (primfun map (lambda (lambda ,T0 ,T1) (lambda (vector ,T0) (vector ,T1))))
                         (lambda (y ,T0)
                           (apply
                             (apply
                               (primfun ,pf^ (lambda ,T0 (lambda ,T0 ,T1)))
                               (x ,T0)
                               (lambda ,T0 ,T1))
                             (y ,T0)
                             ,T1)
                           (lambda ,T0 ,T1))
                         (lambda (vector ,T0) (vector ,T1)))
                       (xs (vector ,T0))
                       (vector ,T1))
                     (lambda (vector ,T0) (vector ,T1)))
                   (lambda ,T0 (lambda (vector ,T0) (vector ,T1))))))]
           ; 1+2 3 -> 3 4
           [(and (or (equal? pf 'plus) (equal? pf 'star) (equal? pf 'minus) (equal? pf 'equal)
                     (equal? pf 'less) (equal? pf 'more))
                 (unify (list (list '(lambda (vector (typevar T0)) (lambda (typevar T0) (vector (typevar T1))))
                                    t^)))) => (lambda (sub)
            (let ([pf^ (cond
                         [(equal? pf 'star) 'multiplication]
                         [else pf])]
                    [T0 (mk-Type (sub '(typevar T0)))]
                    [T1 (mk-Type (sub '(typevar T1)))])
              `(lambda (xs (vector ,T0))
                 (lambda (x ,T0)
                   (apply
                     (apply
                       (primfun map (lambda (lambda ,T0 ,T1) (lambda (vector ,T0) (vector ,T1))))
                       (lambda (y ,T0)
                         (apply
                           (apply
                             (primfun ,pf^ (lambda ,T0 (lambda ,T0 ,T1)))
                             (y ,T0)
                             (lambda ,T0 ,T1))
                           (x ,T0)
                           ,T1)
                         (lambda ,T0 ,T1))
                       (lambda (vector ,T0) (vector ,T1)))
                     (xs (vector ,T0))
                     (vector ,T1))
                   (lambda ,T0 (vector ,T1)))
                 (lambda (vector ,T0) (lambda ,T0 (vector ,T1))))))]
           ; 1 2+3 4 or 1 2-3 4
           [(and (or (equal? pf 'plus) (equal? pf 'minus) (equal? pf 'star))
                 (equal? '(lambda (vector int) (lambda (vector int) (vector int))) t^))
            (let ([pf^ (cond
                         [(equal? pf 'star) 'multiplication]
                         [else pf])])
              `(lambda (ys (vector int))
                 (lambda (xs (vector int))
                   (apply
                     (apply
                       (apply
                         (primfun zip (lambda (lambda int (lambda int int)) (lambda (vector int) (lambda (vector int) (vector int)))))
                         (primfun ,pf^ (lambda int (lambda int int)))
                         (lambda (vector int) (lambda (vector int) (vector int))))
                       (ys (vector int))
                       (lambda (vector int) (vector int)))
                     (xs (vector int))
                     (vector int))
                   (lambda (vector int) (vector int)))
                 (lambda (vector int) (lambda (vector int) (vector int)))))]
           [(equal? pf 'display)
            (cond
              [(equal? '(lambda int int) t^)
               `(primfun output-scalar (lambda int int))]
              [(equal? '(lambda (vector int) (vector int)) t^)
               `(primfun output-vector (lambda (vector int) (vector int)))]
              [else (error 'expand-idioms "displaying unsupported type" t^)])]
           [(and (equal? pf 'min) (equal? '(lambda bool (lambda bool bool)) t^))
            `(primfun and ,(mk-Type t^))]
           [(and (equal? pf 'max) (equal? '(lambda bool (lambda bool bool)) t^))
            `(primfun or ,(mk-Type t^))]
           [(and (equal? pf 'star) (equal? '(lambda int (lambda int int)) t^))
            `(primfun multiplication ,(mk-Type t^))]
           [(and (equal? pf 'star)
                 (unify (list (list '(lambda (vector (typevar T0)) (typevar T0)) t^)))) => (lambda (sub)
            (let ([T0 (mk-Type (sub '(typevar T0)))])
              `(primfun first (lambda (vector ,T0) ,T0))))]
           ; TODO: use unify to match the polymorphic type
           [(and (equal? pf 'map) (equal? '(lambda (lambda int (lambda int int))
                                             (lambda (vector int)
                                               (lambda int (vector int)))) t^))
            `(lambda (f (lambda int (lambda int int)))
               (lambda (rs (vector int))
                 (lambda (l int)
                   (apply
                     (apply
                       (primfun map (lambda (lambda int int)
                                      (lambda (vector int) (vector int))))
                       (lambda (r int)
                         (apply
                           (apply
                             (f (lambda int (lambda int int)))
                             (r int)
                             (lambda int int))
                           (l int)
                           int)
                         (lambda int int))
                       (lambda (vector int) (vector int)))
                     (rs (vector int))
                     (vector int))
                   (lambda int (vector int)))
                 (lambda (vector int) (lambda int (vector int))))
               (lambda (lambda int (lambda int int))
                 (lambda (vector int) (lambda int (vector int)))))]
           [(and (equal? pf 'shape)
                 (unify (list (list '(lambda (vector (typevar T0)) int) t^)))) => (lambda (sub)
            (let ([T0 (mk-Type (sub '(typevar T0)))])
              `(primfun length (lambda (vector ,T0) int))))]
           [(and (equal? pf 'shape)
                 (unify (list (list '(lambda (vector (typevar T0)) (lambda int (vector (typevar T0)))) t^)))) => (lambda (sub)
            (let ([T0 (mk-Type (sub '(typevar T0)))])
              `(primfun reshape (lambda (vector ,T0) (lambda int (vector ,T0))))))]
           [(and (equal? pf 'shape)
                 (unify (list (list '(lambda (typevar T0) (lambda int (vector (typevar T0)))) t^)))) => (lambda (sub)
            (let ([T0 (mk-Type (sub '(typevar T0)))])
              `(primfun make-vector (lambda ,T0 (lambda int (vector ,T0))))))]
           [(and (equal? pf 'min) (equal? '(lambda (vector int) (vector int)) t^))
            `(primfun where (lambda (vector int) (vector int)))]
           [(and (equal? pf 'negation) (equal? '(lambda int bool) t^))
            `(primfun is-zero (lambda int bool))]
           [else e]))]))

  (define-pass type-check : L9 (e) -> L9 ()
    (unwrap-Type : Type (t) -> * ()
      [(vector ,t) `(vector ,(unwrap-Type t))]
      [(lambda ,t0 ,t1) `(lambda ,(unwrap-Type t0) ,(unwrap-Type t1))]
      [,int int]
      [,bool bool])
    (Expr : Expr (expr ctx) -> Expr (t)
      [(,s ,t)
       (let ([t^ (unwrap-Type t)])
         (cond
           [(assq s ctx) => (lambda (st)
              (cond
                [(equal? t^ (cdr st)) (values e t^)]
                [else (error 'type-check "type error for symbol" (cons s t^) ctx)]))]
           [else (error 'type-check "unbound symbol" s ctx)]))]
      [(primfun ,pf ,t) (values expr (unwrap-Type t))]
      [(scalar ,n ,t) (values expr (unwrap-Type t))]
      [(vector ,nv ,t) (values expr (unwrap-Type t))]
      [(apply ,e0 ,e1 ,t)
       (let-values ([(e0^ t0^) (Expr e0 ctx)]
                    [(e1^ t1^) (Expr e1 ctx)]
                    [(ut) (unwrap-Type t)])
         (cond
           [(and (lambda-type? t0^)
                 (equal? (cadr t0^) t1^)
                 (equal? (caddr t0^) ut))
            (values expr ut)]
           [else (error 'type-check "type error in application"
                        (list (unparse-L9 e0^) t0^) (list (unparse-L9 e1^) t1^) ut)]))]
      [(lambda (,s ,t0) ,e ,t1)
       (let*-values ([(ut0) (unwrap-Type t0)]
                     [(e^ t^) (Expr e (cons (cons s ut0) ctx))]
                     [(ut1) (unwrap-Type t1)])
         (cond
           [(and (lambda-type? ut1)
                (equal? (cadr ut1) ut0)
                (equal? (caddr ut1) t^))
            (values expr ut1)]
           [else (error 'type-check "type error in lambda"
                        (list s ut0) (list (unparse-L9 e^) t^) ut1)]))]
      [(lambda-rec (,s ,t0) ,e ,t1 ,t2)
       (let*-values ([(ut0) (unwrap-Type t0)]
                     [(ut1) (unwrap-Type t1)]
                     [(ut2) (unwrap-Type t2)]
                     [(e^ t^) (Expr e (append (list (cons self ut1) (cons s ut0) ctx)))])
         (cond
           [(and (lambda-type? ut1)
                (equal? (cadr ut1) ut0)
                (equal? (caddr ut1) t^)
                (equal? ut1 ut2))
            (values expr ut1)]
           [else (error 'type-check "type error in lambda-rec"
                        (list s ut0) (list (unparse-L9 e^) t^) ut1)]))]
      [(cond (,ca* ...) ,t)
       (let ([t^ (unwrap-Type t)])
         (for-all (lambda (ca) (CondArm ca t^ ctx)) ca*)
         (values expr t^))])
    (CondArm : CondArm (ca t ctx) -> CondArm ()
      [(,e0 ,e1)
       (let*-values ([(e0^ t0) (Expr e0 ctx)]
                     [(e1^ t1) (Expr e1 ctx)])
         (cond
           [(and (equal? t0 'bool) (equal? t t1)) ca]
           [else
             (if (not (equal? t0 'bool))
               (error 'type-check "type error in cond-arm, condition should have type bool"
                      (unparse-L9 e0^) t0)
               (error 'type-check "type error in cond-arm, body's type does not conform"
                      (unparse-L9 e1^) t1 t))]))]
      [(else ,e)
       (let*-values ([(e^ t^) (Expr e ctx)])
         (cond
           [(equal? t t^) ca]
           [else (error 'type-check "type error in cond-arm, else body's type does not conform"
                        (unparse-L9 e^) t^ t)]))])
    (Program : Program (p) -> Program ()
      [(program ,e ,t)
       (let-values ([(e^ t^) (Expr e '())])
         (let ([ut (unwrap-Type t)])
           (cond
             [(equal? ut t^) `(program ,e^ ,t)]
             [else (error 'type-check "type error in program" (list e ut) (list e^ t^))])))]))

  (define underlying-primfun-map
    '((output-scalar   . (lambda int int))
      (output-vector   . (lambda (vector int) (vector int)))
      (input-scalar    . int)
      (input-vector    . (vector int))
      (coerce-bool-int . (lambda bool int))
      (equal           . (lambda int (lambda int bool)))
      (less            . (lambda int (lambda int bool)))
      (more            . (lambda int (lambda int bool)))
      (max             . (lambda int (lambda int int)))
      (min             . (lambda int (lambda int int)))
      (negation        . (lambda bool bool))
      (is-zero         . (lambda int bool))
      (plus            . (lambda int (lambda int int)))
      (minus           . (lambda int (lambda int int)))
      (multiplication  . (lambda int (lambda int int)))
      (map             . (lambda (lambda (typevar UP1) (typevar UP2))
                           (lambda (vector (typevar UP1)) (vector (typevar UP2)))))
      (zip             . (lambda (lambda (typevar UP1) (lambda (typevar UP2) (typevar UP3)))
                           (lambda (vector (typevar UP1)) (lambda (vector (typevar UP2)) (vector (typevar UP3))))))
      (reduce          . (lambda (lambda (typevar UP1) (lambda (typevar UP1) (typevar UP1)))
                           (lambda (vector (typevar UP1)) (typevar UP1))))
      (iota            . (lambda int (vector int)))
      (where           . (lambda (vector int) (vector int)))
      (first           . (lambda (vector (typevar UP1)) (typevar UP1)))
      (length          . (lambda (vector (typevar UP1)) int))
      (reshape         . (lambda (vector (typevar UP1)) (lambda int (vector (typevar UP1)))))
      (make-vector     . (lambda (typevar UP1) (lambda int (vector (typevar UP1)))))
      (at              . (lambda (vector (typevar UP1)) (lambda int (typevar UP1))))
      (at-vector       . (lambda (vector (typevar UP1)) (lambda (vector int) (vector (typevar UP1)))))))

  (define (underlying-primfun? x) (memq x (map car underlying-primfun-map)))

  (define-language
    L9-with-underlying-primfuns
    (extends L9)
    (terminals
      (- (primfun (pf)))
      (+ (underlying-primfun (upf))))
    (Expr (e)
      (- (primfun pf t))
      (+ (primfun upf t))))

  (define-pass check-underlying-primfuns : L9 (ir) -> L9-with-underlying-primfuns ()
    (unwrap-Type : Type (t) -> * ()
      [(vector ,t) `(vector ,(unwrap-Type t))]
      [(lambda ,t0 ,t1) `(lambda ,(unwrap-Type t0) ,(unwrap-Type t1))]
      [,bool bool]
      [,int int])
    (Expr : Expr (expr) -> Expr ()
      [(primfun ,pf ,t)
       (let ([t^ (unwrap-Type t)])
         (cond
           [(assq pf underlying-primfun-map) => (lambda (x)
              (cond
                [(unify (list (list (cdr x) t^))) => (lambda (sub)
                   `(primfun ,pf ,t))]
                [else (error 'check-underlying-primfuns
                             "incorrect type given to primfun" pf t^)]))]
           [else (error 'check-underlying-primfuns
                        "unsupported underlying primfun" pf)]))]))

  (define-language
    L10
    (extends L9-with-underlying-primfuns)
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
      (- (primfun upf t))
      (+ (primfun upf))
      (- (apply e0 e1 t))
      (+ (apply e0 e1))
      (- (lambda (s t0) e t1))
      (+ (lambda (s) e))
      (- (lambda-rec (s t0) e t1 t2))
      (+ (lambda-rec (s) e))
      (- (cond (ca* ...) t))
      (+ (cond (ca* ...))))
    (Program (p)
      (- (program e t))
      (+ (program e))))

  (define-pass untype : L9-with-underlying-primfuns (e) -> L10 ()
    (CondArm : CondArm (ca) -> CondArm ())
    (Expr : Expr (e) -> Expr ()
      [(,s ,t) s]
      [(scalar ,n ,t) `(scalar ,n)]
      [(vector ,nv ,t) `(vector ,nv)]
      [(primfun ,upf ,t) `(primfun ,upf)]
      [(apply ,e0 ,e1 ,t) `(apply ,[Expr e0] ,[Expr e1])]
      [(lambda (,s ,t0) ,e ,t1) `(lambda (,s) ,[Expr e])]
      [(lambda-rec (,s ,t0) ,e ,t1 ,t2) `(lambda-rec (,s) ,[Expr e])]
      [(cond (,ca* ...) ,t) `(cond (,[map CondArm ca*] ...))])
    (Program : Program (p) -> Program ()
      [(program ,e ,t)
       `(program ,(Expr e))]))


  (define-pass output-scheme : L10 (e) -> * ()
    (CondArm : CondArm (ca) -> * ()
      [(,e0 ,e1) `(,[Expr e0] ,[Expr e1])]
      [(else ,e) `(else ,[Expr e])])
    (Expr : Expr (e) -> * ()
      [,s s]
      [(scalar ,n) n]
      [(vector ,nv) `(quote ,nv)]
      [(primfun ,upf)
       (case upf
         ['minus '(lambda (y) (lambda (x) (- x y)))]
         ['plus '(lambda (y) (lambda (x) (+ x y)))]
         ['multiplication '(lambda (y) (lambda (x) (* x y)))]
         ['map '(lambda (f) (lambda (xs) (map f xs)))]
         ['equal '(lambda (y) (lambda (x) (= x y)))]
         ['less '(lambda (y) (lambda (x) (< x y)))]
         ['more '(lambda (y) (lambda (x) (> x y)))]
         ['and '(lambda (y) (lambda (x) (and x y)))]
         ['or '(lambda (y) (lambda (x) (or x y)))]
         ['min '(lambda (y) (lambda (x) (min x y)))]
         ['max '(lambda (y) (lambda (x) (max x y)))]
         ['negation '(lambda (b) (not b))]
         ['is-zero '(lambda (i) (= 0 i))]
         ['kite '(lambda (a) (lambda (b) b))]
         ['coerce-bool-int '(lambda (b) (if b 1 0))]
         ['iota 'iota]
         ['first 'car]
         ['length 'length]
         ['at '(lambda (xs) (lambda (n) (list-ref xs n)))]
         ['at-vector
          '(lambda (xs)
             (lambda (is)
               (letrec ([go (lambda (js acc)
                              (cond
                                [(null? js) (reverse acc)]
                                [else (go (cdr js)
                                          (cons (list-ref xs (car js)) acc))]))])
                 (go is '()))))]
         ['reduce
          '(lambda (f)
            (lambda (xs)
              (letrec ([go (lambda (acc xs)
                             (cond
                               [(null? xs) acc]
                               [else (go ((f acc) (car xs)) (cdr xs))]))])
                (let ([rs (reverse xs)])
                  (go (car rs) (cdr rs))))))]
         ['zip
          '(lambda (f)
             (lambda (ys)
               (lambda (xs)
                 (letrec ([go (lambda (xs ys)
                                (cond
                                  [(and (null? xs) (null? ys)) '()]
                                  [else (cons ((f (car ys)) (car xs)) (go (cdr xs) (cdr ys)))]))])
                   (go xs ys)))))]
         ['input-scalar
          '(string->number (get-line (current-input-port)))]
         ['input-vector
          '(with-input-from-string (format "(~a)" (get-line (current-input-port))) read)]
         ['output-scalar
          '(lambda (x) (display x) x)]
         ['output-vector
          '(lambda (x)
             (letrec ([print-vector (lambda (xs)
                                    (display (car xs))
                                    (let ([tail (cdr xs)])
                                      (cond
                                        [(null? tail) (void)]
                                        [else (display " ") (print-vector tail)])))])
               (print-vector x))
             x)]
         ['reshape
          '(lambda (xs)
             (lambda (n)
               (letrec [(go (lambda (acc ys i)
                              (cond
                                [(= 0 i) (reverse acc)]
                                [else
                                  (let ([ys^ (if (null? ys) xs ys)])
                                    (go (cons (car ys^) acc) (cdr ys^) (- i 1)))])))]
                 (go '() xs n))))]
         ['make-vector '(lambda (a) (lambda (n) (make-list n a)))]
         ['where
          '(lambda (xs)
             (letrec ([go (lambda (xs i acc)
                            (cond
                              [(null? xs) (reverse acc)]
                              [else
                                (go (cdr xs)
                                    (+ i 1)
                                    (append (make-list (car xs) i) acc))]))])
               (go xs 0 '())))]
         [else (error 'output-scheme "unsupported primitive function" upf)])]
      [(apply ,e0 ,e1) `(,(Expr e0) ,(Expr e1))]
      [(lambda (,s) ,e) `(lambda (,s) ,[Expr e])]
      [(lambda-rec (,s) ,e) `(letrec ((,self (lambda (,s) ,[Expr e]))) ,self)]
      [(cond (,ca* ...)) `(cond . ,[map CondArm ca*])])
    (Program : Program (p) -> * ()
      [(program ,e) (Expr e)]))


  (define-pass output-malfunction : L10 (e) -> * ()
    (definitions
      (define (mlf-symbol s) (string->symbol (format "$~s" s)))
      (define mlf-self (mlf-symbol self))
      (define mlf-unit '(block (tag 0)))
      (define (mlf-error reason code)
        `(seq (apply (global $Stdlib $prerr_endline) ,reason)
              (apply (global $Stdlib $exit) ,code)))
      (define (make-malfunction-vector l)
        (letrec ([go (lambda (i k)
                       (if (null? k)
                         '()
                         (cons `(store $v ,i ,(car k)) (go (+ i 1) (cdr k)))))])
          (let ([body (append '(seq) (go 0 l) '($v))])
            `(let ($v (makevec ,(length l) 0)) ,body)))))
    (CondArm : CondArm (ca) -> * ()
      [(,e0 ,e1) `(,[Expr e0] ,[Expr e1])]
      [(else ,e) `(else ,[Expr e])])
    (Expr : Expr (e) -> * ()
      [,s (mlf-symbol s)]
      [(scalar ,n) n]
      [(vector ,nv) (make-malfunction-vector nv)]
      [(primfun ,upf)
       (case upf
         ['minus `(lambda ($y $x) (- $x $y))]
         ['plus `(lambda ($x $y) (+ $x $y))]
         ['multiplication `(lambda ($y $x) (* $x $y))]
         ['equal `(lambda ($y $x) (== $x $y))]
         ['less `(lambda ($y $x) (< $x $y))]
         ['more `(lambda ($y $x) (> $x $y))]
         ['and '$min]
         ['or '$max]
         ['min '$min]
         ['max '$max]
         ['negation '$not]
         ['is-zero '$not]
         ['map `(lambda ($f $xs) (apply $map $f $xs))]
         ['reduce `(lambda ($f $xs) (apply $reduce $f $xs))]
         ['zip `(lambda ($f $ys $xs) (apply $zip $f $ys $xs))]
         ['input-vector `(apply $read_vector ,mlf-unit)]
         ['input-scalar `(apply $read_scalar ,mlf-unit)]
         ['output-scalar '$write_scalar]
         ['output-vector '$write_vector]
         ['kite '$kite]
         ['iota '$iota]
         ['first '$first]
         ['length '$length]
         ['reshape '$reshape]
         ['make-vector '$make_vector]
         ['coerce-bool-int '$identity]
         ['at '$at]
         ['at-vector '$at_vector]
         ['where '$where]
         [else (error 'output-malfunction "unsupported primitive function" upf)])]
      [(apply ,e0 ,e1) `(apply ,(Expr e0) ,(Expr e1))]
      [(lambda (,s) ,e) `(lambda (,[mlf-symbol s]) ,[Expr e])]
      [(lambda-rec (,s) ,e) `(let (rec (,mlf-self (lambda (,[mlf-symbol s]) ,[Expr e]))) ,mlf-self)]
      [(cond (,ca* ...))
       (letrec ([go (lambda (cas)
                      (if (null? cas)
                        `(apply $match_error ,mlf-unit)
                        (let ([ca (car cas)] [tail (cdr cas)])
                          (cond
                            [(equal? (car ca) 'else) (cadr ca)]
                            [else `(switch ,(car ca)
                                           (1 ,(cadr ca))
                                           (_ ,(go tail)))]))))])
         (go (map CondArm ca*)))]
      [else (error 'output-malfunction "unsupported expr" e)])
    (Program : Program (p) -> * ()
      [(program ,e)
       `(module
          ($match_error (lambda ($u) ,[mlf-error "match error" 3]))
          ($length_error (lambda ($u) ,[mlf-error "length error" 4]))
          ($map (lambda ($f $v) (apply (global $Array $map) $f $v)))
          ($zip (lambda ($f $v $w)
                  (let
                    ($n (length $v))
                    ($m (length $w))
                    (if (== $n $m)
                      (apply (global $Array $map2) $f $v $w)
                      (apply $length_error ,mlf-unit)))))
          ($reduce (lambda ($f $v)
                     (let
                       ($n (length $v))
                       (rec ($go (lambda ($acc $i)
                                   (switch $i
                                           (-1 $acc)
                                           (_ (apply $go (apply $f $acc (load $v $i)) (- $i 1)))))))
                       (apply $go (load $v (- $n 1)) (- $n 2)))))
          ($read_scalar (lambda ($x) (apply (global $Stdlib $read_int) ,mlf-unit)))
          ($read_vector (lambda ($x)
                          (apply (global $Array $map)
                                 (lambda ($s) (apply (global $Stdlib $int_of_string) $s))
                                 (apply (global $Array $of_list)
                                        (apply (global $Str $split) (apply (global $Str $regexp) " +")
                                               (apply (global $Stdlib $read_line) ,mlf-unit))))))
          ($write_scalar (lambda ($x) (seq (apply (global $Stdlib $print_int) $x) $x)))
          ($write_vector (lambda ($l)
                           (let ($n (length $l))
                             (apply (global $Array $iteri)
                                    (lambda ($i $x)
                                      (seq
                                        (apply $write_scalar $x)
                                        (if (< (+ $i 1) $n)
                                          (apply (global $Stdlib $print_char) 32)
                                          ,mlf-unit)))
                                    $l))))
          ($kite (lambda ($a $b) $b))
          ($identity (lambda ($a) $a))
          ($first (lambda ($l) (load $l 0)))
          ($length (lambda ($l) (length $l)))
          ($iota (lambda ($n) (apply (global $Array $init) $n $identity)))
          ($max (lambda ($y $x) (switch (< $x $y) (0 $x) (_ $y))))
          ($min (lambda ($y $x) (switch (< $x $y) (0 $y) (_ $x))))
          ($not (lambda ($x) (switch $x (0 1) (_ 0))))
          ($reshape (lambda ($xs $n)
                      (let ($l (length $xs))
                        (apply (global $Array $init) $n (lambda ($i) (load $xs (% $i $l)))))))
          ($make_vector (lambda ($a $n) (makevec $n $a)))
          ($at (lambda ($xs $n) (load $xs $n)))
          ($at_vector (lambda ($xs $is)
                        (apply (global $Array $init)
                               (length $is)
                               (lambda ($i) (load $xs (load $is $i))))))
          ($where (lambda ($xs)
                    (let
                      ($n (length $xs))
                      ($l (apply (global $Array $fold_right) (lambda ($a $b) (+ $a $b)) $xs 0))
                      ($ys (makevec $l 0))
                      (rec
                        ($go (lambda ($o $j $i)
                               (switch (== $j (load $xs $i))
                                       (0 (seq
                                            (store $ys (+ $o $j) $i)
                                            (apply $go $o (+ $j 1) $i)))
                                       (_ (switch (== $n (+ $i 1))
                                            (1 $ys)
                                            (_ (apply $go (+ $o $j) 0 (+ $i 1)))))))))
                      (apply $go 0 0 0))))
          (_ ,[Expr e])
          (_ (apply (global $Stdlib $print_newline) ,mlf-unit))
          (export))
       ]))

  (define (compiler-frontend)
    (untype
      (check-underlying-primfuns
        (type-check
          (expand-idioms
            (coerce-values
              (unify-and-substitute-types
                (derive-type-constraints
                  (add-coercion-points
                    (type-lambda-abstractions
                      (introduce-fresh-typevars
                        (type-scalars-and-vectors
                          (remove-lets
                            (pick-spine-values
                              (translate-L3-spine-intermediate-into-L3-spine
                                (introduce-let-spine
                                  (embedd-L3-into-L3-spine-intermediate
                                    (introduce-lambda-abstractions
                                      (translate-to-primfuns
                                        (differentiate-scalars
                                          (ast-to-Lsrc
                                            (parse-silly-k))))))))))))))))))))))


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
            (assert (= 0 (system (format "malfunction cmx -package str ~s" fn))))
            (assert (= 0 (system (format "exec ocamlopt.opt -o ~s str.cmxa ~s.cmx -I +str" out pn)))))
          '(replace)))))

  )
