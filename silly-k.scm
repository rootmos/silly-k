(import (nanopass) (rnrs))
(include "tools.scm")

(load "lalr-scm/lalr.scm")

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
                     [else (string->number (list->string (reverse l)))])))])
      (skip-spaces)
      (let ([location (make-source-location
                        (port-name (current-input-port))
                        (port-position (current-input-port))
                        #f
                        -1
                        (port-length (current-input-port)))]
            [c (read-char)])
        (cond
          [(eof-object? c) '*eoi*]
            [(char=? c #\+)       (make-lexical-token 'PLUS location #f)]
            [(char=? c #\newline) (make-lexical-token 'NEWLINE location #f)]
            [(char=? c #\:)       (make-lexical-token 'COLON location #f)]
            [(char=? c #\-)       (make-lexical-token 'MINUS location #f)]
            [(char=? c #\()       (make-lexical-token 'LPAREN location #f)]
            [(char=? c #\))       (make-lexical-token 'RPAREN location #f)]
            [(char=? c #\/)       (make-lexical-token 'SLASH location #f)]
            [(char=? c #\')       (make-lexical-token 'QUOTE location #f)]
            [(char-numeric? c)    (make-lexical-token 'NUM location (read-number `(,c)))]
            [else (error 'lex "Unrecognized character" c)])))))

(define parse-silly-k
  (lambda ()
    (let ([parser
            (lalr-parser
              (expect: 0)
              (PLUS (left: NUM) MINUS LPAREN RPAREN SLASH QUOTE COLON NEWLINE)
              (statement (expr) : $1
                         (expr NEWLINE) : $1)
              (expr (num) : $1
                    (verb) : `(apply ,$1 #f #f)
                    (verb expr) : `(apply ,$1 #f ,$2)
                    (expr verb expr) : `(apply ,$2 ,$1 ,$3))
              (exprs (exprs expr) : (append $1 (list $2))
                     (expr)       : (list $1)
                     ()           : '())
              (verb (PLUS) : 'plus
                    (MINUS) : 'minus
                    (NUM COLON) : `(system ,$1)
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
    (adverb (a))
    (number (n))
    (num-vector (nv)))
  (entry Expr)
  (Fun (f)
       v
       (system n)
       (adverb a f))
  (Expr (e)
        nv
        (apply f)
        (apply f e)
        (apply f e0 e1)))

(define-pass ast-to-Lsrc : * (e) -> Lsrc ()
  (Fun : * (f) -> Fun ()
    (cond
      [(verb? f) f]
      [(and (list f) (eq? (car f) 'system))
       `(system ,(cadr f))]
      [(and (list f) (eq? (car f) 'adverb))
       (let ([v (cadr f)]
             [f^ (Fun (caddr f))])
         `(adverb ,v ,f^))]))
  (Expr : * (e) -> Expr ()
    (cond
      [(num-vector? e) e]
      [(eq? (car e) 'apply)
       (let ([v (Fun (cadr e))]
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

(define functions
  '(((plus vector vector)    . (pointwise-addition vector))
    ((plus scalar vector)    . (distribute-addition vector))
    ((plus scalar scalar)    . (scalar-addition scalar))
    (((over plus) #f vector) . (reduce-addition scalar))
    (((system 0) #f #f)      . (system-read-vector vector))
    (((system 1) #f #f)      . (system-read-scalar scalar))
    ))

(define primfun?
  (lambda (x)
    (not (not (memq x (map cadr functions))))))

(define-language
  L2
  (extends L1)
  (terminals
    (+ (primfun (pf))))
  (Fun (f)
       (- v)
       (- (system n))
       (- (adverb a f)))
  (Expr (e)
        (- (apply f))
        (+ (apply pf))
        (- (apply f e))
        (+ (apply pf e))
        (- (apply f e0 e1))
        (+ (apply pf e0 e1))))


(define-pass choose-primitive-functions : L1 (e) -> L2 (k)
  (Expr : Expr (e) -> Expr (#f)
    [(scalar ,n) (values `(scalar ,n) 'scalar)]
    [(vector ,nv) (values `(vector ,nv) 'vector)]
    [(apply (system ,n))
     (let* ([fk (list (list 'system n) #f #f)]
            [mf (assoc fk functions)])
       (if mf
         (values `(apply ,[cadr mf]) (caddr mf))
         (error 'choose-primitive-functions "unsupported combination" fk)))]
    [(apply ,v ,e)
     (let-values ([(e^ l) (Expr e)])
       (let* ([fk (list v #f l)]
              [mf (assoc fk functions)])
         (if mf
           (values `(apply ,[cadr mf] ,e^) (caddr mf))
           (error 'choose-primitive-functions "unsupported combination" fk))))]
    [(apply (adverb ,a ,f) ,e)
     (let-values ([(e^ l) (Expr e)])
       (let* ([fk (list (list a f) #f l)]
              [mf (assoc fk functions)])
         (if mf
           (values `(apply ,[cadr mf] ,e^) (caddr mf))
           (error 'choose-primitive-functions "unsupported combination" fk))))]
    [(apply ,v ,e0 ,e1)
     (let-values ([(e0^ l0) (Expr e0)]
                  [(e1^ l1) (Expr e1)])
       (let* ([fk (list v l0 l1)]
              [mf (assoc fk functions)])
         (if mf
           (values `(apply ,[cadr mf] ,e0^ ,e1^) (caddr mf))
           (error 'choose-primitive-functions "unsupported combination" fk))))]
    [else (error 'choose-primitive-functions "unsupported verb" e)])
  (Expr e))

(define-language L3
  (extends L2)
  (entry Program)
  (Program (p)
           (+ (program e))))

(define-pass wrap : L2 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ())
  `(program ,(Expr e)))

(define (malfunction-print-scalar i)
  `(apply (global $Pervasives $print_int) ,i))

(define malfunction-unit
  '(block (tag 0)))

(define malfunction-print-newline
  `(apply (global $Pervasives $print_newline) ,malfunction-unit))

(define malfunction-print-space
  `(apply (global $Pervasives $print_char) 32))

(define (malfunction-error reason code)
  `(seq (apply (global $Pervasives $prerr_endline) ,reason)
        (apply (global $Pervasives $exit) ,code)))

(define malfunction-length-error
  (malfunction-error "length error" 3))

(define (malfunction-print-vector l)
  `(let ($n (length ,l))
     (apply (global $Array $iteri)
            (lambda ($i $x)
              (seq
                ,(malfunction-print-scalar '$x)
                (if (< (+ $i 1) $n)
                  ,malfunction-print-space
                  ,malfunction-unit)))
            ,l)))

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

(define (make-malfunction-vector l)
  (letrec ([go (lambda (i k)
                 (if (null? k)
                   '()
                   (cons `(store $v ,i ,(car k)) (go (+ i 1) (cdr k)))))])
    (let ([body (append '(seq) (go 0 l) '($v))])
      `(let ($v (makevec ,(length l) 0)) ,body))))

(define (malfunction-print kind x)
  (cond
    [(eq? kind 'scalar) (malfunction-print-scalar x)]
    [(eq? kind 'vector) (malfunction-print-vector x)]
    [else (error 'malfunction-print "unknown kind" kind)]))

(define malfunction-read-scalar-lambda
  `(lambda ($x)
     (apply (global $Pervasives $read_int) ,malfunction-unit)))

(define malfunction-read-vector-lambda
  `(lambda ($x)
     (apply (global $Array $map)
            (lambda ($s) (apply (global $Pervasives $int_of_string) $s))
            (apply (global $Array $of_list)
                   (apply (global $Str $split) (apply (global $Str $regexp) " +")
                          (apply (global $Pervasives $read_line) ,malfunction-unit))))))

(define-pass output-malfunction : L3 (e k) -> * ()
  (Expr : Expr (e) -> * ()
    [(scalar ,n) n]
    [(vector ,nv) (make-malfunction-vector nv)]
    [(apply ,pf)
     (cond
       [(eq? pf 'system-read-vector)
        `(apply $read_vector ,malfunction-unit)]
       [(eq? pf 'system-read-scalar)
        `(apply $read_scalar ,malfunction-unit)]
       [else (error 'output-malfunction "unsupported nullary primitive function" pf)])]
    [(apply ,pf ,e)
     (cond
       [(eq? pf 'reduce-addition)
        (let ([a (Expr e)])
          `(apply $foldr (lambda ($x $y) (+ $x $y)) 0 ,a))]
       [else (error 'output-malfunction "unsupported monadic primitive function" pf)])]
    [(apply ,pf ,e0 ,e1)
     (cond
       [(eq? pf 'scalar-addition)
        (let ([a (Expr e0)]
              [b (Expr e1)])
          `(+ ,a ,b))]
       [(eq? pf 'distribute-addition)
        (let ([a (Expr e0)]
              [b (Expr e1)])
          `(let ($a ,a)
             (apply $map (lambda ($x) (+ $a $x)) ,b)))]
       [(eq? pf 'pointwise-addition)
        (let ([a (Expr e0)]
              [b (Expr e1)])
          `(apply $zip (lambda ($x $y) (+ $x $y)) ,a ,b))]
       [else (error 'output-malfunction "unsupported dyadic primitive function" pf)])]
    [else (error 'output-malfunction "unsupported expr")])
  (Program : Program (p) -> *()
    [(program ,e)
       `(module
          ($map ,malfunction-map-lambda)
          ($zip ,malfunction-zip-lambda)
          ($foldr ,malfunction-foldr-lambda)
          ($read_scalar ,malfunction-read-scalar-lambda)
          ($read_vector ,malfunction-read-vector-lambda)
          ($x ,[Expr e])
          (_ ,(malfunction-print k '$x))
          (_ ,malfunction-print-newline)
          (export))
     ]))

(define (compiler)
  (let-values
    ([(e k)
      (choose-primitive-functions
        (differentiate-scalars
          (ast-to-Lsrc
            (parse-silly-k))))])
    (output-malfunction (wrap e) k)))

(define compile-malfunction
  (lambda (out mlf)
    (with-temporary-file "silly_XXXXXX" (fn p)
      (write mlf p)
      (flush-output-port p)
      (assert (= 0 (system (format "opam config exec -- malfunction cmx ~s" fn))))
      (assert (= 0 (system (format "opam config exec -- ocamlfind ocamlopt -o ~s str.cmxa ~s.cmx" out fn))))
      )))

(define compile
  (lambda (o)
    (compile-malfunction o (compiler))))

(define (compile-and-run)
  (compile "a.out")
  (assert (= 0 (system "./a.out")))
  (void))

(let ([args (cdr (command-line))])
  (unless (null? args)
    (let-values
      ([(output input)
        (if (equal? (car args) "-o")
          (values (cadr args) (caddr args))
          (values "a.out" (car args)))])
      (let ([go (lambda () (compile output))])
        (if (not (eq? "-" input))
          (with-input-from-file input go)
          (go))))))
