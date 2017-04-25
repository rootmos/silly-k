(import (nanopass) (rnrs))

(load "tools.scm")
(load "parser.scm")

(define num-vector?
  (lambda (l)
    (and (list? l)
         (fold-right (lambda (x y) (and x y)) #t (map number? l)))))

(define verb?
  (lambda (x)
    (not (not (memq x '(plus minus))))))

(define adverb?
  (lambda (x)
    (not (not (memq x '(each over))))))

(define-language
  Lsrc
  (terminals
    (verb (v))
    (adverb (a))
    (num-vector (nv)))
  (entry Expr)
  (Fun (f)
       v
       (adverb a f))
  (Expr (e)
        nv
        (apply f e)
        (apply f e0 e1)))
(define-pass ast-to-Lsrc : * (e) -> Lsrc ()
  (Fun : * (f) -> Fun ()
    (cond
      [(verb? f) f]
      [(and (list f) (eq? (car f) 'adverb))
       (let ([v (cadr f)]
             [f^ (Fun (caddr f))])
         `(adverb ,v ,f^))]))
  (Expr : * (e) -> Expr ()
    (cond
      [(num-vector? e) e]
      [(eq? (car e) 'apply)
       (let ([v (Fun (cadr e))]
             [r (cadddr e)])
         (if (caddr e)
           `(apply ,v ,(Expr (caddr e)) ,r)
           `(apply ,v ,r)))]
      [else e]
      ))
  (Expr e))

(define-language
  L1
  (extends Lsrc)
  (terminals
    (+ (number (n))))
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
       (- (adverb a f)))
  (Expr (e)
        (- (apply f e))
        (+ (apply pf e))
        (- (apply f e0 e1))
        (+ (apply pf e0 e1))))

(define-pass choose-primitive-functions : L1 (e) -> L2 (k)
  (Expr : Expr (e) -> Expr (#f)
    [(scalar ,n) (values `(scalar ,n) 'scalar)]
    [(vector ,nv) (values `(vector ,nv) 'vector)]
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
    [else (error 'choose-primitive-functions "unsupported verb")])
  (Expr e))

(define-language L3
  (extends L2)
  (entry Program)
  (Program (p)
           (+ (program e))))

(define-pass wrap : L2 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ())
  `(program ,(Expr e)))

(define (malfunction-print-integer i)
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
                ,(malfunction-print-integer '$x)
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
    [(eq? kind 'scalar) (malfunction-print-integer x)]
    [(eq? kind 'vector) (malfunction-print-vector x)]
    [else (error 'malfunction-print "unknown kind" kind)]))

(define-pass output-malfunction : L3 (e k) -> * ()
  (Expr : Expr (e) -> * ()
    [(scalar ,n) n]
    [(vector ,nv) (make-malfunction-vector nv)]
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
    [(apply ,pf ,e)
     (cond
       [(eq? pf 'reduce-addition)
        (let ([a (Expr e)])
          `(apply $foldr (lambda ($x $y) (+ $x $y)) 0 ,a))]
       [else (error 'output-malfunction "unsupported monadic primitive function" pf)])]
    [else (error 'output-malfunction "unsupported expr")])
  (Program : Program (p) -> *()
    [(program ,e)
       `(module
          ($map ,malfunction-map-lambda)
          ($zip ,malfunction-zip-lambda)
          ($foldr ,malfunction-foldr-lambda)
          ($x ,[Expr e])
          (_ ,(malfunction-print k '$x))
          (_ ,malfunction-print-newline)
          (export))
     ]))


;(compile-and-run "1")
;(compile-and-run "1 2 3")
;(compile-and-run "1+2 3 4")
;(compile-and-run "1 2+3 4 5")
;(compile-and-run "1 2+3 4")
;(compile-and-run "+/1 2 3")

(define (compiler s)
  (let-values
    ([(e k)
      (choose-primitive-functions
        (differentiate-scalars
          (ast-to-Lsrc
            (parse-silly-k-string s))))])
    (output-malfunction (wrap e) k)))

(define compile-malfunction
  (lambda (out mlf)
    (with-temporary-file "silly.XXXXXX" (fn p)
      (write mlf p)
      (flush-output-port p)
      (assert (= 0 (system (format "opam config exec -- malfunction compile -o ~s ~s" out fn))))
      )))

(define (compile e)
  (compile-malfunction "a.out" (compiler e)))

(define (compile-and-run e)
  (compile e)
  (assert (= 0 (system "./a.out")))
  (void))
