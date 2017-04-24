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
  (Expr (e)
        nv
        (verb v e)
        (verb v e0 e1)
        (adverb a v e)
        (adverb a v e0 e1)))
(define-pass ast-to-Lsrc : * (e) -> Lsrc ()
  (Expr : * (e) -> Expr ()
    (cond
      [(num-vector? e) e]
      [(verb? (car e))
       (let ([v (car e)]
             [r (caddr e)])
         (if (cadr e)
           `(verb ,v ,(Expr (cadr e)) ,r)
           `(verb ,v ,r)))]
      [(adverb? (car e))
       (let ([a (car e)]
             [v (cadr e)]
             [r (Expr (cadddr e))])
         (if (caddr e)
           `(adverb ,a ,v ,(Expr (caddr e)) ,r)
           `(adverb ,a ,v ,r)))]
      [else e]
      ))
  (Expr e))
(ast-to-Lsrc (parse-silly-k-string "1 2"))
(ast-to-Lsrc (parse-silly-k-string "+1 2"))
(ast-to-Lsrc (parse-silly-k-string "1+2 3"))
(ast-to-Lsrc (parse-silly-k-string "1 2+3 4"))
(ast-to-Lsrc (parse-silly-k-string "+/1 2"))
(ast-to-Lsrc (parse-silly-k-string "7+/1 2"))

(define-pass analyze-lengths : Lsrc (e) -> Lsrc (l)
  (Expr : Expr (e) -> Expr (#f)
    [,nv (values nv (length nv))]
    [(verb ,v ,e)
     (let-values ([(e^ l) (Expr e)])
       (values `(verb ,v ,e^) l))])
  (Expr e))
(analyze-lengths (ast-to-Lsrc (parse-silly-k-string "1 2")))
(analyze-lengths (ast-to-Lsrc (parse-silly-k-string "+1 2")))
(analyze-lengths (ast-to-Lsrc (parse-silly-k-string "1+2 3")))
(analyze-lengths (ast-to-Lsrc (parse-silly-k-string "1 2+3 4")))
(analyze-lengths (ast-to-Lsrc (parse-silly-k-string "+/1 2")))
(analyze-lengths (ast-to-Lsrc (parse-silly-k-string "7+/1 2")))
