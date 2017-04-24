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
    (number (n)))
  (Expr (e)
        (n* ...)
        (verb v e* ...)
        (verb-with-left v e e* ...)
        (adverb a v e* ...)
        (adverb-with-left a v e e* ...)
        ))
(define-pass ast-to-Lsrc : * (e) -> Lsrc ()
  (Expr : * (e) -> Expr ()
    (cond
      [(num-vector? e) `(,e ...)]
      [(verb? (car e))
       (let ([v (car e)]
             [es (map Expr (cddr e))])
         (if (cadr e)
           `(verb-with-left ,v ,[Expr (cadr e)] ,es ...)
           `(verb ,v ,es ...)))]
      [(adverb? (car e))
       (let ([a (car e)]
             [v (cadr e)]
             [es (map Expr (cdddr e))])
         (if (caddr e)
           `(adverb-with-left ,a ,v ,[Expr (caddr e)] ,es ...)
           `(adverb ,a ,v ,es ...)))]
      [else e]
      ))
  (Expr e))
(ast-to-Lsrc (parse-silly-k-string "1 2"))
(ast-to-Lsrc (parse-silly-k-string "+1 2"))
(ast-to-Lsrc (parse-silly-k-string "1 2+3 4"))
(ast-to-Lsrc (parse-silly-k-string "+/1 2"))
(ast-to-Lsrc (parse-silly-k-string "7+/1 2"))

