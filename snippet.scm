(load "silly-k.scm")

(define (id x) x)

(define passes
  '((parse-silly-k                 . id)
    (ast-to-Lsrc                   . unparse-Lsrc)
    (differentiate-scalars         . unparse-L1)
    (translate-to-primfuns         . unparse-L2)
    (introduce-lambda-abstractions . unparse-L3)
    (introduce-fresh-typevars      . unparse-L4)))


(define (compiler s)
  (display "> ")
  (display s)
  (newline)

  (fold-left (lambda (acc p)
               (let* ([pass (car p)]
                      [unparser (cdr p)]
                      [expr (cond
                              [(null? acc) (list pass)]
                              [else (list pass acc)])])
                 (display ((eval unparser) (with-input-from-string s (lambda () (eval expr)))))
                 (newline)
                 expr))
             '()
             passes)

  (newline))

(compiler "1+1 2 3")
(compiler "1+2+3+4")
(compiler "foo")
(compiler "foo bar")
(compiler "{x w w}")
(compiler "1 2 3")
(compiler "{w}")
(compiler "{a}")
(compiler "{1+w}2")
(compiler "1{a+w}2")
(compiler "{1+w}2 3 4")
(compiler "(1:)+0:")
(compiler "{1+w}'1 2 3")
