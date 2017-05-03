(load "silly-k.scm")

(define (compiler s)
  (display
    (with-input-from-string s
      (lambda ()
        (parse-silly-k)
        )))
  (newline)

  (display
    (with-input-from-string s
      (lambda ()
        (unparse-Lsrc (ast-to-Lsrc (parse-silly-k)))
        )))
  (newline)

  (newline))

(compiler "1+1 2 3")
(compiler "1+2+3+4")
(compiler "foo")
(compiler "foo bar")
(compiler "1 2 3")
(compiler "{w}")
(compiler "{1+w}'1 2 3")
