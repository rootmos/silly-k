(import (silly-k) (rnrs))

(let ([args (cdr (command-line))])
  (unless (null? args)
    (let-values
        ([(output input)
          (if (equal? (car args) "-o")
          (values (cadr args) (caddr args))
          (values "a.out" (car args)))])
        (let ([go (lambda () (compile-silly-k output))])
          (if (not (eq? "-" input))
            (with-input-from-file input go)
            (go))))))
