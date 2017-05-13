(import (silly-k) (rnrs))

(let ([args (cdr (command-line))])
  (let-values ([(verbose) (if (and (not (null? args)) (equal? (car args) "-v"))
                            (values #t)
                            (values #f))])
    (letrec ([go (lambda ()
                   (display "   ")
                   (let ([e (get-line (console-input-port))])
                     (cond
                       [(equal? e #!eof) (newline)]
                       [else
                         (let ([scm (with-exception-handler
                                      (lambda (e) (display-condition e) (newline) (go))
                                      (lambda () (with-input-from-string e compile-to-scheme)))])
                           (when verbose (pretty-print scm))
                           (display (eval scm))
                           (newline)
                           (go))])))])
          (go))))
