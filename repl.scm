(import (silly-k) (rnrs))

(let ([args (cdr (command-line))])
  (let-values ([(verbose) (if (and (not (null? args)) (equal? (car args) "-v"))
                            (values #t)
                            (values #f))])
    (letrec ([go (lambda ()
                   (display "   ]")
                   (let ([e (get-line (console-input-port))])
                     (cond
                       [(equal? e #!eof) (newline)]
                       [else
                         (let* ([e^ (string-append "]" e)]
                                [scm (with-exception-handler
                                      (lambda (ex) (display-condition ex) (newline) (go))
                                      (lambda () (with-input-from-string e^ compile-to-scheme)))])
                           (when verbose (pretty-print scm))
                           (eval scm)
                           (newline)
                           (go))])))])
          (go))))
