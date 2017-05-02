(library
  (silly-k tools)
  (export with-temporary-file)
  (import (chezscheme))

  (define (string->foreign str addr)
    (let ([n (string-length str)]
          [bv (string->utf8 str)])
      (letrec ([go (lambda (i)
                     (cond
                       [(= i n) (foreign-set! 'unsigned-8 addr i 0)]
                       [else
                         (foreign-set! 'unsigned-8 addr i (bytevector-u8-ref bv i))
                         (go (+ i 1))]))])
        (go 0))))

  (define (foreign->string n addr)
    (let ([bv (make-bytevector n)])
      (letrec ([go (lambda (i)
                     (let ((b (foreign-ref 'unsigned-8 addr i)))
                       (bytevector-u8-set! bv i b)
                       (cond
                         [(= b 0) i]
                         [(= n (+ i 1)) (+ i 1)]
                         [else (go (+ i 1))])))])
        (bytevector-truncate! bv (go 0))
        (utf8->string bv))))

  (define mkstemp
    (begin
      (if (not (foreign-entry? "mkstemp"))
        (load-shared-object "libc.so.6"))
      (foreign-procedure "mkstemp" (uptr) int)))

  (define make-temporary-file
    (lambda (pattern)
      (let* ([n (+ 1 (string-length pattern))]
             [m (foreign-alloc n)])
        (string->foreign pattern m)
        (let ([fd (mkstemp m)])
          (let ([fn (foreign->string n m)])
            (foreign-free m)
            (cons fn fd))))))

  (define-syntax with-temporary-file
    (syntax-rules ()
                  ((_ pattern (filename port) body ...)
                   (let* ([fnfd (make-temporary-file pattern)]
                          [filename (car fnfd)]
                          [port (open-fd-input/output-port (cdr fnfd) 'block (make-transcoder (utf-8-codec)))])
                     (let ([x (begin body ...)]) (close-port port) (delete-file filename) x)))))
)
