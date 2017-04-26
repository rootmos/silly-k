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
            [(char=? c #\+)    (make-lexical-token 'PLUS location #f)]
            [(char=? c #\:)    (make-lexical-token 'COLON location #f)]
            [(char=? c #\-)    (make-lexical-token 'MINUS location #f)]
            [(char=? c #\()    (make-lexical-token 'LPAREN location #f)]
            [(char=? c #\))    (make-lexical-token 'RPAREN location #f)]
            [(char=? c #\/)    (make-lexical-token 'SLASH location #f)]
            [(char=? c #\')    (make-lexical-token 'QUOTE location #f)]
            [(char-numeric? c) (make-lexical-token 'NUM location (read-number `(,c)))]
            [else (error 'lex "Unrecognized character" c)])))))

(define parse-silly-k
  (lambda ()
    (let ([parser
            (lalr-parser
              (expect: 0)
              (PLUS NUM MINUS LPAREN RPAREN SLASH QUOTE COLON)
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

; (with-input-from-string "+//1 3" parse-silly-k)
; (with-input-from-string "2+//1 3" parse-silly-k)
; (with-input-from-string "1 2+3" parse-silly-k)
; (with-input-from-string "+1 2" parse-silly-k)
; (with-input-from-string "+/9 2" parse-silly-k)
; (with-input-from-string "-'9 2" parse-silly-k)
; (with-input-from-string "2+0:" parse-silly-k)
; (with-input-from-string "1:1 2" parse-silly-k)
; (with-input-from-string "0:" parse-silly-k)

(define (parse-silly-k-string s)
  (with-input-from-string s parse-silly-k))

