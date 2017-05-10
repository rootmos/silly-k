(import (chezscheme) (silly-k))

(define-record test-case (name code input expected))

(define test-cases
  (list
    (make-test-case "number01"   "]7"             #f         "7")
    (make-test-case "number02"   "]1 2 3"         #f         "1 2 3")
    (make-test-case "addition01" "]1+2"           #f         "3")
    (make-test-case "addition02" "]1+2 3"         #f         "3 4")
    (make-test-case "addition03" "]1 2+3 4"       #f         "4 6")
    (make-test-case "addition04" "]1 2+3"         #f         "4 5")
    (make-test-case "minus01"    "]2-3"           #f         "-1")
    (make-test-case "minus02"    "]1-(-2)"        #f         "3")
    (make-test-case "minus03"    "]1 2-3 4"       #f         "-2 -2")
    (make-test-case "over01"     "]({w+1}'1 2 3)" #f         "2 3 4")
    (make-test-case "over02"     "]({1-w}'3 4 5)" #f         "-2 -3 -4")
    (make-test-case "negation01" "](-7)"          #f         "-7")
    (make-test-case "negation02" "](-(-2))"       #f         "2")
    (make-test-case "reduce01"   "](+/1 2 3)"     #f         "6")
    ;(make-test-case "reduce02"   "](-/1 2 3)"     #f         "-2")
    (make-test-case "input01"    "]1:"            "7"        "7")
    (make-test-case "input02"    "]0:"            "1 2 3"    "1 2 3")
    (make-test-case "input03"    "](1:)+1"        "7"        "8")
    (make-test-case "input04"    "]1+0:"          "1 2 3"    "2 3 4")
    (make-test-case "input05"    "](0:)+1:"       "2\n1 2 3" "3 4 5")
    ))

(define build-dir "_build")

(unless (file-directory? build-dir)
  (mkdir build-dir))

(define utf-8-transcoder
  (make-transcoder (utf-8-codec)))

(define skip-malfunction-tests (getenv "QUICK"))

(define (run-test tc)
  (let ([target (format "~a/~a" build-dir (test-case-name tc))])
    (unless skip-malfunction-tests
      (printf "Running ~a (malfunction)... " (test-case-name tc))
      (with-input-from-string (test-case-code tc)
        (lambda ()
          (set-port-name! (current-input-port) target)
          (compile-silly-k target)))
      (let-values ([(to-stdin from-stdout from-stderr process-id)
                    (open-process-ports (format "exec ~a" target) 'line utf-8-transcoder)])
        (when (test-case-input tc)
          (put-string to-stdin (test-case-input tc))
          (flush-output-port to-stdin)
          (close-output-port to-stdin))
        (let ([output (get-line from-stdout)])
          (cond
            [(equal? output (test-case-expected tc)) (printf "ok~%")]
            [else (printf " failed! Output: ~a Expected: ~a~%" output (test-case-expected tc)) #f]))))

    (printf "Running ~a (scheme)... " (test-case-name tc))
    (let ([scm (with-input-from-string (test-case-code tc) compile-to-scheme)])
      (let* ([go (lambda () (with-output-to-string (lambda () (eval scm))))]
             [output (cond
                       [(test-case-input tc) => (lambda (input) (with-input-from-string input go))]
                       [else (go)])])
        (cond
          [(equal? output (test-case-expected tc)) (printf "ok~%")]
          [else (printf " failed! Output: ~a Expected: ~a~%" output (test-case-expected tc)) #f])))

    ))

(for-all run-test test-cases)
