(import (chezscheme) (silly-k))

(define-record test-case (name code input expected))

(define test-cases
  (list
    (make-test-case "number01"   "]7"                                 #f                 "7")
    (make-test-case "number02"   "]1 2 3"                             #f                 "1 2 3")
    (make-test-case "bool01"     "]1=2"                               #f                 "0")
    (make-test-case "bool02"     "]2=2"                               #f                 "1")
    (make-test-case "less01"     "]2<3"                               #f                 "1")
    (make-test-case "less02"     "]2<2"                               #f                 "0")
    (make-test-case "less03"     "]3<2"                               #f                 "0")
    (make-test-case "less05"     "]@1<1 2 3"                          #f                 "0 1 1")
    (make-test-case "less06"     "]@1 2 3>2"                          #f                 "0 0 1")
    (make-test-case "more01"     "]2>3"                               #f                 "0")
    (make-test-case "more02"     "]2>2"                               #f                 "0")
    (make-test-case "more03"     "]3>2"                               #f                 "1")
    (make-test-case "more04"     "]@{w>1}'1 2 3"                      #f                 "0 1 1")
    (make-test-case "more05"     "]@3>1 2 3"                          #f                 "1 1 0")
    (make-test-case "more06"     "]@1 2 3>2"                          #f                 "0 0 1")
    (make-test-case "equal01"    "]1=2"                               #f                 "0")
    (make-test-case "equal02"    "]2=2"                               #f                 "1")
    (make-test-case "equal03"    "]@{w=2}'1 2 3"                      #f                 "0 1 0")
    (make-test-case "equal04"    "]@1 2 3=3"                          #f                 "0 0 1")
    (make-test-case "equal05"    "]@1=1 2 3"                          #f                 "1 0 0")
    (make-test-case "negation01" "]~1=2"                              #f                 "1")
    (make-test-case "negation02" "]~2=2"                              #f                 "0")
    (make-test-case "negation03" "]~0"                                #f                 "1")
    (make-test-case "negation04" "]~7"                                #f                 "0")
    (make-test-case "negation05" "]~2=1 2 3"                          #f                 "1 0 1")
    (make-test-case "or01"       "](1=2)|2=3"                         #f                 "0")
    (make-test-case "or02"       "](1=2)|2=2"                         #f                 "1")
    (make-test-case "or03"       "](2=2)|2=3"                         #f                 "1")
    (make-test-case "or04"       "](2=2)|3=3"                         #f                 "1")
    (make-test-case "and01"      "](1=2)&2=3"                         #f                 "0")
    (make-test-case "and02"      "](1=2)&2=2"                         #f                 "0")
    (make-test-case "and03"      "](2=2)&2=3"                         #f                 "0")
    (make-test-case "and04"      "](2=2)&3=3"                         #f                 "1")
    (make-test-case "min01"      "]2&3"                               #f                 "2")
    (make-test-case "max01"      "]2|3"                               #f                 "3")
    (make-test-case "addition01" "]1+2"                               #f                 "3")
    (make-test-case "addition02" "]1+2 3"                             #f                 "3 4")
    (make-test-case "addition03" "]1 2+3 4"                           #f                 "4 6")
    (make-test-case "addition04" "]1 2+3"                             #f                 "4 5")
    (make-test-case "addition05" "](1=1)+(2=2)"                       #f                 "2")
    (make-test-case "addition06" "]1+(2=2)"                           #f                 "2")
    (make-test-case "minus01"    "]2-3"                               #f                 "-1")
    (make-test-case "minus02"    "]1-(-2)"                            #f                 "3")
    (make-test-case "minus03"    "]1 2-3 4"                           #f                 "-2 -2")
    (make-test-case "minus04"    "]@-7"                               #f                 "-7")
    (make-test-case "minus05"    "]@-(-2)"                            #f                 "2")
    (make-test-case "minus06"    "]1-2 3"                             #f                 "-1 -2")
    (make-test-case "minus07"    "]1 2-3"                             #f                 "-2 -1")
    (make-test-case "mult01"     "]2*3"                               #f                 "6")
    (make-test-case "mult02"     "]1 2*3"                             #f                 "3 6")
    (make-test-case "mult03"     "]4*2 3"                             #f                 "8 12")
    (make-test-case "mult04"     "]1 2*3 4"                           #f                 "3 8")
    (make-test-case "iota01"     "]!1"                                #f                 "0")
    (make-test-case "iota02"     "]!4"                                #f                 "0 1 2 3")
    (make-test-case "over01"     "]@{w+1}'1 2 3"                      #f                 "2 3 4")
    (make-test-case "over02"     "]@{1-w}'3 4 5"                      #f                 "-2 -3 -4")
    (make-test-case "over03"     "]2{a+w}'3 4 5"                      #f                 "5 6 7")
    (make-test-case "over04"     "]2{w-a}'3 4 5"                      #f                 "1 2 3")
    (make-test-case "over05"     "]2+'3 4 5"                          #f                 "5 6 7")
    (make-test-case "over06"     "]2-'3 4 5"                          #f                 "-1 -2 -3")
    (make-test-case "reduce01"   "]@+/1 2 3"                          #f                 "6")
    (make-test-case "reduce02"   "]@-/1 2 3"                          #f                 "2")
    (make-test-case "reduce03"   "]@{w-a}/1 2 3"                      #f                 "0")
    (make-test-case "reduce04"   "]+/2<!5"                            #f                 "2")
    (make-test-case "input01"    "]1:"                                "7"                "7")
    (make-test-case "input02"    "]0:"                                "1 2 3"            "1 2 3")
    (make-test-case "input03"    "](1:)+1"                            "7"                "8")
    (make-test-case "input04"    "]1+0:"                              "1 2 3"            "2 3 4")
    (make-test-case "input05"    "](0:)+1:"                           (list "2" "1 2 3") "3 4 5")
    (make-test-case "cond01"     "](1=1;2;3)"                         #f                 "2")
    (make-test-case "cond02"     "](1=2;1 2;3 4)"                     #f                 "3 4")
    (make-test-case "cond03"     "]7{w=1;w;a}1"                       #f                 "1")
    (make-test-case "cond04"     "]7{w=1;w;a}8"                       #f                 "7")
    (make-test-case "cond05"     "]{w=1;w+1;w=2;w+2;w+3}1"            #f                 "2")
    (make-test-case "cond06"     "]{w=1;w+1;w=2;w+2;w+3}2"            #f                 "4")
    (make-test-case "cond07"     "]{w=1;w+1;w=2;w+2;w+3}3"            #f                 "6")
    (make-test-case "cond08"     "]{w=1;w+1;w=2;w+2;w+3}4"            #f                 "7")
    (make-test-case "cond09"     "](1=1;2;3)"                         #f                 "2")
    (make-test-case "self01"     "]{w=0;0;w+_f(w-1)}6"                #f                 "21")
    (make-test-case "fibonacci"  "]{w=1;1;w=2;1;(_f(w-2))+_f(w-1)}1:" "7"                "13")
    (make-test-case "bind01"     "]x+x:7"                             #f                 "14")
    (make-test-case "bind02"     "](x:1)+x:2"                         #f                 "3")
    (make-test-case "bind03"     "]x+(x:1)+x:2"                       #f                 "4")
    (make-test-case "bind04"     "]x+x{x:1+a-w}x:2"                   #f                 "3")
    ))

(with-output-to-file
  "tests.markdown"
  (lambda ()
    (printf "Code | Stdin | Stdout~%")
    (printf "---- | ----- | ------~%")
    (for-all
      (lambda (tc)
        (let ([input (test-case-input tc)])
          (cond
            [(and input (list? input))
             (printf "`~a` | `~{~a~^\\n~}` | `~a`~%"
                     (test-case-code tc)
                     input
                     (test-case-expected tc))]
            [(and input (string? input))
             (printf "`~a` | `~a` | `~a`~%"
                     (test-case-code tc)
                     input
                     (test-case-expected tc))]
            [else
              (printf "`~a` | | `~a`~%" (test-case-code tc) (test-case-expected tc))])))
      test-cases))
  '(replace))

(define build-dir "_build")

(unless (file-directory? build-dir)
  (mkdir build-dir))

(define utf-8-transcoder
  (make-transcoder (utf-8-codec)))

(define skip-malfunction-tests (getenv "QUICK"))

(define (run-test tc)
  (let ([target (format "~a/~a" build-dir (test-case-name tc))])
    (and
      (unless skip-malfunction-tests
        (printf "Running ~a (malfunction)... " (test-case-name tc))
        (with-input-from-string (test-case-code tc)
          (lambda ()
            (set-port-name! (current-input-port) target)
            (compile-silly-k target)))
        (let-values ([(to-stdin from-stdout from-stderr process-id)
                      (open-process-ports (format "exec ~a" target) 'line utf-8-transcoder)])
          (let ([input (test-case-input tc)])
            (when input
              (cond
                [(list? input)
                 (for-all (lambda (s) (put-string to-stdin s) (newline to-stdin)) input)]
                [else (put-string to-stdin input)])
              (flush-output-port to-stdin)
              (close-output-port to-stdin)))
          (let ([output (get-line from-stdout)])
            (cond
              [(equal? output (test-case-expected tc)) (printf "ok~%")]
              [else (printf " failed! Output: ~a Expected: ~a~%" output (test-case-expected tc)) #f]))))

      (begin
        (printf "Running ~a (scheme)... " (test-case-name tc))
        (let ([scm (with-input-from-string (test-case-code tc) compile-to-scheme)])
          (let* ([go (lambda () (with-output-to-string (lambda () (eval scm))))]
                 [output (cond
                           [(test-case-input tc) => (lambda (input)
                              (cond
                                [(list? input)
                                 (with-input-from-string (format "~{~a~^~%~}" input) go)]
                                [else
                                 (with-input-from-string input go)]))]
                           [else (go)])])
            (cond
              [(equal? output (test-case-expected tc)) (printf "ok~%")]
              [else (printf " failed! Output: ~a Expected: ~a~%" output (test-case-expected tc)) #f])))))))

(for-all run-test test-cases)
