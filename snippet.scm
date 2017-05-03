(load "silly-k.scm")

(display (with-input-from-string "1+1 2 3" parse-silly-k))
(newline)

(display (with-input-from-string "foo" parse-silly-k))
(newline)

(display (with-input-from-string "foo bar" parse-silly-k))
(newline)

(display (with-input-from-string "{w}" parse-silly-k))
(newline)

(display (with-input-from-string "{1+w}'1 2 3" parse-silly-k))
(newline)
