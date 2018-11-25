#lang typed/racket/base

(provide string-truncate)

(require racket/string)

(: string-truncate (String Exact-Nonnegative-Integer [#:end String] [#:trim Boolean] . -> . String))
(define (string-truncate s len #:end [end "…"] #:trim [trim #t])
  (define s1 (if trim (string-trim s) s))
  (if (<= (string-length s1) len)
      s1
      (string-append (string-trim (substring s1 0 len)) end)))
