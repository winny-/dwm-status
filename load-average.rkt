#lang racket

(provide (contract-out [getloadavg (contract:-> (values exact-nonnegative-integer? (vectorof real?)))]))

(require ffi/unsafe
         ffi/unsafe/define
         (prefix-in contract: racket/contract/base))

(define-ffi-definer define-libc (ffi-lib "libc" "6"))

(define-libc getloadavg (_fun [loadavg : (_vector io _double 3) = (make-vector 3 0.0)]
                              [_int = 3]
                              -> [ret : _int]
                              -> (values ret loadavg)))
