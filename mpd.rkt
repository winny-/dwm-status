#lang typed/racket/base

(provide (all-defined-out))

(require #;racket/contract/base
         racket/function
         racket/match
         racket/path
         racket/string
         racket/tcp)

(require "util.rkt")

(struct mpd-address ([password : (U String False)]))
(struct mpd-address/unix mpd-address ([path : Path-String]))
(struct mpd-address/tcp mpd-address ([host : String]
                                     [port : Exact-Nonnegative-Integer]))

(define-type Mpd-Address mpd-address/tcp)

(define-type Mpd-Status-HashTable (HashTable (U 'current 'status) (HashTable String String)))

(: mpd-status (Mpd-Address . -> . (U False Mpd-Status-HashTable)))
(define (mpd-status address)
  (match-define (struct* mpd-address/tcp ([host host] [port port])) address)
  (match-define (struct* mpd-address ([password password])) address)
  (define-values (ip op) (tcp-connect host port))
  (with-handlers ([exn:fail? (const #f)])
    (read-response ip)
    (when password
      (write-string (format "password ~a\n" password) op)
      (flush-output op)
      (displayln (read-response ip)))
    (write-string "status\n" op)
    (flush-output op)
    (define status-lines (read-response ip))
    (write-string "currentsong\n" op)
    (flush-output op)
    (define current-lines (read-response ip))
    (write-string "close\n" op)
    (flush-output op)
    (close-output-port op)
    (close-input-port ip)
    (hash 'current (response-lines->hash current-lines)
          'status (response-lines->hash status-lines))))

(: format-song (Mpd-Status-HashTable . -> . String))
(define (format-song ht)
  (match (hash-ref ht 'current)
    [(hash-table ("Artist" (and (not "") a))
                 ("Title" (and (not "") t)))
     (string-append (string-truncate t 15)
                     " — "
                     (string-truncate a 15))]
    [(hash-table ("file" (and (not "") f)))
     (string-truncate (assert (file-name-from-path f) string?) 15)]))

(: response-lines->hash ((Listof String) . -> . (HashTable String String)))
(define (response-lines->hash ls)
  (for/hash : (HashTable String String) ([line ls])
    (match-define (list _ k v) (regexp-match #rx"^([^: ]*): (.*)" line))
    (values (assert k string?) (assert v string?))))

(: read-response (Input-Port . -> . (Listof String)))
(define (read-response ip)
  (let loop ([ls : (Listof String) '()])
    (match (read-line ip 'linefeed)
      [(? eof-object?) (error 'read-response "Bad response, early EOF")]
      [(regexp #rx"^OK ?(.*)?" (list _ message))
       (reverse
        (if (and message (non-empty-string? message))
            (cons message ls)
            ls))]
      [(regexp #rx"^ACK ?(.*)?" (list _ message))
       (if (and message (non-empty-string? message))
           (error 'read-response "Error from server: ~v" message)
           (error 'read-response "Unspecified error from server"))]
      [(? string? s) (loop (cons s ls))])))
