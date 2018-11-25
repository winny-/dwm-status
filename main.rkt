#lang typed/racket/base

(require typed/racket/date
         racket/function
         racket/match
         racket/string)

(require "mpd.rkt"
         "util.rkt")

(require/typed "load-average.rkt"
  [getloadavg (-> (Values Exact-Nonnegative-Integer (Vectorof Real)))])

(define-type Infolet-Event (U String 'emit 'quit))

(: make-infolet/thunk ((-> String) . -> . (Channelof Infolet-Event)))
(define (make-infolet/thunk th)
  (: ch (Channelof Infolet-Event))
  (define ch (make-channel))
  (thread
   (thunk
    (let loop ()
      (match (channel-get ch)
        ['emit (channel-put ch (th)) (loop)]
        ['quit (void)]
        [(? string?) (error 'make-infolet/thunk "Bad event request")]))))
  ch)

; ------------------------------------------------------------
; Renderers

(: display-stdout (String . -> . Void))
(define (display-stdout s)
  (display #\return)
  (display s))
(: display-xsetroot (String . -> . Void))
(define (display-xsetroot s)
  (define-values (sp stdout stdin stderr)
       (subprocess (current-output-port)
                   (current-input-port)
                   (current-error-port)
                   (assert (find-executable-path "xsetroot") path-string?)
                   "-name"
                   s))
     (subprocess-wait sp))

; ------------------------------------------------------------

(define (make-date-infolet)
  (make-infolet/thunk
   (thunk
    (parameterize ([date-display-format 'rfc2822])
      (date->string (current-date) #t)))))

(define (make-load-average-infolet)
  (make-infolet/thunk
   (thunk
    (define-values (n v) (getloadavg))
    (string-join (map number->string (vector->list v)) " ")
    )))

(: make-mpd-infolet (String Exact-Nonnegative-Integer (U String False) . -> . (Channelof Infolet-Event)))
(define (make-mpd-infolet host port password)
  (make-infolet/thunk
   (thunk
    (define h (mpd-status (mpd-address/tcp password host port)))
    (or (and h (string=? "play" (hash-ref (hash-ref h 'status) "state"))
             (format-song h))
        ""))))

; ------------------------------------------------------------

(module+ main
  ; XXX: MPD infolet does not fail gracefully and prevents status from being shown.
  (define infolets (list #;(make-mpd-infolet "localhost" 6600 #f) (make-load-average-infolet) (make-date-infolet)))
  (let loop ()
    (for ([i infolets]) (channel-put i 'emit))
    (display-stdout (string-join (filter non-empty-string? (for/list : (Listof String) ([i infolets])
                                                             (assert (channel-get i) string?)))
                                 " • "))
    (sleep 5)
    (loop)))
