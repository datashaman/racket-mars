#lang racket

(require "redcode.rkt")

(struct core (instructions config [warriors #:auto #:mutable]) #:transparent #:auto-value '())
(struct warrior (program [queue #:auto #:mutable]
                         [address #:auto #:mutable]) #:transparent)

(define (warrior-instructions warrior)
  (redcode-program-instructions (warrior-program warrior)))

(define default-instruction (redcode-instruction '() 'DAT '() '$ '0 '$ '0))

(define (merge-config options)
  (make-hash (append (list (cons "CORESIZE" 8000)
                           (cons "PSPACESIZE" 500)
                           (cons "MAXCYCLES" 80000)
                           (cons "MAXPROCESSES" 8000)
                           (cons "MAXLENGTH" 100)
                           (cons "MINDISTANCE" 100))
                     (hash->list options))))

(define (place-instructions src dest address)
  (unless (empty? src)
    (vector-set! dest address (first src))
    (place-instructions (rest src) dest (add1 address))))

(define (too-close? address-a address-b coresize mindistance)
  (define difference (abs (- address-a address-b)))
  (or (< difference mindistance)
      (< (- coresize difference) mindistance)))

(define (check-address warriors address coresize mindistance)
  (if (empty? warriors)
    address
    (if (too-close? address (warrior-address (first warriors)) coresize mindistance)
      (generate-address core coresize mindistance)
      (check-address (rest warriors) address coresize mindistance))))

(define (generate-address core coresize mindistance)
  (check-address (core-warriors core) (random coresize) coresize mindistance))

(define (place-warriors core warriors)
  (unless (empty? warriors)
    (let* ([coresize (hash-ref (core-config core) "CORESIZE")]
           [mindistance (hash-ref (core-config core) "MINDISTANCE")]
           [address (generate-address core coresize mindistance)]
           [warrior (first warriors)])
      (set-warrior-address! warrior address)
      (set-core-warriors! core (append (core-warriors core)
                                       (list warrior)))
      (place-instructions
        (warrior-instructions warrior)
        (core-instructions core)
        address)

      (place-warriors core (rest warriors)))))

(define (make-core warriors options)
  (let* ([config (merge-config options)]
         [instructions (make-vector (hash-ref config "CORESIZE") default-instruction)]
         [the-core (core instructions config)])
    (place-warriors the-core warriors)
    the-core))

(define (first-core options)
  (let ([imp (warrior (redcode-parse "imp" (open-input-file "warriors/imp.redcode")))]
        [dwarf (warrior (redcode-parse "dwarf" (open-input-file "warriors/dwarf.redcode")))])
    (make-core (list dwarf imp) options)))

(pretty-print (first-core (hash "CORESIZE" 1000)))
