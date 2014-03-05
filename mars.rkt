#lang racket

(require
  "redcode.rkt")

(struct core (warriors instructions) #:transparent)

(define default-instruction (redcode-instruction '() 'DAT '() '$ '0 '$ '0))

(define (merge-config options)
  (make-hash (append (list (cons 'core-size 8000)
                           (cons 'pspace-size 500)
                           (cons 'max-cycles 80000)
                           (cons 'max-processes 8000)
                           (cons 'max-length 100)
                           (cons 'min-distance 100))
                     (hash->list options))))

(define (make-core warriors options)
  (let* ([config (merge-config options)]
         [instructions (make-vector (hash-ref config 'core-size) default-instruction)])
    (core warriors instructions)))

(define (first-core options)
  (let ([imp (redcode-parse "imp" (open-input-file "warriors/imp.redcode"))]
        [dwarf (redcode-parse "dwarf" (open-input-file "warriors/dwarf.redcode"))])
    (make-core (list imp dwarf) options)))

(print (first-core (hash 'core-size 100)))
