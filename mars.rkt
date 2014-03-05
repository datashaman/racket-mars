#lang racket

(require
  "redcode.rkt")

(struct core (warriors instructions) #:transparent)

(define default-instruction (redcode-instruction '() 'DAT '() '$ '0 '$ '0))

(define (make-core warriors options)
  (let* ([config (hash "core-size" 8000
                        "pspace-size" 500
                        "max-cycles" 80000
                        "max-processes" 8000
                        "max-length" 100
                        "min-distance" 100)])
    (core warriors (make-vector (hash-ref config "core-size") default-instruction))))

(define (first-core options)
  (let ([imp (redcode-parse "imp" (open-input-file "warriors/imp.redcode"))]
        [dwarf (redcode-parse "dwarf" (open-input-file "warriors/dwarf.redcode"))])
    (make-core (list imp dwarf) options)))

(print (first-core (hash "core-size" 100)))
