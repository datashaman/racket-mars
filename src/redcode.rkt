(module redcode racket
  (module+ test
    (require rackunit))
  
  (provide (all-defined-out))
  
  (require parser-tools/yacc
           parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           syntax/readerr)
  
  (struct redcode-directive (key value) #:transparent) 
  (struct redcode-instruction (label opcode modifier mode-a address mode-b address-b) #:transparent)
  (struct redcode-program (input-port directives instructions) #:transparent)
  
  (define (redcode-program-name program)
    (hash-ref (redcode-program-directives program) "name" "anonymous"))
  
  (define-tokens value-tokens (LABEL OPCODE MODIFIER MODE ADDRESS DIRECTIVE))
  (define-empty-tokens op-tokens (EOF))
  
  (define-lex-abbrevs
    [opcode (:or "DAT" "MOV" "ADD" "SUB" "MUL" "DIV" "MOD" "JMP" "JMZ"
                 "JMN" "DJN" "SPL" "CMP" "SEQ" "SNE" "SLT" "LDP" 
                 "STP" "NOP")]
    [modifier (:: #\. (:or "A" "B" "AB" "BA" "F" "X" "I"))]
    [mode (char-set "#$*@{<}>")]
    [sign (:or "" "+" "-")]
    [digit (:/ "0" "9")]
    [address (:or label (:: sign (:+ digit)))]
    [label (:+ alphabetic)]
    [comment (:: #\; (:* (:~ #\newline)))])
  
  (define redcode-lexer
    (lexer-src-pos
     [(:: ";redcode" (:* whitespace) #\newline)
      (token-DIRECTIVE (cons "redcode" '()))]
     [(:: ";redcode-" (:* (:~ #\newline)))
      (token-DIRECTIVE (cons "redcode" (string-trim (second (regexp-match #rx";redcode-(.*)" lexeme)))))]
     [(:: ";" (:or "name" "author" "description" "strategy") (:* (:~ #\newline)))
      (token-DIRECTIVE (let ([match (regexp-match #rx";(name|author|description|strategy)(.*)" lexeme)])
                    (cons (second match) (string-trim (third match)))))]
     [(:or whitespace #\, comment) (return-without-pos (redcode-lexer input-port))]
     [(eof) 'EOF]
     [opcode (token-OPCODE (string->symbol lexeme))]
     [label (token-LABEL lexeme)]
     [modifier (token-MODIFIER (string->symbol (substring lexeme 1)))]
     [mode (token-MODE (string->symbol lexeme))]
     [(:or label address) (token-ADDRESS (string->number lexeme))]))
  
  (define (redcode-parser source-name)
    (parser
     (src-pos)
     (start start)
     (end EOF)
     (tokens value-tokens op-tokens)
     (error (λ (a name val start end)
              (raise-read-error 
               "read-error"
               source-name
               (position-line start)
               (position-col start)
               (position-offset start)
               (- (position-offset end)
                  (position-offset start)))))
     (grammar
      (start
       [(exp-list) (reverse $1)])
      
      (modifier
       [() '()]
       [(MODIFIER) $1])
      
      (mode
       [() '$]
       [(MODE) $1])
      
      (label
       [() '()]
       [(LABEL) $1])
      
      (exp
       [(DIRECTIVE) (redcode-directive (car $1) (cdr $1))]
       [(label OPCODE modifier mode ADDRESS) (redcode-instruction $1 $2 $3 $4 $5 '() '())]
       [(label OPCODE modifier mode ADDRESS mode ADDRESS) (redcode-instruction $1 $2 $3 $4 $5 $6 $7)])
    
      (exp-list
       [() '()]
       [(exp-list exp) (cons $2 $1)]))))
  
  (define (redcode-parse input-port)
    (port-count-lines! input-port)
    (let* ([lines ((redcode-parser input-port) (λ () (redcode-lexer input-port)))]
           [directives (make-hash (map (λ (m) (cons (redcode-directive-key m) (redcode-directive-value m)))
                                       (filter (λ (line) (redcode-directive? line))
                                               lines)))]
           [instructions (filter (λ (line) (redcode-instruction? line))
                                 lines)])
      (redcode-program input-port directives instructions)))
  
  (module+ test
    (let ([imp (redcode-parse (open-input-file "warriors/imp.redcode"))]
          [dwarf (redcode-parse (open-input-file "warriors/dwarf.redcode"))])
      [check-equal? (redcode-program-name imp) "Imp"]
      [check-equal? (redcode-program-name dwarf) "Dwarf"]

      [check-equal? (redcode-program-instructions imp) (list
                                                 (redcode-instruction '() 'MOV '() '$ 0 '$ 1))]
      [check-equal? (redcode-program-instructions dwarf) (list 
                                                 (redcode-instruction '() 'ADD '() '|#| 4 '$ 3)
                                                 (redcode-instruction '() 'MOV '() '$ 2 '@ 2)
                                                 (redcode-instruction '() 'JMP '() '$ -2 '() '())
                                                 (redcode-instruction '() 'DAT '() '|#| 0 '|#| 0))])))
