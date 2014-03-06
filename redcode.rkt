(module redcode racket
  (module+ test
    (require rackunit))
  
  (provide redcode-parse redcode-instruction redcode-program redcode-program-instructions)
  
  (require parser-tools/yacc
           parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           syntax/readerr)
  
  (struct redcode-instruction (label opcode modifier mode-a address mode-b address-b) #:transparent)
  (struct redcode-program (label source instructions) #:transparent)
  
  (define-tokens value-tokens (LABEL OPCODE MODIFIER MODE ADDRESS))
  (define-empty-tokens op-tokens (EOF))
  
  (define-lex-abbrevs
    [opcode (:or "DAT" "MOV" "ADD" "SUB" "MUL" "DIV" "MOD" "JMP" "JMZ"
                 "JMN" "DJN" "SPL" "CMP" "SEQ" "SNE" "SLT" "LDP" 
                 "STP" "NOP")]
    [modifier (:: #\. (:or "A" "B" "AB" "BA" "F" "X" "I"))]
    [mode (char-set "#$*@{<}>")]
    [sign (:or "" "+" "-")]
    [digit (:/ "0" "9")]
    [address (:: sign (:+ digit))]
    [label (:+ alphabetic)]
    [comment (:: #\; (:* (:~ #\newline)))])
  
  (define redcode-lexer
    (lexer-src-pos
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
       [(label OPCODE modifier mode ADDRESS) (redcode-instruction $1 $2 $3 $4 $5 '() '())]
       [(label OPCODE modifier mode ADDRESS mode ADDRESS) (redcode-instruction $1 $2 $3 $4 $5 $6 $7)])
    
      (exp-list
       [() '()]
       [(exp-list exp) (cons $2 $1)]))))
  
  (define (redcode-parse name input-port)
    (port-count-lines! input-port)
    (redcode-program name input-port ((redcode-parser name) (λ () (redcode-lexer input-port)))))
  
  (module+ test
    (let ([imp (redcode-parse "imp" (open-input-file "warriors/imp.redcode"))]
          [code (redcode-parse "dwarf" (open-input-file "warriors/dwarf.redcode"))])
      [check-equal? (redcode-program-instructions imp) (list (redcode-instruction '() 'MOV '() '$ 0 '$ 1))]
      [check-equal? (redcode-program-instructions code) (list 
                                                 (redcode-instruction '() 'ADD '() '|#| 4 '$ 3)
                                                 (redcode-instruction '() 'MOV '() '$ 2 '@ 2)
                                                 (redcode-instruction '() 'JMP '() '$ -2 '() '())
                                                 (redcode-instruction '() 'DAT '() '|#| 0 '|#| 0))])))
