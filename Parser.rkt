#lang racket
(define (parse name)
 (CheckNext(file->lines name)))

(define (CheckNext program)
(if (null? program )
    "Done"
    
    (begin
       (map DebugScan (string-split(first program)))
       (println " ")
       (CheckNext (rest program)))))



(define (Scan token)
  (cond
    [(string->number token) 'number]
    ;Keywords
    [(equal?  "if" token ) 'if ]
    [(equal? "then" token) 'then]
    [(equal? "read" token) 'read]
    [(equal? "write" token) 'write]
    [(equal? "goto" token) 'goto]
    [(equal? "gosub" token) 'gosub]
    [(equal? "return" token) 'return]
    ;Symbols
    [(equal? "+" token) '+]
    [(equal? "-" token) '-]
    [(equal? "=" token) '=]
    [(equal? "(" token) 'LeftParenthesis]
    [(equal? ")" token) 'RightParenthesis]
    [(equal? "$$" token) '$$]
    ;alphabetic characters
    [(andmap char-alphabetic? (string->list token)) 'letters ]
    [else  (error "Scanner Error: Invalid Character" )]
    ))

(define (DebugScan line)
  (print(Scan line)))
  
  
  
