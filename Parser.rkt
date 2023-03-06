#lang racket
(require racket/exn)
(define (parse name)
 (CheckNext(file->lines name)))

(define (CheckNext program)

    (begin
      ;These bindings are nesscary in order to save line number in case of error. Otherwise this logc could be done directly in the upcomming map function
       (let ([thisline (first program)] )
        (let ([thisline<list> (string-split thisline)])
            ;Checks for end of input. If there is more input continue parsing.
          (if (and(null? (rest program)) (equal? (Scan (first thisline<list>)) '$$ ))
              "Program Accepted :)"
    
              ;Checks if first token is the line number. Parsing only continues if a linenumber is able to be bound for this line
              (if (not(equal? (Scan(first thisline<list>)) 'number))
                  (error "Parse Error: Missing Linenumber")
                  (begin
                    (let([linenum (string->number(first thisline<list>))])
                      (with-handlers ([exn:fail? ;Catches an error and appends the line number, then rethrows the error
                                       ;TODO: find the name of the error being caught
                                    (lambda (exn) (string-append "Error on line " (number->string linenum)))])  
                      (map Scan thisline<list>)
                      (CheckNext (rest program)))))))))))



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
    [(equal? ":" token) ':]
    [(equal? "$$" token) '$$] 
    ;alphabetic characters
    [(andmap char-alphabetic? (string->list token)) 'letters ]
    [else  (error "Scanner Error: Invalid Character" )]
    ))

(define (DebugScan token)
  (println(Scan token))
  (Scan token)) 
  
  
  
