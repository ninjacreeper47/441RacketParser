#lang racket

;Known limitations: all tokens must be seperated by whitespace. Does not check for nonzero leading digits in any numbers.  
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
                      (if(programline (rest(map Scan thisline<list>)))
                      (CheckNext (rest program))
                      (error "Invalid line")))))))))))



(define (Scan token)
  (cond
    ;TODO passback actual number in order to implment idx and num checking
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
  


;TODO check for leading 0 digits
(define (idx stream)
  (if(equal? (first stream)'number)
     (rest stream)
     #f))

(define (id stream)
  (if(equal? (first stream) 'letters)
     (rest stream)
     #f))
 (define (op= stream)
  (if(equal? (first stream) '=)
     (rest stream)
     #f))
(define (op+ stream)
  (if(equal? (first stream) '+)
     (rest stream)
     #f))
(define (op- stream)
  (if(equal? (first stream) '-)
     (rest stream)
     #f))
(define (opLP stream)
  (if(equal? (first stream) 'LeftParenthesis)
     (rest stream)
     #f))
(define (opRP stream)
  (if(equal? (first stream) 'RightParenthesis)
     (rest stream)
     #f))
(define (op: stream)
  (if(equal? (first stream) ':)
     (rest stream)
     #f))
(define (key:if stream)
  (if(equal? (first stream) 'if)
     (rest stream)
     #f))
(define (key:then stream)
  (if(equal? (first stream) 'then)
     (rest stream)
     #f))
(define (key:read stream)
  (if(equal? (first stream) 'read)
     (rest stream)
     #f))
(define (key:write stream)
  (if(equal? (first stream) 'write)
     (rest stream)
     #f))
(define (key:goto stream)
  (if(equal? (first stream) 'goto)
     (rest stream)
     #f))
(define (key:gosub stream)
  (if(equal? (first stream) 'gosub)
     (rest stream)
     #f))
(define (key:return stream)
  (if(equal? (first stream) 'return)
     (rest stream)
     #f))

(define (digits stream)
  (if(equal? (first stream) 'number)
     (rest stream)
     #f))
(define (numsign stream)
  (cond
    [(empty? stream)
     stream]
    [(op+ stream)
     (op+ stream)]
    [(op- stream)
     (op- stream)]
    [else stream]))

(define (num stream)
  (digits(numsign stream)))

(define (etail stream)
  (cond
    [(empty? stream)
     stream]
    [(op+ stream)
     (expr(op+ stream))]
    [(op- stream)
    (expr(op- stream))]
    [(op= stream)
     (expr(op= stream))]
    [else stream]))

(define (expr stream)
  (cond
    [(id stream)
     (etail(id stream))]
    [(num stream)
     (etail(num stream))]
    [(opLP stream)
     (opRP(expr(opLP stream)))]
    [else #f]))
  
(define (stmt stream)
  (cond
   [(id stream)
     (expr(op=(id stream))) ]
   [(key:if stream)
    (stmt(key:then(expr(key:if stream))))]
   [(key:read stream)
    (id(key:read stream))]
   [(key:write stream)
    (expr(key:write stream))]
   [(key:goto stream)
    (idx(key:goto stream))]
   [(key:gosub stream)
    (idx(key:gosub stream))]
   [(key:return stream)
    (key:return stream)]
    [else #f]))

(define (programline stream) ;Note linenumbers are checked in the parse function
  (if(empty? (stmt stream))
     #t
     (if (op:(stmt stream))
         (programline (op:(stmt  stream)))
         #f)))