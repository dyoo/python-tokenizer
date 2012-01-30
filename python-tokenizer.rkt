#lang racket/base

;; This is a translation of the tokenizer.py library from Python.
;;
;; See:
;;
;; http://hg.python.org/cpython/file/2.7/Lib/tokenize.py

(require racket/generator
         racket/list
         racket/sequence
         data/gvector
         (for-syntax racket/base)
         (only-in srfi/13 string-trim-right)
         "while-loop.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper macros:
;;
;; We define a few macros here to help port the code over.
;;
;; The more significant one, the while loop macro, I've put in a
;; separate file "while-loop.rkt".  Here are a few miscellaneous
;; macros:


;; The original Python library uses mutation heavily.
;; As such, we'll extend the syntax of this file so that variable mutation 
;; (set!) is shorter to write, supports chaining, and whose result
;; is the rhs value.
(define-syntax (<- stx)
  (syntax-case stx ()
    [(= id1 id-rest ... val) 
     (andmap identifier? (syntax->list #'(id1 id-rest ...)))
     (syntax/loc stx
       (let ([v val])
         (set! id1 v)
         (set! id-rest v) ...
         v))]))


;; Since there's quite a bit of mutative variable
;; incrementing, we provide a small syntax for this.
(define-syntax (++ stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (syntax/loc stx
       (set! id (add1 id)))]))



(define tabsize 8)


;; Token errors will be exceptions:
(define-struct (exn:fail:token exn:fail) (loc))
;; As will indentation errors:
(define-struct (exn:fail:indentation exn:fail) (loc))


;; string-right-ref: string number -> char
;; Referencing characters from the right side.
(define (string-right-ref str n)
  (string-ref str (- (string-length str) n)))


;; slice-end: string number -> string
;; Slice the end of a string.
(define (slice-end str n)
  (substring str
             (max (- (string-length str) n)
                  0)))

;; rstrip-newlines: string -> string
;; Trim off the newline characters off a string.
(define (rstrip-newlines s)
  (regexp-replace #px"[\r\n]+$" s ""))

;; gvector-pop!: (gvectorof X) -> X
;; Remove the last element of the gvector and return it.
(define (gvector-pop! gv)
  (define last-index (sub1 (gvector-count gv)))
  (define val (gvector-ref gv last-index))
  (gvector-remove! gv last-index))


;; gvector-last: (gvectorof X) -> X
(define (gvector-last gv)
  (define last-index (sub1 (gvector-count gv)))
  (gvector-ref gv last-index))


;; gvector-member: X (gvectorof X) -> boolean
(define (gvector-member x gv)
  (let/ec return
    (for ([elt (in-gvector gv)])
      (when (equal? x elt)
        (return #t)))
    (return #f)))


;; What are our token types?  Here they are:
(define NAME 'NAME)
(define NUMBER 'NUMBER)
(define STRING 'STRING)
(define COMMENT 'COMMENT)
(define NL 'NL)
(define DEDENT 'DEDENT)
(define INDENT 'INDENT)
(define ERRORTOKEN 'ERRORTOKEN)
(define ENDMARKER 'ENDMARKER)



;; generate-tokens: sequence -> sequence
(define (generate-tokens line-sequence)
  #|
        The generate_tokens sequence requires one argument, line-sequence, which
        must be a sequence-like object.
    
        The returned sequence produces 5-tuples with these members: the token type; the
        token string; a 2-tuple (srow, scol) of ints specifying the row and
        column where the token begins in the source; a 2-tuple (erow, ecol) of
        ints specifying the row and column where the token ends in the source;
        and the line on which the token was found. The line passed is the
        logical line; continuation lines are included.
  |#
  
  (in-generator
    
    ;; The idiom for reading from a sequence in Racket doesn't use
    ;; "it's easier to ask forgiveness than permission".
    (define-values (read-line-not-exhausted? read-line) 
      (sequence-generate line-sequence))
    
    (define lnum 0)
    (define strstart (list 0 0))
    (define start 0)
    (define end 0)
    (define pos #f)
    (define max #f)
    (define column 0)
    (define parenlev 0)
    (define continued? #f)
    (define namechars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (define numchars "0123456789")
    (define contstr "")
    (define needcont? #f)
    (define contline #f)
    (define indents (gvector 0))
    (define line "")
    (define endprog #px"")
    
    (while #t                          ;; loop over lines in stream
           (if (read-line-not-exhausted?)
               (<- line (read-line))
               (<- line ""))
           (++ lnum)
           (<- pos 0)
           (<- max (string-length line))

           (cond 
             [(> (string-length contstr) 0)                  ;; continued string
              (when (string=? line "")
                (raise (exn:fail:token "EOF in multi-line string")
                       (current-continuation-marks)
                       strstart))
              ;; Note: endprog must anchor the match with "^" or else
              ;; this does not have equivalent behavior to Python!
              (define endmatch (regexp-match-positions endprog line))
              (cond
                [endmatch
                 (<- pos end 
                     (cdr (first endmatch)))
                 (yield STRING
                        (string-append contstr (substring line 0 end))
                        strstart
                        (list lnum end)
                        (string-append contline  line))
                 (<- contstr "")
                 (<- needcont? #f)
                 (<- contline #f)]
                
                [(and needcont?
                      (not (string=? (slice-end line 2) "\\\n"))
                      (not (string=? (slice-end line 3) "\\\r\n")))
                 (yield ERRORTOKEN
                        (string-append contstr line)
                        strstart
                        (list lnum (string-length line))
                        contline)
                 (<- contstr "")
                 (<- contline #f)
                 (continue)]
                
                [else
                 (<- contstr (string-append contstr line))
                 (<- contline (string-append contline line))
                 (continue)])]
             
             [(and (= parenlev 0)
                   (not continued?))                    ;; new statement
              (when (string=? line "")
                (break))
              (<- column 0)
              (while (< pos max)                        ;; measure leading whitespace
                     (cond
                       [(char=? (string-ref line pos) #\space)
                        (++ column)]
                       [(char=? (string-ref line pos) #\tab)
                        (<- column (* tabsize (add1 (quotient column tabsize))))]
                       [(char=? (string-ref line pos) #\page)
                        (<- column 0)]
                       [else
                        (break)])
                     (++ pos))
              (when (= pos max)
                (break))

              (when (member (string-ref line pos) (list #\# #\return #\newline))
                (cond
                  [(char=? (string-ref line pos) #\#)
                   (define comment-token (rstrip-newlines (substring line pos)))
                   (define nl-pos (+ pos (string-length comment-token)))
                   (yield COMMENT
                          comment-token
                          (list lnum pos)
                          (list lnum (+ pos (string-length comment-token)))
                          line)
                   (yield NL
                          (substring line nl-pos)
                          (list lnum nl-pos)
                          (list lnum (string-length line))
                          line)]
                  [else
                   (yield (if (char=? (string-ref line pos) #\#) COMMENT NL)
                          (string-ref line pos)
                          (list lnum pos)
                          (list lnum (string-length line))
                          line)])
                (continue))
                
              (when (> column (gvector-last indents))  ;; count indents or dedents
                (gvector-add! indents column)
                (yield INDENT
                       (string-ref line 0 pos)
                       (list lnum 0)
                       (list lnum pos)
                       line))
              (while (< column (gvector-last indents))
                (unless (gvector-member column indents)
                  (raise (exn:fail:indentation "unindent does not match any outer indentation level"
                                               (current-continuation-marks)
                                                (list "<tokenize>" lnum pos line)))
                  (gvector-pop! indents)
                  (yield DEDENT 
                         ""
                         (list lnum pos)
                         (list lnum pos)
                         line)))]

             [else                                     ;; continued statement
              (if (= (string-length line) 0)
                  (raise (exn:fail:token "EOF in multi-line statement" 
                                         (current-continuation-marks)
                                         (list lnum 0)))
                  (set! continued? #f))])
;   359 
;   360         while pos < max:
;   361             pseudomatch = pseudoprog.match(line, pos)
;   362             if pseudomatch:                                # scan for tokens
;   363                 start, end = pseudomatch.span(1)
;   364                 spos, epos, pos = (lnum, start), (lnum, end), end
;   365                 token, initial = line[start:end], line[start]
;   366 
;   367                 if initial in numchars or \
;   368                    (initial == '.' and token != '.'):      # ordinary number
;   369                     yield (NUMBER, token, spos, epos, line)
;   370                 elif initial in '\r\n':
;   371                     yield (NL if parenlev > 0 else NEWLINE,
;   372                            token, spos, epos, line)
;   373                 elif initial == '#':
;   374                     assert not token.endswith("\n")
;   375                     yield (COMMENT, token, spos, epos, line)
;   376                 elif token in triple_quoted:
;   377                     endprog = endprogs[token]
;   378                     endmatch = endprog.match(line, pos)
;   379                     if endmatch:                           # all on one line
;   380                         pos = endmatch.end(0)
;   381                         token = line[start:pos]
;   382                         yield (STRING, token, spos, (lnum, pos), line)
;   383                     else:
;   384                         strstart = (lnum, start)           # multiple lines
;   385                         contstr = line[start:]
;   386                         contline = line
;   387                         break
;   388                 elif initial in single_quoted or \
;   389                     token[:2] in single_quoted or \
;   390                     token[:3] in single_quoted:
;   391                     if token[-1] == '\n':                  # continued string
;   392                         strstart = (lnum, start)
;   393                         endprog = (endprogs[initial] or endprogs[token[1]] or
;   394                                    endprogs[token[2]])
;   395                         contstr, needcont? = line[start:], true
;   396                         contline = line
;   397                         break
;   398                     else:                                  # ordinary string
;   399                         yield (STRING, token, spos, epos, line)
;   400                 elif initial in namechars:                 # ordinary name
;   401                     yield (NAME, token, spos, epos, line)
;   402                 elif initial == '\\':                      # continued stmt
;   403                     continued = 1
;   404                 else:
;   405                     if initial in '([{':
;   406                         parenlev += 1
;   407                     elif initial in ')]}':
;   408                         parenlev -= 1
;   409                     yield (OP, token, spos, epos, line)
;   410             else:
;   411                 yield (ERRORTOKEN, line[pos],
;   412                            (lnum, pos), (lnum, pos+1), line)
;   413                 pos += 1
;   414 
           (for ([indent (sequence-tail indents 1)]) ;; pop remaining indent levels
             (yield DEDENT
                    ""
                    (list lnum 0)
                    (list lnum 0)
                    ""))
           (yield ENDMARKER
                  ""
                  (list lnum 0)
                  (list lnum 0)
                  ""))))