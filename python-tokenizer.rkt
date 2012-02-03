#lang at-exp racket/base

;; This is a translation of the tokenizer.py library from Python.
;; 
;; Translation by Danny Yoo (dyoo@hashcollision.org)
;;
;; See:
;;
;;     http://hg.python.org/cpython/file/2.7/Lib/tokenize.py
;;
;; for the original Python sources.


(require racket/generator
         racket/list
         racket/sequence
         racket/string
         racket/set
         data/gvector
         (for-syntax racket/base)
         (planet dyoo/while-loop))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper macros:
;;
;; We define a few macros here to help port the code over.
;;
;; The more significant one, the while loop macro, I've put in a
;; separate file "while-loop.rkt".

;; Here are a few miscellaneous macros:
;;

;; set! multiple identifiers at once.
(define-syntax (set!* stx)
  (syntax-case stx ()
    [(= id1 id-rest ... val) 
     (andmap identifier? (syntax->list #'(id1 id-rest ...)))
     (syntax/loc stx
       (let ([v val])
         (set! id1 v)
         (set! id-rest v) ...))]))

;; Since there's quite a bit of mutative variable
;; incrementing and decrementing, we can provide a small syntax for this.
(define-syntax (++ stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (syntax/loc stx
       (set! id (add1 id)))]))

(define-syntax (-- stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (syntax/loc stx
       (set! id (sub1 id)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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

;; my-gvector-pop!: (gvectorof X) -> X
;; Remove the last element of the gvector and return it.
(define (my-gvector-pop! gv)
  (define last-index (sub1 (gvector-count gv)))
  (define val (gvector-ref gv last-index))
  (gvector-remove! gv last-index)
  val)

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


;; What are our token types?
;; In the original Python sources, they were integers
;; Here, we'll use symbols.
(define NAME 'NAME)
(define NUMBER 'NUMBER)
(define STRING 'STRING)
(define OP 'OP)
(define COMMENT 'COMMENT)
(define NL 'NL)
(define DEDENT 'DEDENT)
(define INDENT 'INDENT)
(define ERRORTOKEN 'ERRORTOKEN)
(define ENDMARKER 'ENDMARKER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular expression stuff.

(define (group . choices)
  (string-append "(" (string-join choices "|") ")"))

(define (any . choices)
  (string-append (apply group choices) "*"))

(define (maybe . choices)
  (string-append (apply group choices) "?"))



;; We'll use @-reader support to get equivalent functionality to
;; Python's raw strings.
;; See: http://jarnaldich.me/2011/08/07/raw-strings-in-racket.html
;; for a discussion.  Raw strings let us do the proper string escaping.
(define r string-append)



(define Whitespace "[ \f\t]*")
(define Comment "#[^\r\n]*")
(define Ignore (string-append Whitespace 
                              (any (string-append "\\\r?\n"  Whitespace))
                              (maybe Comment)))
(define Name @r{[a-zA-Z_]\w*})

(define Hexnumber @r{0[xX][\da-fA-F]+[lL]?})
(define Octnumber @r{(0[oO][0-7]+)|(0[0-7]*)[lL]?})
(define Binnumber @r{0[bB][01]+[lL]?})
(define Decnumber @r{[1-9]\d*[lL]?})
(define Intnumber (group Hexnumber Binnumber Octnumber Decnumber))
(define Exponent  @r{[eE][-+]?\d+})
(define Pointfloat (string-append
                    (group @r{\d+\.\d*} @r{\.\d+}) 
                    (maybe Exponent)))
(define Expfloat (string-append @r{\d+} Exponent))
(define Floatnumber (group Pointfloat Expfloat))
(define Imagnumber (group @r{\d+[jJ]}
                          (string-append Floatnumber @r{[jJ]})))

(define Number (group Imagnumber Floatnumber Intnumber))


;; Tail end of ' string.
(define Single @r{[^'\\]*(?:\\.[^'\\]*)*'})
;; Tail end of " string.
(define Double @r{[^"\\]*(?:\\.[^"\\]*)*"})
;; Tail end of ''' string.
(define Single3 @r{[^'\\]*(?:(?:\\.|'(?!''))[^'\\]*)*'''})
;; Tail end of """ string.
(define Double3 @r{[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*"""})
(define Triple (group @r{[uU]?[rR]?'''} @r{[uU]?[rR]?"""}))
;; Single-line ' or " string.
;;
;; dyoo: Note that there's a little trickiness since the newline
;; has to really be in the string (and not its raw-string equivalent).
;; That's why the translation unescapes the newlines in the regexp group.
(define String (group @r{[uU]?[rR]?'[^@r["\n"]'\\]*(?:\\.[^@r["\n"]'\\]*)*'}
                      @r{[uU]?[rR]?"[^@r["\n"]"\\]*(?:\\.[^@r["\n"]"\\]*)*"}))


;; Because of leftmost-then-longest match semantics, be sure to put the
;; longest operators first (e.g., if = came before ==, == would get
;; recognized as two instances of =).
(define Operator (group @r{\*\*=?} @r{>>=?} @r{<<=?} @r{<>} @r{!=}
                        @r{//=?}
                        @r{[+\-*/%&|^=<>]=?}
                        @r{~}))

(define Bracket "[][(){}]")
(define Special (group "\r?\n" @r|{[:;.,`@]}|))
(define Funny (group Operator Bracket Special))

(define PlainToken (group Number Funny String Name))
(define Token (string-append Ignore PlainToken))

;;  First (or only) line of ' or " string.
(define ContStr (group (string-append @r{[uU]?[rR]?'[^@r["\n"]'\\]*(?:\\.[^@r["\n"]'\\]*)*}
                                      (group "'" "\\\r?\n"))
                       (string-append @r{[uU]?[rR]?"[^@r["\n"]"\\]*(?:\\.[^@r["\n"]"\\]*)*}
                                      (group @r{"}  "\\\r?\n"))))

(define PseudoExtras (group "\\\r?\n" Comment Triple))

(define PseudoToken 
  (string-append Whitespace (group PseudoExtras Number Funny ContStr Name)))


(define-values (tokenprog pseudoprog single3prog double3prog)
  (apply values
         (map (lambda (x)
                ;; Slight change: explicitly adding the leading anchor
                ;; to force matches at the beginning
                (pregexp (string-append "^" x)))
              (list Token PseudoToken Single3 Double3))))

(define endprogs 
  (hash @r{'} (pregexp (string-append "^" Single))
        @r{"} (pregexp (string-append "^" Double))
        @r{'''} single3prog
        @r{"""} double3prog
        @r{'''} single3prog
        @r{"""} double3prog
        @r{u'''} single3prog
        @r{u"""} double3prog
        @r{ur'''} single3prog
        @r{ur"""} double3prog
        @r{R'''} single3prog
        @r{R"""} double3prog
        @r{U'''} single3prog
        @r{U"""} double3prog
        @r{uR'''} single3prog
        @r{uR"""} double3prog
        @r{Ur'''} single3prog
        @r{Ur"""} double3prog
        @r{UR'''} single3prog
        @r{UR"""} double3prog
        @r{b'''} single3prog
        @r{b"""} double3prog
        @r{br'''} single3prog
        @r{br"""} double3prog
        @r{B'''} single3prog
        @r{B"""} double3prog
        @r{bR'''} single3prog
        @r{bR"""} double3prog
        @r{Br'''} single3prog
        @r{Br"""} double3prog
        @r{BR'''} single3prog
        @r{BR"""} double3prog
        @r{r} #f
        @r{R} #f
        @r{u} #f
        @r{U} #f
        @r{b} #f
        @r{B} #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define tabsize 8)


;; Notes during the translation of generate-tokens:
;;
;; One of the nasty things is to see how much state's involved in
;; tokenization.  In the original sources, it's a bit hard to tell 
;; what all the lexer's state is, since variables are function-scoped.



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
   ;; Slight deviation: rather than represent namechars as a string, use a set.
   (define namechars (apply set (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
   ;; Slight deviation: rather than represent numchars as a string, use a set.
   (define numchars (apply set (string->list "0123456789")))
   (define contstr "")
   (define needcont? #f)
   (define contline #f)
   (define indents (gvector 0))
   (define line "")
   (define endprog #px"")
   
   (while #t                          ;; loop over lines in stream
     (if (read-line-not-exhausted?) (set! line (read-line)) (set! line ""))
     (++ lnum)
     (set! pos 0)
     (set! max (string-length line))
     
     (cond 
       [(> (string-length contstr) 0)                  ;; continued string
        (when (string=? line "")
          (raise (exn:fail:token "EOF in multi-line string")
                 (current-continuation-marks)
                 strstart))
        ;; Note: endprog must anchor the match with "^" or else
        ;; this does not have equivalent behavior to Python.
        (define endmatch (regexp-match-positions endprog line))
        (cond
          [endmatch
           (set!* pos end 
                  (cdr (first endmatch)))
           (yield STRING
                  (string-append contstr (substring line 0 end))
                  strstart
                  (list lnum end)
                  (string-append contline  line))
           (set! contstr "")
           (set! needcont? #f)
           (set! contline #f)]
          
          [(and needcont?
                (not (string=? (slice-end line 2) "\\\n"))
                (not (string=? (slice-end line 3) "\\\r\n")))
           (yield ERRORTOKEN
                  (string-append contstr line)
                  strstart
                  (list lnum (string-length line))
                  contline)
           (set! contstr "")
           (set! contline #f)
           (continue)]
          
          [else
           (set! contstr (string-append contstr line))
           (set! contline (string-append contline line))
           (continue)])]
       
       [(and (= parenlev 0)
             (not continued?))                    ;; new statement
        (when (string=? line "")
          (break))
        (set! column 0)
        (while (< pos max)                        ;; measure leading whitespace
          (cond
            [(char=? (string-ref line pos) #\space)
             (++ column)]
            [(char=? (string-ref line pos) #\tab)
             (set! column (* tabsize (add1 (quotient column tabsize))))]
            [(char=? (string-ref line pos) #\page)
             (set! column 0)]
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
            (my-gvector-pop! indents)
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
     
     (while (< pos max)
       (void)
       (define pseudomatch (regexp-match-positions pseudoprog line pos))
       (cond [pseudomatch                                  ;; scan for tokens
              (set! start (car (first pseudomatch)))
              (set! end (cdr (first pseudomatch)))
              (define spos (list lnum start))
              (define epos (list lnum end))
              (set! pos end)
              (define token (substring line start end))
              (define initial (string-ref line start))
              (cond
                [(or (set-member? initial numchars)
                     (and (char=? initial #\.) (not (string=? token "."))))      ;; ordinary number
                 (yield NUMBER token spos epos line)]
                [(or (char=? initial #\return) (char=? initial #\newline))
                 (yield (if (> parenlev 0) NL NEWLINE)
                        token spos epos line)]
                [
                 ;   373                 elif initial == '#':
                 ;   374                     assert not token.endswith("\n")
                 ;   375                     yield (COMMENT, token, spos, epos, line)
                 ]
                [
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
                 ]
                [
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
                 ]
                [(set-member? initial namechars)                  ;; ordinary name
                 (yield NAME token spos epos line)]               
                [(char=? initial #\\)                             ;; continued stmt
                 (set! continued #t)]
                [else
                 (cond [(or (char=? initial #\() (char=? initial #\[) (char=? initial #\{))
                        (++ parenlev)]
                       [(or (char=? initial #\)) (char=? initial #\]) (char=? initial #\}))
                        (-- parenlev)])
                 (yield OP token spos epos line)])]
             [else
              (yield ERRORTOKEN
                     (string-ref line pos)
                     (list lnum pos)
                     (list lnum (+ pos 1))
                     line)]))
     
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