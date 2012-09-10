#lang setup/infotab
(define name "python-tokenizer: a translation of Python's @tt{tokenize.py} library for Racket")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.1.1")
(define version "1.1")
(define repositories '("4.x"))
(define scribblings '(("manual.scrbl")))
(define primary-file "main.rkt")
(define blurb 
  '("python-tokenizer: a translation of Python's @tt{tokenize.py} library for Racket"))
(define release-notes
  '((p "Bug fix.  Corrected an infinite-loop bug due to mis-typing a paren.  Thanks to Joe Politz for the bug report!")))