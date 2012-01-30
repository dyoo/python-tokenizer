#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam)

(provide while break continue)

;; The following is adapted from:
;;
;; http://matt.might.net/articles/implementing-exceptions/
;;
;; with a few adjustments so that break and continue are
;; hygienic.
(define-syntax-parameter break
  (lambda (stx) 
    (raise-syntax-error #f "Used outside the context of a while" stx)))

(define-syntax-parameter continue
  (lambda (stx) 
    (raise-syntax-error #f "Used outside the context of a while" stx)))

(define-syntax (while stx)
  (syntax-case stx ()
    [(_)
     (raise-syntax-error #f "missing test and body" stx)]
    [(_ cond body ...)
     (begin
       ;; When the body is empty, let's raise a syntactic error.
       (when (null? (syntax->list #'(body ...)))
         (raise-syntax-error #f "empty body" stx))
     #'(call/ec (λ (fresh-break)
                  (syntax-parameterize ([break (make-rename-transformer #'fresh-break)])
                    (letrec ([loop (lambda ()
                                     (when cond
                                       (call/ec (lambda (fresh-continue)
                                                  (syntax-parameterize ([continue
                                                                         (make-rename-transformer #'fresh-continue)])
                                                    (begin body ...))))
                                       (loop)))])
                      
                      (loop))))))]))
