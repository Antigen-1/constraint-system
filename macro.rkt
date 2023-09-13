#lang racket/base
(require "connector.rkt" "constraints.rkt" (for-syntax syntax/parse racket/base racket/syntax))
(provide define-constant define-probe)

(define-syntax (define-constant stx)
  (syntax-parse stx
    ((_ var:id value:expr)
     (with-syntax ((temp (generate-temporary #'var)) (temp-setter (generate-temporary 'setter)))
       #'(define var
           (let ((temp (make-connector)))
             (set-value! temp value 'temp-setter)
             temp))))))

(define-syntax (define-probe stx)
  (define-syntax-class name
    #:description "name"
    (pattern n:string)
    (pattern n:id))
  (define-splicing-syntax-class probe
    #:description "probe"
    (pattern (~seq var:id)
             #:with variable #'var
             #:with name #''var
             #:with printer #'display)
    (pattern (~seq var:id #:name n:name)
             #:with variable #'var
             #:with name #'n
             #:with printer #'display)
    (pattern (~seq var:id (~or* (~seq #:name n:name #:printer p:expr) (~seq #:printer p:expr #:name n:name)))
             #:with variable #'var
             #:with name #'n
             #:with printer #'p))
  (syntax-parse stx
    ((_ probe-info:probe)
     (with-syntax ((temp (generate-temporary #'probe-info.variable)))
       #'(define probe-info.variable
           (let ((temp (make-connector)))
             (probe probe-info.name temp #:printer probe-info.printer)
             temp))))))
