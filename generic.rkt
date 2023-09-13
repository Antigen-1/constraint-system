#lang racket/base
(require racket/generic racket/contract)
(provide gen:field-instance field-instance? field-instance/c additive-identity multiplicative-identity additive-inverse multiplicative-inverse
         (contract-out (legal-field? (-> field-instance? any))
                       (add operator/c)
                       (mul operator/c)
                       (equal operator/c)
                       (sub operator/c)
                       (div operator/c)))

(define-generics field-instance
  (add field-instance another)
  (mul field-instance another)
  (additive-identity field-instance)
  (multiplicative-identity field-instance)
  (additive-inverse field-instance)
  (multiplicative-inverse field-instance)
  (equal field-instance another)

  #:defaults ((complex?
               (define (add f a) (+ f a))
               (define (mul f a) (* f a))
               (define (additive-identity f) 0.0)
               (define (multiplicative-identity f) 1.0)
               (define (additive-inverse f) (- f))
               (define (multiplicative-inverse f) (/ 1.0 f))
               (define (equal f a) (= f a)))))

;; derived functions
(define (sub instance1 instance2)
  (add instance1 (additive-inverse instance2)))
(define (div instance1 instance2)
  (define zero (additive-identity instance1))
  (if (equal instance2 zero)
      (raise (make-exn:fail:contract:divide-by-zero "Zero field instance is not allowed here." (current-continuation-marks)))
      (mul instance1 (multiplicative-inverse instance2))))

(define operator/c (-> field-instance? field-instance? any))

(define (legal-field? instance)
  (let ((zero (additive-identity instance))
        (one (multiplicative-identity instance)))
    (and
     ;; commutativity
     (equal (add instance one) (add one instance))
     (equal (mul instance one) (mul one instance))
     ;; associativity
     (equal (add instance (add instance one)) (add (add instance instance) one))
     (equal (mul instance (mul instance one)) (mul (mul instance instance) one))
     ;; identities
     (equal (add instance zero) instance)
     (equal (mul one instance) instance)
     ;; inverse
     (equal zero (sub instance instance))
     (equal one (div instance instance))
     ;; distributive property
     (equal (mul instance (add instance one)) (add (mul instance instance) (mul instance one))))))

(module* test racket/base
  (require (submod "..") rackunit)

  (struct complex (complex) #:methods gen:field-instance ((define (add f a) (complex (+ (complex-complex f) (complex-complex a))))
                                                          (define (mul f a) (complex (* (complex-complex f) (complex-complex a))))
                                                          (define (additive-identity f) (complex 0.0))
                                                          (define (multiplicative-identity f) (complex 1.0))
                                                          (define (additive-inverse f) (complex (- (complex-complex f))))
                                                          (define (multiplicative-inverse f) (complex (/ 1.0 (complex-complex f))))
                                                          (define (equal f a) (= (complex-complex f) (complex-complex a)))))

  (check-true (legal-field? (complex 10.0+7.0i)))
  (check-exn exn:fail:contract:divide-by-zero? (lambda () (legal-field? (complex 0.0))))
  (check-true (equal (complex 1.0) (div (complex 10.0) (complex 10.0))))
  (check-true (equal (complex 0.0) (sub (complex 10.0) (complex 10.0)))))
