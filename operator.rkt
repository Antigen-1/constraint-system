#lang racket/base
(require "generic.rkt" "connector.rkt" "constraints.rkt" racket/contract)
(provide (contract-out (c+ operator/c) (c* operator/c) (c/ operator/c) (cv (-> field-instance? any))))

;; expression-oriented format
;; there is no operator for probes because `probe` has keyword arguments
(define ((make-operator constraint) c1 c2)
  (let ((temp (make-connector)))
    (constraint c1 c2 temp)
    temp))
(define operator/c (-> any/c any/c any))
(define c+ (make-operator adder))
(define c* (make-operator multiplier))
(define c/ (make-operator divider))

(define (cv val) (let ((temp (make-connector))) (constant val temp) temp))
