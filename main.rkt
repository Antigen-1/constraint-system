#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require "generic.rkt" "connector.rkt" "constraints.rkt")
(provide (all-from-out "generic.rkt" "connector.rkt" "constraints.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (struct real (num) #:methods gen:field-instance ((define (add f a) (real (+ (real-num f) (real-num a))))
                                                   (define (mul f a) (real (* (real-num f) (real-num a))))
                                                   (define (multiplicative-identity f) (real 1.0))
                                                   (define (additive-identity f) (real 0.0))
                                                   (define (multiplicative-inverse f) (real (/ 1.0 (real-num f))))
                                                   (define (additive-inverse f) (real (- (real-num f))))
                                                   (define (equal f a) (= (real-num f) (real-num a)))))

  (define (celsius-fahrenheit-converter c f)
    (let ((u (make-connector))
          (v (make-connector))
          (w (make-connector))
          (x (make-connector))
          (y (make-connector)))
      (multiplier c w u)
      (multiplier v x u)
      (adder v y f)
      (constant (real 9.0) w)
      (constant (real 5.0) x)
      (constant (real 32.0) y)))

  (define C (make-connector))
  (define F (make-connector))
  (celsius-fahrenheit-converter C F)
  (probe "Celsius temp" C #:printer (lambda (o) (display (real-num o))))
  (probe "Fahrenheit temp" F #:printer (lambda (o) (display (real-num o))))
  
  (set-value! C (real 25.0) 'user)
  (check-true (equal (value F) (real 77.0)))
  (check-exn exn:fail:contract? (lambda () (set-value! F (real 212.0) 'user)))
  (forget-value! C 'user)
  (set-value! F (real 212.0) 'user)
  (check-true (equal (value C) (real 100.0))))
