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

(require "generic.rkt" "connector.rkt" "constraints.rkt" "macro.rkt")
(provide (all-from-out "generic.rkt" "connector.rkt" "constraints.rkt") define-constant define-probe)

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
    (let ((u (make-connector)) (v (make-connector)))
      (define-constant w (real 9.0))
      (define-constant x (real 5.0))
      (define-constant y (real 32.0))
      (multiplier c w u)
      (multiplier v x u)
      (adder v y f)))

  (define-probe C #:name "Celsius temp" #:printer (lambda (o) (display (real-num o))))
  (define-probe F #:name "Fahrenheit temp" #:printer (lambda (o) (display (real-num o))))
  (celsius-fahrenheit-converter C F)
  
  (set-value! C (real 25.0) 'user)
  (check-true (equal (value F) (real 77.0)))
  (check-exn exn:fail:contract? (lambda () (set-value! F (real 212.0) 'user)))
  (forget-value! C 'user)
  (set-value! F (real 212.0) 'user)
  (check-true (equal (value C) (real 100.0))))
