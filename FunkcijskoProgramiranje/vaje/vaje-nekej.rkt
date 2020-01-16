#lang racket

(define (timed-add)
  (begin
    (sleep 2)
    (+ 123 123)))

(define dta (delay (timed-add)))

