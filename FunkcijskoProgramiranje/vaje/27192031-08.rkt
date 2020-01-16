#lang racket
(struct true () #:transparent)
(struct false () #:transparent)
(struct zz (n) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct ~ (e1) #:transparent)
(struct is-zz? (e1) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)

(define-syntax-rule (ifte c then e1 else e2)
  (if-then-else c e1 e2))

(define-syntax-rule (geq? e1 e2)
  (leq? e2 e1))

(define (fri expr)
  (cond [(true? expr) (true)]
        [(false? expr) (false)]
        [(zz? expr) (if (integer? (zz-n expr)) expr (raise "error zz"))]
        [(add? expr)
         (let ([r1 (fri (add-e1 expr))]
               [r2 (fri (add-e2 expr))])
           (cond
                [(and (or (true? r1) (false? r1)) (or (true? r2) (false? r2)))
                          (if (and (false? r1) (false? r2)) (false) (true))]
                [(and (zz? r1) (zz? r2)) (zz (+ (zz-n r1) (zz-n r2)))]
                [#t (raise "error add")]))]
        [(mul? expr)
         (let ([r1 (fri (mul-e1 expr))]
               [r2 (fri (mul-e2 expr))])
           (cond
                [(and (or (true? r1) (false? r1)) (or (true? r2) (false? r2)))
                          (if (and (true? r1) (true? r2)) (true) (false))]
                [(and (zz? r1) (zz? r2)) (zz (* (zz-n r1) (zz-n r2)))]
                [#t (raise "error mul")]))]
        [(leq?? expr)
         (let ([r1 (fri (leq?-e1 expr))]
               [r2 (fri (leq?-e2 expr))])
           (cond
                [(and (or (true? r1) (false? r1)) (or (true? r2) (false? r2)))
                          (if (implies (true? r1) (true? r2)) (true) (false))]
                [(and (zz? r1) (zz? r2)) (if (<= (zz-n r1) (zz-n r2)) (true) (false))]
                [#t (raise "error leq?")]))]
        [(~? expr)
         (let ([e1 (fri (~-e1 expr))])
           (cond [(true? e1) (false)]
                 [(false? e1) (true)]
                 [(zz? e1) (zz (- (zz-n e1)))]
                 [#t (raise "error ~")]))]
        [(is-zz?? expr) (if (zz? (fri (is-zz?-e1 expr))) (true) (false))]
        [(if-then-else? expr) (if (true? (fri (if-then-else-condition expr)))
                              (fri (if-then-else-e1 expr))
                              (fri (if-then-else-e2 expr)))]
        ))

#|(equal? (fri (zz 5)) (zz 5))
(equal? (fri (false)) (false))
(equal? (fri (true)) (true))
(equal? (list (fri (add (false) (false))) (fri (add (true) (false))) (fri (add (false) (true))) (fri (add (true) (true)))) (list (false) (true) (true) (true)))
(equal? (list (fri (add (zz 2) (zz 3))) (fri (add (zz 5) (zz -5))) (fri (add (zz 5) (zz -10)))) (list (zz 5) (zz 0) (zz -5)))
(equal? (list (fri (mul (false) (false))) (fri (mul (true) (false))) (fri (mul (false) (true))) (fri (mul (true) (true)))) (list (false) (false) (false) (true)))
(equal? (list (fri (mul (zz 2) (zz 3))) (fri (mul (zz 5) (zz -5))) (fri (mul (zz 5) (zz -10)))) (list (zz 6) (zz -25) (zz -50)))
(equal? (list (fri (leq? (false) (false))) (fri (leq? (true) (false))) (fri (leq? (false) (true))) (fri (leq? (true) (true)))) (list (true) (false) (true) (true)))
(equal? (list (fri (leq? (zz 2) (zz 3))) (fri (leq? (zz 5) (zz -5))) (fri (leq? (zz 5) (zz 5)))) (list (true) (false) (true)))
(equal? (list (fri (~ (true))) (fri (~ (false)))) (list (false) (true)))
(equal? (list (fri (~ (zz 5))) (fri (~ (zz 0))) (fri (~ (zz -2)))) (list (zz -5) (zz 0) (zz 2)))
(equal? (list (fri (if-then-else (true) (zz 5) (add (zz 2) (zz "a")))) (fri (if-then-else (false) (zz 5) (add (zz 2) (zz 13))))) (list (zz 5) (zz 15)))
(equal? (list (fri (is-zz? (zz 5))) (fri (is-zz? (true)))) (list (true) (false)))
(equal? (ifte (true) then (add (zz 1) (zz 1)) else (zz 3)) (if-then-else (true) (add (zz 1) (zz 1)) (zz 3)))
(equal? (geq? (add (zz 1) (zz 1)) (zz 4)) (leq? (zz 4) (add (zz 1) (zz 1))))|#
