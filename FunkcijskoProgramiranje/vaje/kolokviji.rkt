#lang racket

(define (vsota a b c) (+ a b c))

(define (stat f)
  (mcons f (mcons 0 -inf.0)))

(define (exec a s)
  (let ([r (apply (mcar a) s)])
    (begin (set-mcar! (mcdr a) (+ (mcar (mcdr a)) 1))
           (if (> r (mcdr (mcdr a)))
               (set-mcdr! (mcdr a) r)
               (void))
           r)))

(define b (stat vsota))

(exec b (list 1 2 3))

(let ([a 5]
      [b 6]
      [c 7])
  (begin (let ([c a])
           (set! a b)
           (set! b c))
         (let ([c b])
           (set! b c)
           (set! c c)))
  (list a b c))

#|5 6 7
6 5 7
6 5 7|#

(define-syntax-rule (swap a b) (let ([c a]) (set! a b) (set! b c)))
(define-syntax rotate (syntax-rules () [(rotate a b c) (begin (swap a b) (swap b c))]))
(let ([a 5] [b 6] [c 7]) (rotate a b c) (list a b c))

(define naravna (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))]) (f 1)))
(define (maptok f t)
  (letrec ([aux (lambda (f t)
                  (cons (f (car t)) (lambda () (aux f ((cdr t))))))])
    (aux f t)))

(define novi (maptok (lambda (x) (+ x 10)) naravna))

(define hello (lambda (x) (lambda (y) y)))

((hello #t) 3.14)

(define (foo a) (letrec ([b (lambda () (+ c 1))] [c (+ a 1)]) (+ a (b))))
(foo 5)

(define (first n e)
  (if (= 0 n)
      null
      (cons (car e) (first (- n 1) ((cdr e))))))

(define quad (letrec ([f (lambda (x) (cons (mcons x (* x x x x)) (lambda () (f (+ x 1)))))]) (f 5)))

(define (transform str)
  (let ([x (mcar (car str))]
        [y (mcdr (car str))])
    (begin (set-mcar! (car str) y)
           (set-mcdr! (car str) x)
           (cons (car str) (lambda () (transform ((cdr str))))))))

(define seznam
  (lambda (#:start [start 1] . sez)
    (letrec ([aux (lambda (x s)
                    (if (null? s)
                        null
                        (cons (expt (car s) x)
                              (aux (+ x 1) (cdr s)))))])
      (aux start sez))))

(define tok120
  (letrec([aux (lambda (x)
                 (cons x (lambda () (aux (modulo (+ x 1) 3)))))])
    (aux 1)))

(define (preskocni tok skoki)
  (letrec ([razsiri (lambda (t s)
                    (if (= s 0)
                        ((cdr t))
                        (razsiri ((cdr t)) (- s 1))))]
           [aux (lambda (t s)
                  (cons (car t) (lambda () (aux (razsiri t (car s)) ((cdr s))))))])
    (aux tok skoki)))

(define (prozi fn n)
  (letrec ([aux (lambda (f n s)
                  (if (= n 1)
                      (cons (force (delay (lambda () (f s)))) (lambda () (aux f 0 (+ s 1))))
                      (cons (delay (lambda () (f s))) (lambda () (aux f (- n 1) (+ s 1))))))])
    (aux fn n 1)))

(define elseznama
  (letrec ([aux (lambda (x)
                  (cons x (lambda () (aux (lambda (z) (x (cdr z)))))))])
    (aux (lambda (x) (car x)))))

(define (foldl/stream func acc stream n)
  (if (= n 0)
      acc
      (foldl/stream func (func (car stream) acc) ((cdr stream)) (- n 1))))
