#lang racket
(define (potenca x n)
  (if  (= n 0) 1 (* x (potenca x (- n 1)))))

(define (gcd a b)
  (if (= b 0) a (gcd b (modulo a b))))

(define (fib n)
  (cond [(= n 1) 1]
        [(= n 2) 1]
        [else ( + (fib (- n 1)) (fib (- n 2)))]))

(define (reverse l)
  (if (null? l)
      null
      (append (reverse (cdr l)) (list (car l)))))

(define (remove x l)
  (if (null? l)
      null
      (if (= x (car l)) (remove x (cdr l)) (append (list (car l)) (remove x (cdr l))))))

(define (map f l)
  (if (null? l)
      null
      (append (list (f (car l))) (map f (cdr l)))))

(define (filter f l)
  (if (null? l)
      null
      (if (f (first l)) (append (list (first l)) (filter f (rest l))) (filter f (rest l)))))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      null
      (append ( list (list* (car l1) (car l2))) (zip (cdr l1) (cdr l2)))))

(define (range a b s)
  (if ( > a b)
      null
      (append (list a) (range (+ a s) b s))))

(define (is-palindrome l)
  (letrec ([aux (λ (l1 l2) (
                        if (null? l1) #t (and (= (car l1) (car l2) ) (aux (cdr l1) (cdr l2)))))])
    (aux l (reverse l))))

#| TESTS |#
#|
(define (test name l1 l2)
  (letrec ([aux (λ (l1 l2 n) (
                        if (null? l1)
                           " OK"
                           (if (equal? (car l1) (car l2)) (aux (cdr l1) (cdr l2) (+ n 1)) (string-append " ERROR at " (~v n)))))])
    (string-append "test: " name (aux l1 l2 1))))

(test "potenca" (list (potenca 1 0) (potenca 1 5) (potenca 5 0) (potenca 5 3) (potenca 2 8)) '(1 1 1 125 256))

(test "gcd" (list (gcd 5 12) (gcd 8 16) (gcd 1 9485) (gcd 15 10) (gcd 125 5)) '(1 8 1 5 5))

(test "fib" (list (fib 1) (fib 2) (fib 3) (fib 5) (fib 10)) '(1 1 2 5 55))

(test "reverse" (list (reverse '()) (reverse '(1 2 3)) (reverse '(1))) (list '() '(3 2 1) '(1)))

(test "remove" (list (remove 5 '()) (remove 2 '(1 2 3)) (remove 4 '(1 2 3)) (remove 1 '(1 2 3 1 1 4))) (list '() '(1 3) '(1 2 3) '(2 3 4)))

(test "map"
      (list (map (lambda (a) (* a 2)) (list 1 2 3)) (map (lambda (a) (* a 2)) '()) (map (lambda (a) (modulo a 3)) (list 1 2 3 4 5 6 7 8)) )
      (list '(2 4 6) '() '(1 2 0 1 2 0 1 2)))

(test "filter"
      (list (filter (lambda (a) (= a 2)) (list 1 2 3)) (filter (lambda (a) (= a 2)) '()) (filter (lambda (a) (= (modulo a 3) 0)) (list 1 2 3 4 5 6 7 8 9)) )
      (list '(2) '() '(3 6 9)))

(test "zip"
      (list (zip (list 1 2 3) (list 4 5 6)) (zip (list 1 2 3) (list "1" "2" "3" "4")) (zip '() (list 4 5 6)))
      (list '((1 . 4) (2 . 5) (3 . 6)) '((1 . "1") (2 . "2") (3 . "3")) '()))

(test "range"
      (list (range 0 1 0.5) (range 1 5 1) (range 1 0 5))
      (list '(0 0.5 1.0) '(1 2 3 4 5) '()))

(test "is-palindrome"
      (list (is-palindrome '()) (is-palindrome (list 2 3 5 1 6 1 5 3 2)) (is-palindrome (list 2 3 6 1 6 1 5 3 2)) (is-palindrome (list 2)))
      (list #t #t #f #t))
|#