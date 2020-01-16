#lang racket

; tok ones, ki ustreza zaporedju samih enic (1 1 1 ...)
(define ones (cons 1 (lambda () ones)))

; tok naturals, ki ustreza zaporedju naravnih števil (1 2 3 4 ...)
(define naturals (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
                  (f 1)))

; tok fibs, ki ustreza zaporedju fibonaccijevih števil (1 1 2 3 5 ...)
(define fibs (letrec ([f (lambda (x y) (cons y (lambda () (f y (+ x y)))))])
                  (f 0 1)))

; first, ki sprejme število n in tok, ter vrne seznam prvih n števil iz toka.
(define (first n e)
  (if (= 0 n)
      null
      (cons (car e) (first (- n 1) ((cdr e))))))


; squares, ki sprejme tok, in vrne nov tok, ki vsebuje kvadrirane elemente prvega toka.
(define (squares tok)
  (letrec ([f (lambda (x) (cons (* x x) (lambda () (squares ((cdr tok))))))])
    (f (car tok))))

; 6, 7. my-delay, my-force. Funkciji za zakasnitev in sprožitev delujeta tako, da si funkcija za sprožitev pri prvem klicu zapomni rezultat, ob naslednjih
; pa vrne shranjeno vrednost. Popravite funkciji tako, da bo funkcija za sprožitev ob prvem in nato ob vsakem petem klicu ponovno izračunala in shranila rezultat.

; zakasnitev
(define (my-delay thunk) 
  (mcons 0 (mcons thunk thunk))) 

; sprožitev
(define (my-force prom)
  (if (= 0 (modulo (mcar prom) 5))
      (begin (set-mcar! prom (+ (mcar prom) 1))
             (set-mcdr! (mcdr prom) ((mcar (mcdr prom))))
             (mcdr (mcdr prom)))
      (begin (set-mcar! prom (+ (mcar prom) 1))
        (mcdr (mcdr prom)))))

;partitions, ki sprejme števili k in n, ter vrne število različnih načinov, na katere lahko n zapišemo kot vsoto k naravnih števil (naravna števila se v tem kontekstu začnejo z 1).
;(Če se dva zapisa razlikujeta samo v vrstnem redu elementov vsote, ju obravnavamo kot en sam zapis ------ p(k,n)=p(k,n-k)+p(k-1,n-1)

(define (partitions k n)
  (cond [(> k n) 0]
        [(= k n) 1]
        [(= k 1) 1]
        [#t (+ (partitions k (- n k)) (partitions (- k 1) (- n 1)))]))

#| TESTS |#
#|
(define-syntax StrNext
  (syntax-rules()
    [(StrNext e)
     ((cdr e))]))

(define-syntax StrVal
  (syntax-rules()
    [(StrVal e)
     (car e)]))

(define (StrNth e n)
  (if (= n 1)
         (car e)
         (StrNth ((cdr e)) (- n 1))))

(define (test name l1 l2)
  (letrec ([aux (λ (l1 l2 n) (
                        if (null? l1)
                           " OK"
                           (if (equal? (car l1) (car l2)) (aux (cdr l1) (cdr l2) (+ n 1)) (string-append " ERROR at " (~v n)))))])
    (string-append "test: " name (aux l1 l2 1))))

(test "ones" (list (StrVal ones) (StrVal (StrNext ones)) (first 3 ones)) '(1 1 (1 1 1)))

(test "naturals" (list (StrVal naturals) (StrVal (StrNext naturals)) (first 3 naturals)) '(1 2 (1 2 3)))

(test "fibs" (list (StrVal fibs) (StrVal (StrNext fibs)) (first 6 fibs)) '(1 1 (1 1 2 3 5 8)))

(test "first" (list (first 0 ones) (first 1 naturals) (first 5 naturals) (first 6 fibs)) '(() (1) (1 2 3 4 5) (1 1 2 3 5 8)))

(test "squares" (list (first 0 (squares ones)) (first 1 (squares ones)) (first 5 (squares naturals)) (first 5 (squares fibs))) '(() (1) (1 4 9 16 25) (1 1 4 9 25)))

(define f (my-delay (lambda () (begin (write "bla") 123))))
(my-force f)
(my-force f)
(my-force f)
(my-force f)
(my-force f)
(my-force f)
(my-force f)

(test "partitons" (list (partitions 3 2) (partitions 5 5) (partitions 1 123) (partitions 3 7)) '(0 1 1 4))
|#