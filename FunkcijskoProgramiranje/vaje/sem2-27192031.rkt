#lang racket

#| V seminarski nalogi sem implementiral vse podatkovne tipe, konstrukte za nadzor toka, funkcije (navadne in rekurzivne), procedure in makre.
   Optimiziral sem tudi funkcijsko ovojnico ter implementirane konstrukte pretestiral. |#

#| Strukture za podatkovne tipe |#
(struct true () #:transparent)
(struct false () #:transparent)
(struct zz (n) #:transparent)
(struct qq (e1 e2) #:transparent)
(struct cc (e1 e2) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)

#| Strukture za nadzor toka |#
(struct if-then-else (condition e1 e2) #:transparent)
(struct is-zz? (e1) #:transparent)
(struct is-qq? (e1) #:transparent)
(struct is-cc? (e1) #:transparent)
(struct is-bool? (e1) #:transparent)
(struct is-seq? (e1) #:transparent)
(struct is-proper-seq? (e1) #:transparent)
(struct is-empty? (e1) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct rounding (e1) #:transparent)
(struct =? (e1 e2) #:transparent)
(struct left (e1) #:transparent)
(struct right (e1) #:transparent)
(struct ~ (e1) #:transparent)
(struct all? (e1) #:transparent)
(struct any? (e1) #:transparent)

#| Strukture za spremenljivke |#
(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

#| Strukture za funkcije |#
(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

#| Dodatne strukture za debugiranje funkcijskih ovojnic |#
;(struct debug-env ()) ; s pomocjo te strukture izpisemo funkcijsko ovojnico

#| Makri |#
(define (numerator e1)
  (left e1))

(define (denominator e1)
  (right e1))

(define (re e1)
  (left e1))

(define (im e1)
  (right e1))

(define (gt? e1 e2)
  (~ (leq? e1 e2)))

(define (inv e1)
    (vars "__r__" e1 ; izracun prejetega izraza, ki je nato shranjen v __r__
          (if-then-else (is-zz? (valof "__r__"))
                        (qq (zz 1) (valof "__r__"))
          (if-then-else (is-qq? (valof "__r__"))
                        (qq (right (valof "__r__")) (left (valof "__r__"))) ; obrnemo ulomek
          (if-then-else (is-cc? (valof "__r__"))
                        (vars (list "__a__" "__b__") (list (left (valof "__r__")) (right (valof "__r__")))  
                              (vars (list "__a1__" "__a2__" "__b1__" "__b2__" "s") ; razpisemo komponente in izracunamo po formuli konjugirana/norma
                                    (list (left (valof "__a__")) (right (valof "__a__"))
                                          (left (valof "__b__")) (right (valof "__b__"))
                                          (add (mul (valof "__a__") (valof "__a__"))
                                               (mul (valof "__b__") (valof "__b__"))))
                                    (cc (qq (mul (valof "__a1__") (right (valof "s")))
                                            (mul (valof "__a2__") (left (valof "s"))))
                                        (~ (qq (mul (valof "__b1__") (right (valof "s")))
                                               (mul (valof "__b2__") (left (valof "s"))))))))
          #| Seq |#           (call (fun "__tailrec__" (list "__seq__" "__acc__") ; repna rekurzija za obrnit seznam
                                         (if-then-else (is-empty? (valof "__seq__"))
                                                       (valof "__acc__")
                                                       (call (valof "__tailrec__")
                                                             (list (right (valof "__seq__"))
                                                                   (.. (left (valof "__seq__")) (valof "__acc__"))))))
                                    (list (valof "__r__") (empty))))))))

; klic funkcije map narejene v FR-ju
(define (mapping f seq)
  (call (fun "__map__" (list "__fun_map__" "__seq_map__")
             (if-then-else (is-empty? (valof "__seq_map__"))
                           (empty)
                           (.. (call (valof "__fun_map__") (list (left (valof "__seq_map__"))))
                               (call (valof "__map__") (list (valof "__fun_map__") (right (valof "__seq_map__")))))))
        (list f seq)))

; klic funkcije filtering narejene v FR-ju
(define (filtering f seq)
  (call (fun "__filter__" (list "__fun_filter__" "__seq_filter__")
             (if-then-else (is-empty? (valof "__seq_filter__"))
                           (empty)
                           (if-then-else (call (valof "__fun_filter__") (list (left (valof "__seq_filter__"))))
                                         (.. (left (valof "__seq_filter__"))
                                             (call (valof "__filter__") (list (valof "__fun_filter__") (right (valof "__seq_filter__")))))
                                         (call (valof "__filter__") (list (valof "__fun_filter__") (right (valof "__seq_filter__")))))))
        (list f seq)))

; klic funkcije folding narejene v FR-ju
(define (folding f init seq)
  (call (fun "__fold__" (list "__fun_fold__" "__init_fold__" "__seq_fold__")
             (if-then-else (is-empty? (valof "__seq_fold__"))
                           (valof "__init_fold__")
                           (call (valof "__fold__")
                                 (list f
                                       (call (valof "__fun_fold__") (list (left (valof "__seq_fold__")) (valof "__init_fold__")))
                                       (right (valof "__seq_fold__"))))))
        (list f init seq)))

#| Pomozne funkcije |#
; Preveri, ce je zz stevilo v racketu in v primeru pravilnosti ta izraz vrne
(define (check-zz expr env)
    (if (and (zz? expr) (integer? (zz-n expr)))
        expr
        (error "Error zz: not an integer.")))

; Preveri, ce je qq pravilno sestavljeno in v primeru pravilnosti ta izraz okrajsa ter vrne
(define (check-qq expr env)
  (if (qq? expr)
      (letrec
          ([z1 (zz-n (fri (qq-e1 expr) env))]
           [z2 (zz-n (fri (qq-e2 expr) env))]
           [our-gcd (lambda (a b) (if (= b 0) a (our-gcd b (modulo a b))))] ; implementacija gcd-ja, s pomocjo katere pravilno postavimo minuse
           [gc (our-gcd z1 z2)])
        (cond [(= z2 0) (error "Error qq: division by zero.")]
              [(= z1 0) (qq (zz 0) (zz 1))]
              [#t (qq (zz (/ z1 gc)) (zz (/ z2 gc)))]))
      (error "Error qq: wrong structure.")))

; Preveri, ce je cc pravilno sestavljeno in v primeru pravilnosti ta izraz vrne
(define (check-cc expr env)
  (if (cc? expr)
      (let ([r1 (fri (cc-e1 expr) env)]
            [r2 (fri (cc-e2 expr) env)])
        (cc (check-qq r1 env) (check-qq r2 env)))
      (error "Error cc: wrong structure.")))

; Sestavi list parov spremenljivk in vrednosti rekurzivno (pomozna funkcija za vars)
(define (create-list ss es env)
  (if (and (not (null? ss)) (not (null? es)))
      (cons (cons (car ss) (if (closure? (car es))
                               (car es)
                               (fri (car es) env)))
            (create-list (cdr ss) (cdr es) env))
      null))

; Vrne okolje za proceduro, glede na to ali je anonimna ali ne
(define (return-env-proc r env)
  (if (equal? "" (proc-name (call-e r)))
      env
      (cons (cons (proc-name (call-e r)) (call-e r))
            env)))

; Vrne okolje, ob definiciji funkcije
(define (trim-env env expr)
  (letrec ([aux (lambda (e) ; pomozna funkcija, ki gre cez izraz in vrne seznam spremenljivk, ki se v njem pojavijo
                  (cond [(qq? e) (append (aux (qq-e1 e)) (aux (qq-e2 e)))] ; za vecino struktur le zdruzi seznama podizrazov
                        [(cc? e) (append (aux (cc-e1 e)) (aux (cc-e2 e)))]
                        [(..? e) (append (aux (..-e1 e)) (aux (..-e2 e)))]
                        [(cc? e) (append (aux (cc-e1 e)) (aux (cc-e2 e)))]
                        [(if-then-else? e) (append (aux (if-then-else-condition e)) (aux (if-then-else-e1 e)) (aux (if-then-else-e2 e)))]
                        [(is-zz?? e) (aux (is-zz?-e1 e))]
                        [(is-qq?? e) (aux (is-qq?-e1 e))]
                        [(is-cc?? e) (aux (is-cc?-e1 e))]
                        [(is-bool?? e) (aux (is-bool?-e1 e))]
                        [(is-seq?? e) (aux (is-seq?-e1 e))]
                        [(is-proper-seq?? e) (aux (is-proper-seq?-e1 e))]
                        [(is-empty?? e) (aux (is-empty?-e1 e))]
                        [(add? e) (append (aux (add-e1 e)) (aux (add-e2 e)))]
                        [(mul? e) (append (aux (mul-e1 e)) (aux (mul-e2 e)))]
                        [(leq?? e) (append (aux (leq?-e1 e)) (aux (leq?-e2 e)))]
                        [(rounding? e) (aux (rounding-e1 e))]
                        [(=?? e) (append (aux (=?-e1 e)) (aux (=?-e2 e)))]
                        [(left? e) (aux (left-e1 e))]
                        [(right? e) (aux (right-e1 e))]
                        [(~? e) (aux (~-e1 e))]
                        [(any?? e) (aux (any?-e1 e))]
                        [(all?? e) (aux (all?-e1 e))]
                        [(vars? e) (append (foldl (lambda (a b) (append (aux a) b)) ; doda imena spremenljivk, ki se pojavijo pri izracunu e1
                                                  null
                                                  (if (list? (vars-e1 e)) (vars-e1 e) (list (vars-e1 e))))
                                           (let ([r (aux (vars-e2 e))]) ; poleg tega pa doda se presek imen spremenljivk v izrazu e2 ter s
                                             (remove* (if (list? (vars-s e))
                                                          (vars-s e)
                                                          (list (vars-s e)))
                                                      (if (list? r) r (list r)))))]
                        [(valof? e) (list (valof-s e))] ; vrne ime spremenljivke
                        [(proc? e) (remove (proc-name e) (aux (proc-body e)))] ; vrne imena spremenljivk brez imena procedure
                        [(fun? e) (remove* (cons (fun-name e) (fun-farg e)) (aux (fun-body e)))] ; podobno kot procedure, a se brez imen argumentov
                        [(call? e) (append (aux (call-e e)) ; vrne imena spremenljivk v podizrazu ter imena, ki se pojavijo pri izracunu argumentov
                                           (foldl (lambda (a b) (append (aux a) b))
                                                  null
                                                  (call-args e)))]
                        [#t null]))]
           [envlist (remove* (fun-farg expr) (remove-duplicates (aux expr)))]) ; poklicemo pomozno funkcijo, odstranimo podvojene ter imena argumentov
    (remove-duplicates (filter (lambda (x) (member (car x) envlist)) env) ; iz okolja vzememo potrebne pare
                       #:key car)))

#| Main function |#

(define (fri expr env)
  (cond ; Podatkovni tipi in njihova preverba
        [(true? expr) (true)]
        [(false? expr) (false)]
        [(zz? expr) (check-zz expr env)]
        [(qq? expr) (check-qq expr env)]
        [(cc? expr) (check-cc expr env)]
        [(..? expr) (.. (fri (..-e1 expr) env) (fri (..-e2 expr) env))]
        [(empty? expr) (empty)]
        [(is-zz?? expr) (if (zz? (fri (is-zz?-e1 expr) env)) (true) (false))] ; podobno za vse, izracunamo e1 in preverimo ce je pravega tipa
        [(is-qq?? expr) (if (qq? (fri (is-qq?-e1 expr) env)) (true) (false))]
        [(is-cc?? expr) (if (cc? (fri (is-cc?-e1 expr) env)) (true) (false))]
        [(is-bool?? expr) (if (or (true? (fri (is-bool?-e1 expr) env))
                                  (false? (fri (is-bool?-e1 expr) env)))
                              (true)
                              (false))]
        [(is-seq?? expr) (let ([r (fri (is-seq?-e1 expr) env)])
                           (if (or (empty? r) (..? r))
                               (true)
                               (false)))]
        [(is-proper-seq?? expr) (letrec ([res (fri (is-proper-seq?-e1 expr) env)] ; izracunamo izraz in ga preverjamo do konca, ce je empty potem
                                         [aux (lambda (s)                         ; vrnemo true cene false
                                                (if (empty? s)
                                                    (true)
                                                    (if (..? s)
                                                        (aux (..-e2 s))
                                                        (false))))])
                                  (aux res))]
        [(is-empty?? expr) (if (empty? (fri (is-empty?-e1 expr) env)) (true) (false))]
        ; Nadzor toka
        [(if-then-else? expr) (if (false? (fri (if-then-else-condition expr) env)) ; preverimo, ce je evalviran izraz false in glede na to evalviramo previlno vejo
                              (fri (if-then-else-e2 expr) env)
                              (fri (if-then-else-e1 expr) env))]
        [(add? expr) (letrec ([r1 (fri (add-e1 expr) env)] ; evalviramo oba podizraza in jih nato sestejemo glede na podatkovni tip
                              [r2 (fri (add-e2 expr) env)]) ; pri tem si pomagamo z sestevanjem manjsih izrazov s pomocjo interpreterja
                       (cond [(and (or (true? r1) (false? r1)) (or (true? r2) (false? r2)))
                              (if (and (false? r1) (false? r2)) (false) (true))]
                             [(and (zz? r1) (zz? r2)) (zz (+ (zz-n r1) (zz-n r2)))]
                             [(and (qq? r1) (zz? r2))
                              (fri (qq (zz (+ (zz-n (qq-e1 r1))
                                              (* (zz-n (qq-e2 r1)) (zz-n r2))))
                                       (qq-e2 r1)) null)]
                             [(and (qq? r2) (zz? r1))
                              (fri (qq (zz (+ (zz-n (qq-e1 r2))
                                              (* (zz-n (qq-e2 r2)) (zz-n r1))))
                                       (qq-e2 r2)) null)]
                             [(and (qq? r1) (qq? r2))
                              (fri (qq (zz (+ (* (zz-n (qq-e1 r1)) (zz-n (qq-e2 r2)))
                                              (* (zz-n (qq-e1 r2)) (zz-n (qq-e2 r1)))))
                                       (zz (* (zz-n (qq-e2 r1)) (zz-n (qq-e2 r2))))) null)]
                             [(and (cc? r1) (or (zz? r2) (qq? r2))) (cc (fri (add (cc-e1 r1) r2) null)
                                                                        (cc-e2 r1))]
                             [(and (cc? r2) (or (zz? r1) (qq? r1))) (cc (fri (add (cc-e1 r2) r1) null)
                                                                        (cc-e2 r2))]
                             [(and (cc? r2) (cc? r1)) (cc (fri (add (cc-e1 r2) (cc-e1 r1)) null)
                                                          (fri (add (cc-e2 r2) (cc-e2 r1)) null))]
                             [(and (or (..? r1) (empty? r1))
                                   (or (..? r2) (empty? r2)))
                              (letrec ([aux (lambda (a b) ; gremo cez e1 zaporedij in ko pridemo do empty ga zamenjamo z drugim
                                              (cond [(empty? a) b]
                                                    [(..? a) (.. (..-e1 a) (aux (..-e2 a) b))]
                                                    [#t (error "Error add: cannot add this sequences.")]))])
                                (aux r1 r2))]
                             [#t (error "Error add: wrong call.")]))]
         [(mul? expr) (letrec ([r1 (fri (mul-e1 expr) env)] ; podobno kot add, a tukaj izraze mnozimo namesto sestejemo, pri tem si pomagamo
                               [r2 (fri (mul-e2 expr) env)]) ; z mnozenjem manjsih izrazov
                       (cond [(and (or (true? r1) (false? r1)) (or (true? r2) (false? r2)))
                              (if (and (true? r1) (true? r2)) (true) (false))]
                             [(and (zz? r1) (zz? r2)) (zz (* (zz-n r1) (zz-n r2)))]
                             [(and (qq? r1) (zz? r2)) (fri (qq (zz (* (zz-n (qq-e1 r1)) (zz-n r2)))
                                                               (qq-e2 r1)) null)]
                             [(and (qq? r2) (zz? r1)) (fri (qq (zz (* (zz-n (qq-e1 r2)) (zz-n r1)))
                                                               (qq-e2 r2)) null)]
                             [(and (qq? r1) (qq? r2)) (fri (qq (zz (* (zz-n (qq-e1 r1)) (zz-n (qq-e1 r2))))
                                                               (zz (* (zz-n (qq-e2 r1)) (zz-n (qq-e2 r2))))) null)]
                             [(and (cc? r1) (or (zz? r2) (qq? r2))) (cc (fri (mul (cc-e1 r1) r2) null)
                                                                        (fri (mul (cc-e2 r1) r2) null))]
                             [(and (cc? r2) (or (zz? r1) (qq? r1))) (cc (fri (mul (cc-e1 r2) r1) null)
                                                                        (fri (mul (cc-e2 r2) r1) null))]
                             [(and (cc? r1) (cc? r2)) (cc (fri (add (mul (cc-e1 r1) (cc-e1 r2))
                                                                    (~ (mul (cc-e2 r1) (cc-e2 r2)))) null)
                                                          (fri (add (mul (cc-e1 r1) (cc-e2 r2))
                                                                    (mul (cc-e1 r2) (cc-e2 r1))) null))]
                             [#t (error "Error mul: wrong call.")]))]
         [(leq?? expr)
          (let ([r1 (fri (leq?-e1 expr) env)] ; evalviramo podizraza in nato preverimo implikacijo oz ali je vrednost r1 <= r2
                [r2 (fri (leq?-e2 expr) env)])
            (cond [(and (or (true? r1) (false? r1)) (or (true? r2) (false? r2)))
                   (if (implies (true? r1) (true? r2)) (true) (false))]
                  [(and (zz? r1) (zz? r2)) (if (<= (zz-n r1) (zz-n r2)) (true) (false))]
                  [(and (qq? r1) (qq? r2)) ; damo na skupni imenovalec in preverimo stevce
                   (let ([q1 (fri (mul r1 (qq-e2 r2)) env)]
                         [q2 (fri (mul r2 (qq-e2 r1)) env)])
                     (if (<= (zz-n (qq-e1 q1)) (zz-n (qq-e1 q2))) (true) (false)))]
                  [(and (qq? r1) (zz? r2))
                   (let ([z (* (zz-n r2) (zz-n (qq-e2 r1)))])
                     (if (<= (zz-n (qq-e1 r1)) z) (true) (false)))]
                  [(and (zz? r1) (qq? r2))
                   (let ([z (* (zz-n r1) (zz-n (qq-e2 r2)))])
                     (if (<= z (zz-n (qq-e1 r2))) (true) (false)))]
                  [(and (or (..? r1) (empty? r1))
                        (or (..? r2) (empty? r2)))
                   (letrec ([aux (lambda (s1 s2) ; gremo cez zaporedje dokler se prvo oz drugo zaporedje ne konca
                                   (cond [(and (..? s1) (..? s2)) (aux (..-e2 s1) (..-e2 s2))]
                                         [(not (..? s1)) (true)]
                                         [#t (false)]))])
                     (aux r1 r2))]
                  [#t (error "Error leq?: wrong call.")]))]
         [(rounding? expr)
          (let ([r (fri (rounding-e1 expr) env)]) ; evalviramo in zaokrozimo
            (cond [(zz? r) r]
                  [(qq? r) (zz (round (/ (zz-n (qq-e1 r)) (zz-n (qq-e2 r)))))]
                  [#t (error "Error rounding: cannot round this expression.")]))]
         [(=?? expr) (if (equal? (fri (=?-e1 expr) env) (fri (=?-e2 expr) env)) (true) (false))]
         [(left? expr)
          (let ([e1 (fri (left-e1 expr) env)]) ; evalviramo in vrnemo levo stran tipa
            (cond [(qq? e1) (qq-e1 e1)]
                  [(cc? e1) (cc-e1 e1)]
                  [(..? e1) (..-e1 e1)]
                  [#t (error "Error left: wrong call.")]))]
         [(right? expr)
          (let ([e2 (fri (right-e1 expr) env)]) ; evalviramo in vrnemo desno stran tipa
            (cond [(qq? e2) (qq-e2 e2)]
                  [(cc? e2) (cc-e2 e2)]
                  [(..? e2) (..-e2 e2)]
                  [#t (error "Error right: wrong call.")]))]
         [(~? expr)
          (let ([e1 (fri (~-e1 expr) env)]) ; negiramo oz konjugiramo evalviran izraz
            (cond [(true? e1) (false)]
                  [(false? e1) (true)]
                  [(zz? e1) (zz (- (zz-n e1)))]
                  [(qq? e1) (qq (fri (~ (qq-e1 e1)) null) (qq-e2 e1))]
                  [(cc? e1) (cc (cc-e1 e1) (fri (~ (cc-e2 e1)) null))]
                  [#t (error "Error ~: wrong call.")]))]
         [(all?? expr)
          (letrec ([e1 (fri (all?-e1 expr) env)] ; evalviramo izraz in nato preverimo da zaporedje nima falsa
                   [aux (lambda (e)
                          (cond [(empty? e) (true)]
                                [(not (..? e)) (error "Error all: wrong call.")]
                                [(false? (..-e1 e)) (false)]
                                [#t (aux (..-e2 e))]))])
            (aux e1))]
         [(any?? expr)
          (letrec ([e1 (fri (any?-e1 expr) env)]  ; evalviramo izraz in nato preverimo ce vsi elementi niso false
                   [aux (lambda (e)
                          (cond [(empty? e) (false)]
                                [(not (..? e)) (error "Error any: wrong call.")]
                                [(false? (..-e1 e)) (aux (..-e2 e))]
                                [#t (true)]))])
            (aux e1))]
         ; Spremenljivke
         [(vars? expr) ; evalviramo izraze in jih dodamo k okolju ter evalviramo izraz e2
          (cond [(list? (vars-s expr)) (fri (vars-e2 expr) (append (create-list (vars-s expr) (vars-e1 expr) env)
                                                                   env))] ; sestavimo seznam s pomocjo create-list
                [#t (fri (vars-e2 expr) (cons (cons (vars-s expr)
                                                    (fri (vars-e1 expr) env)) env))])]
         [(valof? expr) ; gremo rekurzivno cez seznam in poiscemo prvo pojavitev spremenljivke z imenom shranjenim v s
          (letrec ([aux (lambda (s en)
                          (cond [(null? en) (error (string-append "Error vars: no such variable: " s "."))]
                                [#t (if (equal? (car (car en)) s)
                                        (cdr (car en))
                                        (aux s (cdr en)))]))])
            (aux (valof-s expr) env))]
         ; Funkcije
         [(proc? expr) expr] ; vrnemo proceduro
         [(fun? expr) (closure (trim-env env expr) expr)] ; dobimo novo okolje s pomocjo trim-env in sestavimo ovojnico z njim in izrazom
         [(call? expr) (cond [(proc? (call-e expr))
                              (fri (proc-body (call-e expr)) (return-env-proc expr env))] ; poklicemo proceduro z okoljem, ki ga vrne pomozna funkcija
                             [(fun? (call-e expr))
                              (let ([r (fri (call-e expr) env)]) ; poklicemo telo funkcije v njenem okolju
                                (fri (vars (if (equal? (fun-name (closure-f r)) "") ; po potrebi dodamo ime funkcije (anonimnost)
                                               (fun-farg (closure-f r))
                                               (cons (fun-name (closure-f r)) (fun-farg (closure-f r))))
                                           (if (equal? (fun-name (closure-f r)) "") ; po potrebi dodamo telo funkcije (anonimnost)
                                               (map (lambda (x) ; evalviramo vse argumente ter jih dodamo v okolje
                                                              (if (closure? x) x (fri x env)))
                                                            (call-args expr))
                                               (cons r (map (lambda (x) ; dodamo telo funkcije
                                                              (if (closure? x) x (fri x env)))
                                                            (call-args expr))))
                                           (fun-body (closure-f r))) (closure-env r)))]
                             [(valof? (call-e expr))
                              (let ([r (fri (call-e expr) env)]) ; najdemo pripadajoco ovojnico
                                (cond [(closure? r) (fri (call (closure-f r) ; poklicemo funcijo v okolju iz ovojnice z evalviranimi argumenti
                                                               (map (lambda (x) (fri x env)) (call-args expr)))
                                                         (filter (lambda (x) (and (not (equal? (car x) (fun-name (closure-f r))))
                                                                                  (not (equal? (car x) (fun-farg (closure-f r))))))
                                                                 (closure-env r)))]
                                      [(proc? r) (fri (call r null)
                                                      (filter (lambda (x) (not (equal? (car x) (proc-name r)))) env))]
                                      [#t (error "Error call: not a function or procedure.")]))]
                             [#t (error "Error call: wrong call.")])]
         ;[(debug-env? expr) env]
         [(closure? expr) (error "Internal error: closure is internal only.")]
         [#t (error "Error: wrong structure.")]))

#| Testi
   Testiranje interpreterja sem razdelil na dva dela: testiranje navadnih struktur ter testiranje funkcij.
   Pri testiranju navadnih strukture sem se posluzil unit-testom ter izprobal vecino robnih primerih na izrazih,
   ki sem jih izracunal na roke. Poleg tega sem uporabil javne primere iz spletne ucilnice. V drugem delu sem pretestiral
   funkcije. Pri teh sem si pomagal z strukturo debug-env ki sem jo naredil, s katero lahko vidimo okolje primera
|#
#|
(require rackunit
         rackunit/text-ui)

(define test-structs
  (test-suite "Structures test"
   (check-equal? (fri (true) null) (true))
   (check-equal? (fri (false) null) (false))
   (check-equal? (fri (empty) null) (empty))      
   (check-equal? (fri (zz 1) null) (zz 1))
   (check-equal? (fri (zz -1) null) (zz -1))
   (check-exn exn:fail? (lambda () (fri (zz 1.5) null)))
   (check-exn exn:fail? (lambda () (fri (zz (true)) null)))
   (check-equal? (fri (qq (zz 1) (zz 2)) null) (qq (zz 1) (zz 2)))
   (check-equal? (fri (qq (zz -1) (zz 2)) null) (qq (zz -1) (zz 2)))
   (check-equal? (fri (qq (zz -1) (zz -2)) null) (qq (zz 1) (zz 2)))
   (check-equal? (fri (qq (zz 1) (zz -2)) null) (qq (zz -1) (zz 2)))
   (check-equal? (fri (qq (zz 9) (zz 18)) null) (qq (zz 1) (zz 2)))
   (check-equal? (fri (qq (zz 50) (zz 2)) null) (qq (zz 25) (zz 1)))
   (check-equal? (fri (qq (zz 0) (zz 18)) null) (qq (zz 0) (zz 1)))
   (check-equal? (fri (qq (add (zz 3) (zz -3)) (zz 18)) null) (qq (zz 0) (zz 1)))
   (check-exn exn:fail? (lambda () (fri (qq (zz 1) (zz 0)) null)))
   (check-exn exn:fail? (lambda () (fri (qq (zz 1.5) (zz 3)) null)))
   (check-exn exn:fail? (lambda () (fri (qq 1 (zz 2)) null)))
   (check-equal? (fri (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 2))) null) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 2))))
   (check-equal? (fri (cc (qq (zz 2) (zz 2)) (qq (zz 1) (zz 2))) null) (cc (qq (zz 1) (zz 1)) (qq (zz 1) (zz 2))))
   (check-equal? (fri (cc (qq (zz 1) (zz 2)) (qq (zz 2) (zz 2))) null) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 1))))
   (check-exn exn:fail? (lambda () (fri (cc (qq (zz 1) (zz 3)) (zz 5)) null)))
   (check-exn exn:fail? (lambda () (fri (cc (qq (zz 1.5) (zz 3)) (qq (zz 5) (zz 1))) null)))
   (check-equal? (fri (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))) null) (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))))
   (check-equal? (fri (.. (zz 1) (.. (zz 2) (zz 3))) null) (.. (zz 1) (.. (zz 2) (zz 3))))
   (check-equal? (fri (.. (add (zz 3) (zz 4)) (.. (zz 2) (.. (zz 3) (empty)))) null) (.. (zz 7) (.. (zz 2) (.. (zz 3) (empty)))))))

(define test-is-struct
  (test-suite "Is structure test"
   (check-equal? (fri (is-zz? (zz 1)) null) (true))
   (check-equal? (fri (is-zz? (qq (zz 1) (zz 2))) null) (false))
   (check-equal? (fri (is-zz? (add (zz 2) (zz 1))) null) (true))
   (check-equal? (fri (is-qq? (zz 1)) null) (false))
   (check-equal? (fri (is-qq? (qq (zz 1) (zz 2))) null) (true))
   (check-equal? (fri (is-qq? (add (qq (zz 3) (zz 2)) (qq (zz 3) (zz 1)))) null) (true))
   (check-equal? (fri (is-cc? (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 3)))) null) (true))
   (check-equal? (fri (is-cc? (qq (zz 1) (zz 2))) null) (false))
   (check-equal? (fri (is-cc? (add (cc (qq (zz 3) (zz 2)) (qq (zz 3) (zz 1))) (zz 2))) null) (true))
   (check-equal? (fri (is-bool? (true)) null) (true))
   (check-equal? (fri (is-bool? (false)) null) (true))
   (check-equal? (fri (is-bool? (zz 2)) null) (false))
   (check-equal? (fri (is-bool? (add (true) (false))) null) (true))
   (check-equal? (fri (is-empty? (empty)) null) (true))
   (check-equal? (fri (is-empty? (.. (zz 2) (empty))) null) (false))
   (check-equal? (fri (is-empty? (right (.. (zz 2) (empty)))) null) (true))
   (check-equal? (fri (is-seq? (empty)) null) (true))
   (check-equal? (fri (is-seq? (zz 1)) null) (false))
   (check-equal? (fri (is-seq? (.. (zz 2) (empty))) null) (true))
   (check-equal? (fri (is-seq? (right (.. (true) (.. (zz 2) (empty))))) null) (true))
   (check-equal? (fri (is-proper-seq? (empty)) null) (true))
   (check-equal? (fri (is-proper-seq? (.. (zz 2) (empty))) null) (true))
   (check-equal? (fri (is-proper-seq? (.. (zz 2) (false))) null) (false))
   (check-equal? (fri (is-proper-seq? (right (.. (true) (.. (zz 2) (empty))))) null) (true))))

(define test-control-flow
  (test-suite "Control flow test"
   (check-equal? (fri (if-then-else (true) (zz 1) (zz 0)) null) (zz 1))
   (check-equal? (fri (if-then-else (is-zz? (zz 1)) (add (zz 1) (zz 2)) (zz 0)) null) (zz 3))
   (check-equal? (fri (if-then-else (zz 1) (zz 1) (zz 0)) null) (zz 1))
   (check-equal? (fri (if-then-else (false) (zz 1) (zz 0)) null) (zz 0))
   (check-equal? (fri (if-then-else (is-bool? (zz 1)) (add (zz 1) (zz 2)) (add (zz 0) (zz 2))) null) (zz 2))
   (check-equal? (fri (add (false) (false)) null) (false))
   (check-equal? (fri (add (false) (true)) null) (true))
   (check-equal? (fri (add (true) (false)) null) (true))
   (check-equal? (fri (add (true) (true)) null) (true))
   (check-equal? (fri (add (zz 2) (zz 5)) null) (zz 7))
   (check-equal? (fri (add (qq (zz 1) (zz 2)) (zz 1)) null) (qq (zz 3) (zz 2)))
   (check-equal? (fri (add (zz 1) (qq (zz 1) (zz 2))) null) (qq (zz 3) (zz 2)))
   (check-equal? (fri (add (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) null) (qq (zz 5) (zz 6)))
   (check-equal? (fri (add (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) (zz 1)) null) (cc (qq (zz 3) (zz 2)) (qq (zz 1) (zz 3))))
   (check-equal? (fri (add (zz 1) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (cc (qq (zz 3) (zz 2)) (qq (zz 1) (zz 3))))
   (check-equal? (fri (add (qq (zz 1) (zz 2)) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (cc (qq (zz 1) (zz 1)) (qq (zz 1) (zz 3))))
   (check-equal? (fri (add (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) (qq (zz 1) (zz 2))) null) (cc (qq (zz 1) (zz 1)) (qq (zz 1) (zz 3))))
   (check-equal? (fri (add (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (cc (qq (zz 1) (zz 1)) (qq (zz 2) (zz 3))))
   (check-equal? (fri (add (.. (zz 1) (.. (zz 2) (empty))) (.. (zz 3) (.. (zz 4) (empty))))  null) (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (empty))))))
   (check-equal? (fri (add (empty) (empty)) null) (empty))
   (check-equal? (fri (mul (false) (false)) null) (false))
   (check-equal? (fri (mul (false) (true)) null) (false))
   (check-equal? (fri (mul (true) (false)) null) (false))
   (check-equal? (fri (mul (true) (true)) null) (true))
   (check-equal? (fri (mul (zz 2) (zz 5)) null) (zz 10))
   (check-equal? (fri (mul (qq (zz 1) (zz 2)) (zz 2)) null) (qq (zz 1) (zz 1)))
   (check-equal? (fri (mul (zz 2) (qq (zz 1) (zz 2))) null) (qq (zz 1) (zz 1)))
   (check-equal? (fri (mul (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) null) (qq (zz 1) (zz 6)))
   (check-equal? (fri (mul (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) (zz 2)) null) (cc (qq (zz 1) (zz 1)) (qq (zz 2) (zz 3))))
   (check-equal? (fri (mul (zz 2) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (cc (qq (zz 1) (zz 1)) (qq (zz 2) (zz 3))))
   (check-equal? (fri (mul (qq (zz 1) (zz 2)) (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (cc (qq (zz 1) (zz 4)) (qq (zz 1) (zz 6))))
   (check-equal? (fri (mul (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) (qq (zz 1) (zz 2))) null) (cc (qq (zz 1) (zz 4)) (qq (zz 1) (zz 6))))
   (check-equal? (fri (mul (cc (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) (cc (qq (zz 1) (zz 4)) (qq (zz 1) (zz 5)))) null) (cc (qq (zz 7) (zz 120)) (qq (zz 11) (zz 60))))
   (check-equal? (fri (leq? (false) (false)) null) (true))
   (check-equal? (fri (leq? (false) (true)) null) (true))
   (check-equal? (fri (leq? (true) (false)) null) (false))
   (check-equal? (fri (leq? (true) (true)) null) (true))
   (check-equal? (fri (leq? (zz 1) (zz 3)) null) (true))
   (check-equal? (fri (leq? (zz 2) (zz 2)) null) (true))
   (check-equal? (fri (leq? (zz 3) (zz 1)) null) (false))
   (check-equal? (fri (leq? (qq (zz 1) (zz 3)) (qq (zz 1) (zz 2))) null) (true))
   (check-equal? (fri (leq? (qq (zz 1) (zz 3)) (qq (zz 27) (zz 81))) null) (true))
   (check-equal? (fri (leq? (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) null) (false))
   (check-equal? (fri (leq? (.. (true) (empty)) (.. (true) (.. (false) (empty)))) null) (true))
   (check-equal? (fri (leq? (.. (true) (.. (true) (empty))) (.. (true) (.. (false) (empty)))) null) (true))
   (check-equal? (fri (leq? (.. (true) (.. (false) (empty))) (.. (true) (empty))) null) (false))
   (check-equal? (fri (rounding (zz 1)) null) (zz 1))
   (check-equal? (fri (rounding (qq (zz 1) (zz 2))) null) (zz 0))
   (check-equal? (fri (rounding (qq (zz 3) (zz 2))) null) (zz 2))
   (check-equal? (fri (rounding (qq (zz 2) (zz 3))) null) (zz 1))
   (check-equal? (fri (rounding (qq (zz -3) (zz 2))) null) (zz -2))
   (check-equal? (fri (=? (zz 5) (add (zz 2) (zz 3))) null) (true))
   (check-equal? (fri (=? (zz 2) (qq (zz 2) (zz 1))) null) (false))
   (check-equal? (fri (=? (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))) (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty))))) null) (true))
   (check-equal? (fri (left (add (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (zz 5))
   (check-equal? (fri (right (add (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (zz 6))
   (check-equal? (fri (left (add (zz 1) (cc (qq (zz 1) (zz 2)) (qq (zz 2) (zz 5))))) null) (qq (zz 3) (zz 2)))
   (check-equal? (fri (right (add (zz 1) (cc (qq (zz 1) (zz 2)) (qq (zz 2) (zz 5))))) null) (qq (zz 2) (zz 5)))
   (check-equal? (fri (left (.. (true) (.. (false) (empty)))) null) (true))
   (check-equal? (fri (right (.. (true) (.. (false) (empty)))) null) (.. (false) (empty)))
   (check-equal? (fri (~ (add (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3)))) null) (qq (zz -5) (zz 6)))
   (check-equal? (fri (~ (add (zz 1) (zz 3))) null) (zz -4))
   (check-equal? (fri (~ (add (false) (false))) null) (true))
   (check-equal? (fri (~ (add (true) (true))) null) (false))
   (check-equal? (fri (~ (cc (qq (zz 1) (zz 2)) (qq (zz 2) (zz 5)))) null) (cc (qq (zz 1) (zz 2)) (qq (zz -2) (zz 5))))
   (check-equal? (fri (all? (.. (true) (.. (zz 2) (.. (add (zz 3) (zz 4)) (empty))))) null) (true))
   (check-equal? (fri (all? (.. (true) (.. (leq? (false) (true)) (.. (all? (.. (true) (empty))) (empty))))) null) (true))
   (check-equal? (fri (all? (.. (true) (.. (false) (.. (add (zz 3) (zz 4)) (empty))))) null) (false))
   (check-equal? (fri (all? (empty)) null) (true))
   (check-equal? (fri (any? (empty)) null) (false))
   (check-equal? (fri (any? (.. (true) (.. (false) (.. (add (zz 3) (zz 4)) (empty))))) null) (true))
   (check-equal? (fri (any? (.. (qq (zz 8) (zz 2)) (.. (false) (.. (add (zz 3) (zz 4)) (empty))))) null) (true))
   (check-equal? (fri (any? (.. (false) (.. (false) (.. (false) (empty))))) null) (false))))

(define test-variables
  (test-suite "Variables test"
   (check-equal? (fri (vars "a" (zz 1) (add (zz 1) (zz 1))) null) (zz 2))
   (check-equal? (fri (vars (list "a" "b" "c") (list (zz 1) (zz 2) (zz 3)) (add (valof "a") (add (valof "b") (valof "c")))) null) (zz 6))
   (check-equal? (fri (vars "a" (zz 1) (add (valof "a") (valof "a"))) null) (zz 2))
   (check-equal? (fri (vars "a" (zz 1) (vars "a" (zz 2) (add (valof "a") (valof "a")))) null) (zz 4))))

(define test-makros
  (test-suite "Makros test"
   (check-equal? (fri (numerator (qq (zz 4) (zz 6))) null) (zz 2))
   (check-equal? (fri (denominator (qq (zz 4) (zz 6))) null) (zz 3))
   (check-equal? (fri (re (cc (qq (zz 4) (zz 6)) (qq (zz 10) (zz 18)))) null) (qq (zz 2) (zz 3)))
   (check-equal? (fri (im (cc (qq (zz 4) (zz 6)) (qq (zz 10) (zz 18)))) null) (qq (zz 5) (zz 9)))
   (check-equal? (fri (im (cc (qq (zz 4) (zz 6)) (qq (zz 10) (zz 18)))) null) (qq (zz 5) (zz 9)))
   (check-equal? (fri (gt? (false) (false)) null) (false))
   (check-equal? (fri (gt? (false) (true)) null) (false))
   (check-equal? (fri (gt? (true) (false)) null) (true))
   (check-equal? (fri (gt? (true) (true)) null) (false))
   (check-equal? (fri (gt? (zz 1) (zz 3)) null) (false))
   (check-equal? (fri (gt? (zz 2) (zz 2)) null) (false))
   (check-equal? (fri (gt? (zz 3) (zz 1)) null) (true))
   (check-equal? (fri (gt? (qq (zz 1) (zz 3)) (qq (zz 1) (zz 2))) null) (false))
   (check-equal? (fri (gt? (qq (zz 1) (zz 3)) (qq (zz 27) (zz 81))) null) (false))
   (check-equal? (fri (gt? (qq (zz 1) (zz 2)) (qq (zz 1) (zz 3))) null) (true))
   (check-equal? (fri (gt? (.. (true) (empty)) (.. (true) (.. (false) (empty)))) null) (false))
   (check-equal? (fri (gt? (.. (true) (.. (true) (empty))) (.. (true) (.. (false) (empty)))) null) (false))
   (check-equal? (fri (gt? (.. (true) (.. (false) (empty))) (.. (true) (empty))) null) (true))
   (check-equal? (fri (inv (zz 1)) null) (qq (zz 1) (zz 1)))
   (check-equal? (fri (inv (zz 5)) null) (qq (zz 1) (zz 5)))
   (check-equal? (fri (inv (zz -3)) null) (qq (zz -1) (zz 3)))
   (check-exn exn:fail? (lambda () (fri (inv (zz 0)) null)))
   (check-equal? (fri (inv (qq (zz 1) (zz 4))) null) (qq (zz 4) (zz 1)))
   (check-equal? (fri (inv (qq (zz -1) (zz 4))) null) (qq (zz -4) (zz 1)))
   (check-equal? (fri (inv (qq (zz 3) (zz 6))) null) (qq (zz 2) (zz 1)))
   (check-exn exn:fail? (lambda () (fri (inv (qq (zz 0) (zz 1))) null)))
   (check-exn exn:fail? (lambda () (fri (inv (qq (zz 1) (zz 0))) null)))
   (check-equal? (fri (inv (empty)) null) (empty))
   (check-equal? (fri (inv (.. (zz 1) (.. (zz 2) (.. (add (zz 1) (zz 2)) (empty))))) null) (.. (zz 3) (.. (zz 2) (.. (zz 1) (empty)))))
   (check-equal? (fri (inv (cc (qq (zz 6) (zz 3)) (qq (zz 8) (zz 2)))) null) (cc (qq (zz 1) (zz 10)) (qq (zz -1) (zz 5))))
   (check-equal? (fri (mapping (fun "" (list "e") (mul (valof "e") (valof "e"))) (empty)) null) (empty))
   (check-equal? (fri (mapping (fun "" (list "e") (mul (valof "e") (valof "e"))) (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty))))) null)
                 (.. (zz 1) (.. (zz 4) (.. (qq (zz 9) (zz 4)) (empty)))))
   (check-equal? (fri (filtering (fun "" (list "e") (mul (valof "e") (valof "e"))) (empty)) null) (empty))
   (check-equal? (fri (filtering (fun "" (list "e") (mul (valof "e") (valof "e"))) (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty))))) null)
                 (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty)))))
   (check-equal? (fri (filtering (fun "" (list "e") (is-zz? (valof "e"))) (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty))))) null)
                 (.. (zz 1) (.. (zz 2) (empty))))
   (check-equal? (fri (filtering (fun "" (list "e") (true)) (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty))))) null)
                 (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty)))))
   (check-equal? (fri (filtering (fun "" (list "e") (false)) (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty))))) null)
                 (empty))
   (check-equal? (fri (folding (fun "" (list "a" "b") (add (valof "a") (valof "b"))) (zz 0)
                               (.. (zz 1) (.. (zz 2) (.. (qq (zz 3) (zz 2)) (empty))))) null)
                 (qq (zz 9) (zz 2)))
   (check-equal? (fri (folding (fun "" (list "a" "b") (add (valof "a") (valof "b"))) (zz 0) (empty)) null)
                 (zz 0))
   (check-equal? (fri (folding (fun "" (list "a" "b") (.. (mul (left (valof "a")) (right (valof "b")))
                                                          (add (right (valof "a")) (left (valof "b"))))) (.. (zz 0) (zz 2))
                               (.. (.. (zz 2) (zz 8)) (.. (.. (zz 3) (zz 7)) (.. (.. (zz 4) (zz 6)) (empty))))) null)
                 (.. (zz 44) (zz 30)))))

(define public-tests
  (test-suite
   "all"
 (test-suite
  "pulic"
  (test-case "add1" (check-equal?
                     (add (mul (true) (true)) (false))
                     (add (mul (true) (true)) (false))))
 
  (test-case "add2" (check-equal?
                     (fri (add (mul (true) (true)) (false)) null)
                     (true)))
 
  (test-case "proper-seq1" (check-equal?
                            (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))
                            (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))))
 
  (test-case "proper-seq2" (check-equal?
                            (fri (.. (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))
                                     (is-proper-seq? (.. (zz 1) (.. (zz 2) (zz 3))))) null)
                            (.. (true) (false))))
 
  (test-case "vars-and-complex1" (check-equal?
                                  (fri (vars "a" (cc (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))
                                             (mul (valof "a") (valof "a"))) null)
                                  (cc (qq (zz -5) (zz 16)) (qq (zz -3) (zz 4)))))
 
  (test-case "vars-and-complex2" (check-equal?
                                  (fri (vars (list "a" "b")
                                             (list (cc (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))
                                                   (~ (cc (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))))
                                             (add (valof "a") (valof "b"))) null)
                                  (cc (qq (zz 1) (zz 1)) (qq (zz 0) (zz 1)))))
 
  (test-case "fib1" (check-equal?
                     (fri (call (fun "fib" (list "n")
                                     (if-then-else (leq? (valof "n") (zz 2))
                                                   (zz 1) (add (call (valof "fib")
                                                                     (list (add (valof "n") (zz -1))))
                                                               (call (valof "fib")
                                                                     (list (add (valof "n") (zz -2)))))))
                                (list (zz 10))) null)
                     (zz 55)))
 
  (test-case "seq1" (check-equal?
                     (fri (all? (.. (true) (.. (leq? (false) (true))
                                               (.. (=? (.. (zz -19) (zz 0))
                                                       (.. (left (add (qq (zz 1) (zz 5)) (zz -4)))
                                                           (zz 0)))
                                                   (empty)))))
                          null)
                     (true)))
 
  (test-case "variables1" (check-equal?
                           (fri (vars (list "a" "b" "c")
                                      (list (zz 1) (zz 2) (zz 3))
                                      (fun "linear" (list "x1" "x2" "x3")
                                           (add (mul (valof "a") (valof "x1"))
                                                (add (mul (valof "b") (valof "x2"))
                                                     (mul (valof "c") (valof "x3")))))) null)
                           (closure (list (cons "a" (zz 1))(cons "b" (zz 2)) (cons "c" (zz 3)))
                                    (fun "linear" '("x1" "x2" "x3")
                                         (add (mul (valof "a") (valof "x1"))
                                              (add (mul (valof "b") (valof "x2"))
                                                   (mul (valof "c") (valof "x3")))))))))
 
 (test-suite
  "misc"
  (test-case "add-seq" (check-equal?
                        (fri (add (.. (false) (empty))
                                  (.. (zz 3) (empty))) null)
                        (.. (false) (.. (zz 3) (empty)))))
 
  (test-case "add-empty" (check-equal?
                          (fri (add (empty) (empty)) null)
                          (empty))))

 (test-case
  "long-long"
  (check-equal?
   (fri
    (vars "a" (zz 10)
          (vars (list "f" "g")
                (list (fun "" (list "a" "b")
                           (add (valof "a") (mul (zz 5) (valof "b"))))
                      (fun "" (list "c")
                           (add (valof "a") (valof "c"))))
                (vars (list "a" "d" "g" "e")
                      (list (zz 1)
                            (call (valof "g") (list (zz -9)))
                            (fun "" (list "x")
                                 (add (valof "a") (mul (valof "x")
                                                       (call (valof "f")
                                                             (list (zz 1) (valof "a"))))))
                            (fun "" (list "f" "x")
                                 (call (valof "f") (list (valof "x")))))
                      (vars (list "fib" "test" "unit-fun" "proc")
                            (list (fun "fib" (list "n")
                                       (if-then-else (leq? (valof "n") (zz 2))
                                                     (zz 1)
                                                     (add (call (valof "fib")
                                                                (list (add (valof "n")
                                                                           (zz -1))))
                                                          (call (valof "fib")
                                                                (list (add (valof "n")
                                                                           (zz -2)))))))
                                  (fun "" (list "x")
                                       (add (valof "x") (zz 2)))
                                  
                                  (fun "" null
                                       (add (inv (add (valof "a")
                                                      (valof "a")))
                                            (valof "a")))
                                  
                                  (proc ""
                                        (folding
                                         (fun "" (list "x" "acc") (mul (valof "x") (valof "acc")))
                                         (zz 1)
                                         (.. (valof "a")
                                             (.. (zz 2)
                                                 (.. (zz 3)
                                                     (.. (zz 4)
                                                         (.. (call (valof "g")
                                                                   (list (zz 5)))
                                                             (empty)))))))))
                            
                            
                            (.. (call (valof "unit-fun") null)
                                (.. (call (valof "proc") null)
                                    (add (call (valof "g")
                                               (list (add (zz 5)
                                                          (call (valof "test")
                                                                (list (zz 3))))))
                                         (add (valof "d")
                                              (add (call (valof "f")
                                                         (list (zz -1) (zz -2)))
                                                   (add (valof "a")
                                                        (add (call (valof "fib")
                                                                   (list (zz 5)))
                                                             (call (valof "e")
                                                                   (list (valof "test") (zz 3))))))))))))))
    null)
   (.. (qq (zz 3) (zz 2)) (.. (zz 6360) (zz 521)))))))

(run-tests test-structs)
(run-tests test-is-struct)
(run-tests test-control-flow)
(run-tests test-variables)
(run-tests test-makros)
(run-tests public-tests) 
|#
#| Drugi del testnih primerov. Ta vsebuje teste za procedure, funkcije ter njihove ovojnice. Za uporabo je potrebno odkomentirati
   sturkturo debug-env ter njeno implementacijo |#
#|
  
(fri (call (fun "fib" (list "n")
                (if-then-else (leq? (valof "n") (zz 2))
                              (zz 1)
                              (add (call (valof "fib")
                                         (list (add (valof "n") (zz -1))))
                                   (call (valof "fib")
                                         (list (add (valof "n") (zz -2)))))))
           (list (zz 10))) null)

(fri (vars (list "a" "b")
           (list (zz 5) (zz 8))
           (call (proc "c" (.. (mul (valof "a") (valof "b")) (debug-env))) null)) null)

(fri (vars (list "a" "b")
           (list (zz 5) (zz 8))
           (call (proc "" (debug-env)) null)) null)

(fri (vars (list "a" "b")
           (list (zz 5) (zz 8))
           (vars "d" (proc "c" (.. (mul (valof "a") (valof "b")) (debug-env)))
                 (vars (list "a" "b") (list (zz 10) (zz 12))
                       (call (valof "d") null)))) null)

(fri (proc "a" (mul (zz 4) (zz 5))) null)

(fri (vars (list "a" "b")
           (list (zz 5) (zz 8))
           (vars "d" (fun "c" null (.. (mul (valof "a") (valof "b")) (debug-env)))
                 (vars (list "a" "b") (list (zz 10) (zz 12))
                       (call (valof "d") null)))) null)

(fri (vars (list "a" "b" "c")
           (list (zz 5) (zz 8) (zz 9))
           (vars (list "a" "b") (list (zz 10) (zz 12))
                 (vars "d" (fun "" (list "c") (.. (mul (valof "c") (valof "b")) (debug-env)))
                       (call (valof "d") (list (zz 8)))))) null)

(fri (mapping (fun "" (list "el") (mul (valof "el") (valof "el")))
              (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (.. (zz 5) (empty))))))) null)

(fri (filtering (fun "" (list "el") (leq? (zz 3) (valof "el")))
              (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (.. (zz 5) (empty))))))) null)

(fri (folding (fun "" (list "el1" "el2") (add (valof "el1") (valof "el2")))
              (zz 0)
              (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (.. (zz 5) (empty))))))) null) |#