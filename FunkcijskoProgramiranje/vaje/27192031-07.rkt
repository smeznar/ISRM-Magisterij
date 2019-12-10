#lang racket
; tok ones, ki ustreza zaporedju samih enic (1 1 1 ...)
(define ones (cons 1 (lambda () ones)))

; tok naturals, ki ustreza zaporedju naravnih števil (1 2 3 4 ...)


; tok fibs, ki ustreza zaporedju fibonaccijevih števil (1 1 2 3 5 ...)


; first, ki sprejme število n in tok, ter vrne seznam prvih n števil iz toka.


; squares, ki sprejme tok, in vrne nov tok, ki vsebuje kvadrirane elemente prvega toka.


; 6, 7. makro sml, ki podpira uporabo "sml sintakse" za delo s seznami. Podprite sml funkcije/konstruktorje ::,
; hd, tl, null in nil. Sintaksa naj bo taka, kot je navedena v primeru uporabe spodaj.
; (Sintaksa seveda ne bo povsem enaka sml-jevi, saj zaradi zahtev racketa še vedno ne smemo pisati odvečnih oklepajev, potrebno pa je pisati presledke okoli funkcij/parameterov, pa vseeno.)



; my-delay, my-force. Funkciji za zakasnitev in sprožitev delujeta tako, da si funkcija za sprožitev pri prvem klicu zapomni rezultat, ob naslednjih
; pa vrne shranjeno vrednost. Popravite funkciji tako, da bo funkcija za sprožitev ob prvem in nato ob vsakem petem klicu ponovno izračunala in shranila rezultat.


;partitions, ki sprejme števili k in n, ter vrne število različnih načinov, na katere lahko n zapišemo kot vsoto k naravnih števil (naravna števila se v tem kontekstu začnejo z 1).
;(Če se dva zapisa razlikujeta samo v vrstnem redu elementov vsote, ju obravnavamo kot en sam zapis
