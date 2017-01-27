; Wiktor Adamski
#lang racket

; Zadanie 1

(define końce
  (local [(define end
            (λ (li)
              (if (empty? (cdr li)) (car li) (end (cdr li)))))]
    (λ (li)
    (if (empty? li) (error "Lista pusta")
        (cons (car li) (end li))))))

(define z1t1 (końce '(1 2 3 5)))
(define z1t2 (końce '(1)))
;(define z1t3 (końce '()))          ; rzuca zamierzony wyjątek

; Zadanie 2
(define count
  (λ (n li)
    (if (list? li)
       (if (empty? li) 0
           (if (equal? n (car li)) (+ 1 (count n (cdr li)))
               (count n (cdr li))))
        (error "Argument 2 nie jest listą"))))

(define z2t1 (count 'a '(a l a)))
(define z2t2 (count 'a '()))
;(define z2t3 (count 'a 'a))         ; rzuca zamierzony wyjątek

; Zadanie 3
(define posortowana
  (λ (li)
    (if (or (empty? li)(empty? (cdr li))) #t
        (if (<= (car li) (cadr li)) (posortowana (cdr li)) #f))))

(define z3t1 (posortowana '(1 3 5 5 7)))
(define z3t2 (posortowana '(6 4 2)))
(define z3t3 (posortowana '()))

; Zadanie 4
(define evenN?
  (λ (n)
    (if (zero? n) #t (if (< n 0) (oddN? (+ n 1)) (oddN? (- n 1))))))

(define oddN?
  (λ (n)
    (if (zero? n) #f (if (< n 0) (evenN? (+ n 1)) (evenN? (- n 1))))))

(define z4t1 (evenN? 3))
(define z4t2 (evenN? 2))
(define z4t3 (evenN? -3))
(define z4t4 (evenN? -2))
(define z4t5 (oddN? 3))
(define z4t6 (oddN? 2))
(define z4t7 (oddN? -3))
(define z4t8 (oddN? -2))


; Zadanie 5
(define map1
  (λ (fn li)
    (if (empty? li) '() (cons (fn (car li)) (map1 fn (cdr li))))))

(define z5t1 (map1 (λ (x) (* x x)) '(1 2 3 -4)))

; Zadanie 6
(define unzip
  (λ (li)
    (cons (map1 car li) (map1 cadr li))))

(define z6t1 (unzip '((a 1) (b 2) (c 3))))
(define z6t2 (unzip '()))