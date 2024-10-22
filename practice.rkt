#lang scheme




(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; (require "a.rkt")
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (displayln (atom? (car l))) (lat? (cdr l))
      (else #f))))


(define member?
  (lambda (m lat)
    (or (eq? m (car lat)) (member? m (cdr lat)))))
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ;common (cdr lat)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
(define visit_back
  (lambda (a lat)
    (cond
      ((eq? (car(lat)) (a)) (cdr(lat)))
      (else (visit_back a (cdr(lat)))))))
(define makeset
  (lambda (lat)
    (cond
      ((null? lat)(quote ()))
      ((member? (car lat) (cdr(lat)))(makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))
(define makesetII
  (lambda (lat)
    (cond
      ((null? lat)(quote ()))
      ; ((member? (car lat) (cdr(lat)))(cons(multirember (car lat) (cdr lat))))
      ;simplify version
      (else (cons (car lat) (makesetII(multirember (car lat) (cdr lat))))))))
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? cdr(set1) set2))
      ;there is a and version combined with memeber? and subset?
      ; by no any array is sentensed by false first
      (else #f)
      )))
(define eqset?
  (lambda (set1 set2)
    (cond
      ((and(subset? set1 set2)(subset? set2 set1)) #t)
      (else #f)
      )))

(define intersect
  (lambda (set1 set2)
    (cond
      ((member? (car set1) set2)(cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

; (define intersectall
; (lambda (lat)


; lat is like ((l1)(l2)(...))||(lat? l_n) is #t
; every direct element is
; (cond
; ((member? (car set1) set2)(cons (car set1) (intersect (cdr set1) set2)))
; (else (intersect (cdr set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ; lose its same part
      ((member? (car set1) set2)((union (cdr set1) set2)))
      (else (cons (car set1) (union (cdr set1) set2))))))




;rember a lat
;;remove first a from lat
;(define rember
;  (lambda (a lat)
;   (cond
;    ((member? a lat) cons(prev (visit_back(a lat))))
;   (else ()))

;(define rember
;  (lambda (a lat)
;    (cond
;      (( null? lat) (quote ()))
;      (else (cond
;              ( ( eq? (car lat) a) (cdr lat))
;              (else (rember a (cdr lat))))))))
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (multirember(cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))


; cons a list of every element in first element
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car l) (firsts(cdr(l)))))
      )))
;abstract
(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car(cdr(car l))) (seconds(cdr(l)))))
      )))
;(define insertR
;  (lambda (old newl lat)
;    (cond
;      ((null? lat) '())
;      (else (cond
;              ((eq? car(lat) old) (cons(car(lat) cons( newl cdr(insertR(lat))))))
;              (else (cons car(lat) insertR(old newl cdr(lat))))
;              )))))

; (define insertR
;   (lambda (newl old lat)
;     (cond
;       ((null? lat) (quote 0))
;       (else (cond
;         (( eq? (car lat) old)
;         (cons old (cons newl (cdr lat))))
;         (else (cons (car lat) (insertR newl old (cdr lat)))))))))
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote 0))
      (else (cond
              (( eq? (car lat) old)
               (cons old
                     (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old
                                   (cdr lat)))))))))


(define insertL
  (lambda (newl old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons newl lat))
              (else (cons (car lat) (insertL newl old (cdr lat)))))))))

(define reverse
  (lambda (x)
    (cond ((atom? x) '())
          (else (append (reverse (cdr x)) (list (car x)))))))

; (define a 'power)
; (define c 'short)
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or(eq? o1 (car lat)) (eq? o2 (car lat)))
       (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat))))
      )))

; chapter 6
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x))
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote T))
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp)nexp);is value
      ; (((and (number? (car nexp))(number? (car(cdr(cdr nexp))))))())`
      ((eq? (car(cdr nexp)) '+)(+ value(cdr nexp)))
      ((eq? (car(cdr nexp)) '*)(* value(cdr nexp)))

      (else
       (/ (value (car nexp))
          ( value
            (car (cdr (cdr nexp))))

          )))))
(define zero?
  (lambda (n)
    (null? n)))
(define add1
  (lambda (n)
    (cons (quote ()) n)))
(define sub1
  (lambda (n)
    (cdr n)))
;

; (define evens-only*&co
;   (lambda (I col)
;     (cond
;       ((null? I)
;        (col (quote ()) 1 0)
;             ((atom? (car
;                      I
;                      (cond
;                        ((even? (car
;                                 I
;                                 (evens-only*&co (cdr I)
;                                                 (lambda (newl p s)
;                                                   (col (cons (car I) newl)
;                                                        (x (car l) p) s))))
;                                (else (evens-only*&co (cdr I)
;                                                      (lambda (newl p s)
;                                                        (col newl
;                                                             p (+ (car I) s»))))))
;                         (else (evens-only*&co (car I)
;                                               ... »)))
;__
;||
;\/
(read-eval-print-loop)
;ensure run the script to enter the 
