; CSCI 320 Fall 2016
; Symbolic Differentiation Example
#lang racket

; this predicate just tells us whether a term is a constant term
(define constant? number?)

; this predicate tells us whether a term is a variable named x
(define (variable? T x) 
  (and (symbol? T) (eqv? T x)))

; this predicate tells us whether we have a term of the form (k x) representing the polynomial kx
(define (linear_term? T x)
  (cond ((and (number? (car T)) (variable? (cadr T) x) (null? (cddr T))))
        (else #f)))

; this predicate defines a "term in x" as either a constant, the variable x, a linear term in x
;  or a term of the form (a x n), where a and n are numbers.
(define (term? T x) 
  (cond ((constant? T) #t)
        ((variable? T x) #t)
        ((linear_term? T x) #t)
        ((and (number? (car T)) (variable? (cadr T) x) (number? (caddr T))))
        (else #f)))

; this predicate tells us whether a given expression is a list of "terms in x"
(define (term_list? T x)
   (cond ((null? T) #t)
         ((and (term? (car T) x) (term_list? (cdr T) x)) #t)
         (else #f)))


; this predicate defines a polynomial E in x as having the form of a term,
;  or the form (+ term_in_x term_in_x...term_in_x)
;  that is a list whose first element is the plus sign and whose cdr is a list of terms in x
(define (polynomial? T x)
  (cond ((term? T x) #t)
        ((and (eqv? (car T) '+) (term_list? (cdr T) x)) #t)
        (else #f)))

; here we define some more meaningfully named functions for handling terms in x of the form (a x n)
(define (get-coefficient T) (car T))
(define (get-variable T) (cadr T))
(define (get-exponent T) (caddr T))


; now we implement symbolic derivatives of terms.  Note, the derivative of a term in x with respsect to a variable
;  other than x is 0.  Likewise, the derivative with respect to x of a term in a variable other than x is 0.

(define (d-term T x)
  (cond ((constant? T) 0)
        ((variable? T x) 1)
        ((linear_term? T x) (get-coefficient T))
        ((term? T x) (list (* (get-coefficient T) (get-exponent T))
                         (get-variable T)
                         (- (get-exponent T) 1)))
        (else 0)))

; the derivative of a list with respect to x of a list of terms will be the list of derivatives
;(define (d-term_list TL x)
;  (cond ((null? TL) '())
;        (else (cons(d-term (car TL) x) (d-term_list (cdr TL) x))))) 

; a polynomial is a sum of terms in x, so the derivative of a polynomial is the sum of the derivatives
;  of its constituent terms with respct to x
(define (d-polynomial P x)
  (cond ((term? P x) (d-term P x))
        ((polynomial? P x) (cons (car P)  (d-term_list (cdr P) x)))
        (else 0)))
; --------------------------------------------------------------------
; Below this point are additional functions that are added as a result
; of assignment 2.
; --------------------------------------------------------------------

; my own personal function that returns the length of a list
; L is a list and c should be passed in as 0 since it counts the length of the list
(define (len_list L c)
  (cond ((null? L) c)
        (else (len_list (cdr L) (+ c 1)))
        )
  )

; takes as parameters n and an expression e and constructs a list consisting
; of n nopies of e (number 1)
(define (make_list_of_size n e)
  (cond ((zero? n) '())
        (else (cons e (make_list_of_size (- n 1) e)))
        )
  )
                       
; the derivative of a list with respect to x of a list of terms will
; be the list of derivatives (number 2)
(define (d-term_list TL x)
  (map (lambda (T) (d-term T x)) TL))

; accepts a polynomial of the type used in differentiation.rak as a paramter
; and simplifies it as follows:
; - removes all occurences of the constant 0 from the polynomial
; - '(+ T 0) and '(+ 0 T)  become simply T
; - replaces all terms of the for (a x 1) by the term (a x)
(define (simplify_poly p)
  (cond ((null? p) '())
        ((and (number? (car p)) (zero? (car p))) (simplify_poly (cdr p)))
        ((not (list? (car p))) (cons (car p) (simplify_poly (cdr p))))
        ((and (list? (car p)) (= (caddar p) 1)) (cons (cons (caar p) (cons (cadar p) '())) (simplify_poly (cdr p))))
        (else (cons (car p) (simplify_poly (cdr p))))
        )
  )

; takes an expression E and a variable x as parameters and return #t if the expression
; represents the product of two polynomials in the variable x, and #f otherwise
(define (product? E x)
  (cond ((not (list? E)) #f)
        ((and (eqv? (len_list E 0) 3) (eqv? (car E) '*) (polynomial? (cadr E) x) (polynomial? (caddr E) x)) #t) 
        (else #f)
        )
  )

; takes an expression E and a variable x as paramters and returns the derivative of E
; in x if the expression represents the product of two polynomials
(define (d-product E x)
  (cond ((not (product? E x)) #f)
        (else (cons '+ 
                    (cons (cons '* (cons (cadr E) (cons (simplify_poly (d-polynomial (caddr  E) x)) '() )))
                          (cons (cons '* (cons (caddr  E) (cons (simplify_poly (d-polynomial (cadr E) x)) '() ))) '() ))))
        )
  )

; takes an expression E and a variable x as parameters and returns #t if the expression
; represents the quotient of two polynomials in the variable x, and #f otherwise
(define (quotient? E x)
  (cond ((not (list? E)) #f)
        ((and (eqv? (len_list E 0) 3) (eqv? (car E) '/) (polynomial? (cadr E) x) (polynomial? (caddr E) x)) #t) 
        (else #f)
        )
  )

; takes an expression E and a variable x as parameters and returns the derivative of E in
; x if the expression represents the quotient of two polynomials in the variable x
(define (d-quotient E x)
  (cond ((not (quotient? E x)) #f)
        (else (cons '/ (cons (cons '-
                         (cons (cons '* (cons (caddr E) (cons (simplify_poly (d-polynomial (cadr E) x)) '() )))
                         (cons (cons '* (cons (cadr E) (cons (simplify_poly (d-polynomial (caddr E) x)) '() ))) '() )))
                    (cons (cons '* (cons (caddr E) (cons (caddr E) '() ))) '() ))))
        )
  )
