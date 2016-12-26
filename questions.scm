(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
   (if (null? items) nil (cons (proc (car items)) (map proc (cdr items)))))

(define (cons-all first rests)
   (define (cons-one lst) (append (list first) lst))
    (map cons-one rests))

(define (zip pairs)
  ; returns a list of list with each inner list of the same length as the initial list of lists
  ;(define (how_long pairs)
  ;  (define count 0)
  ;  (if (null? pairs) 
  ;    caount
  ;    a(+ count (cdr pairs))
  ;  )8

  (list (map car pairs) (map cadr pairs))
)

  ;(define length (how_long pairs))
  
  ;(define make_mini (make_flat pairs))



;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (make_list s index)
    (if (null? s) nil

    (cons (cons index (cons (car s) nil)) (make_list (cdr s) (+ index 1)))
    )
  )
  (make_list s 0)
)
  ; END PROBLEM 17

  ;def enumerate(s): 
    ;def make_list(s, index): 
      ;if s is nil: 
        ;return nil
      ;else:
        ;return Pair(Pair(make_list(s.second, index+1)))



;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  
  (cond ((null? denoms) nil)
    ((= 0 total) nil)
    ((< total (car denoms)) (list-change total (cdr denoms)))
    ((> total (car denoms)) 
      (define with (cons-all (car denoms) (list-change (- total (car denoms)) denoms)))
      (define without (list-change total (cdr denoms)))
      (append with without))
    ((= total (car denoms))
      (cons (cons (car denoms) nil) (list-change total (cdr denoms))))
  )  
)

  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
            (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define (find_formals given) 
              (if (eq? (cdr given) nil)
                (cons (caar given) nil)
                (cons (caar given) (find_formals (cdr given)))
              )
            )
           (define (find_vals given) 
              (if (eq? (cdr given) nil)
                (cons (car(cdr(car given))) nil)
                (cons (car(cdr(car given))) (find_vals (cdr given)))
              )
            )

           (cons (cons 'lambda (cons (find_formals values) (map let-to-lambda body))) (map let-to-lambda (find_vals values)))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
