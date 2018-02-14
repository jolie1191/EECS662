#lang plai

;;abstract syntax of the WAEE
(define-type WAEE
  (num (n number?))
  (id (name symbol?))
  (binop (op symbol?) (lhs WAEE?) (rhs WAEE?))
  (with (name symbol?) (named-expr WAEE?) (body WAEE?)))

;;parse-wae:concrete syntax of WAEE --> WAEE data structure
(define parse-waee
  (lambda (sexp)
    (cond ((number? sexp) (num sexp))
          ((symbol? sexp) (id sexp))
          ((list? sexp)
           (case (car sexp)
             ;((+) (add (parse-wae (cadr sexp)) (parse-wae (caddr sexp))))
             ;((-) (sub (parse-wae (cadr sexp)) (parse-wae (caddr sexp))))
             ((+) (binop 'plus (parse-waee (cadr sexp)) (parse-waee (caddr sexp))))
             ((-) (binop 'minus (parse-waee (cadr sexp)) (parse-waee (caddr sexp))))
             ((*) (binop 'mult (parse-waee (cadr sexp)) (parse-waee (caddr sexp))))
             ((/) (binop 'div (parse-waee (cadr sexp)) (parse-waee (caddr sexp))))
             ((with) (with-rec (cadr sexp) (caddr sexp)))
             (else (error 'parse-waee "parse-waee error"))))
          (error 'parse-waee"parse-wae Error" ))))

;;with recurssion
(define with-rec
  (lambda (binding-inst expr)
    (cond ((null? binding-inst) (parse-waee expr))
          ((symbol? (car binding-inst))
           (with (car binding-inst) (parse-waee (cadr binding-inst)) (parse-waee expr)))
          (else
           (if (error-handler (caar binding-inst) (cdr binding-inst))
               (error 'with-rec "identifiers define multiple times")
               (if (or-list (map (lambda (x) (check-reference-error (caar binding-inst) x)) (cdr binding-inst)))
                   (error 'check-reference-error "Reference to an identifier before it is defined!")
                   (with (caar binding-inst) (parse-waee (cadar binding-inst)) (with-rec (cdr binding-inst) expr))))))))



(define or-list
  (lambda (x)
    (if (null? x) #f
        (if (< 1 (length x))
            (or (or-list (cdr x)) (car x))
            (car x)))))


(define check-reference-error
  (lambda (id expr)
    (if (null? expr)
        #f
        (if (pair? expr)
            (or (check-reference-error id (cadr expr)) (if (> (length expr) 2)
                                                           (check-reference-error id (third expr))
                                                           #f))
            (if (number? expr)
                #f
                (if(symbol=? id expr) #t #f))))))


;;error handler
(define error-handler
  (lambda (i binding-list)
    (cond ((null? binding-list) #f)
          (#t (if (symbol=? (caar binding-list) i)
              #t
              (error-handler i (cdr binding-list)))))))
          
    
#|(define check-reference-error
  (lambda (id expr)
    (if (null? expr) #f
        (if (pair? expr)
            ;(begin (displayln id) (displayln expr)
            (or (check-reference-error id (cadr(car expr))) (if (> (length expr) 2)(check-reference-error id (third expr)) #f))
            (if (number? expr) #f
                (if(symbol=? id expr) #t #f))))))|#

#|(define check-reference-error
  (lambda (i binding-list)
    (cond ((null? binding-list) #f)
          (#t (if (memq i (cadr(car binding-list)))
              #t
              (check-reference-error i (cdr binding-list)))))))
             
                  ;(check-reference-error i (cdr binding-list)))))))|#





;;testing
;(parse-waee '(with ((x 2) (y (+ x 5)) (z (- 5 y))) (+ x y)))
;(parse-waee '(with ((x 3) (y 2)) (+ x y)))

;;subst: WAEE symbol WAEE --> WAEE
(define subst
  (lambda (e i v)
    (type-case WAEE e
      (num (n) (num n))
      ;(add (l r) (add (subst l i v) (subst r i v)))
      ;(sub (l r) (sub (subst l i v) (subst r i v)))
      (binop (op l r) (binop op (subst l i v) (subst r i v)))
      (with (bound-id named-expr bound-body)
            (if (symbol=? bound-id i)
                (with bound-id
                      (subst named-expr i v)
                      bound-body)
                (with bound-id
                      (subst named-expr i v)
                      (subst bound-body i v))))
      (id (name)
          (if (symbol=? name i) 
              v
              (id name))))))

;(parse-waee '(with ((x 2) (y (+ x 5))) (+ x y)))
;;interp-waee
;;WAEE --> value
;;evaluates WAEE expressions by reducing them to numbers
(define interp-waee
  (lambda (expr)
    (type-case WAEE expr
      (num (n) n)
      ;(add (l r) (+ (interp-waee l) (interp-waee r)))
      ;(sub (l r) (- (interp-waee l) (interp-waee r)))
      (binop (op-name l r) ((lookup op-name op-table)(interp-waee l) (interp-waee r)))
      (with (bound-id named-expr bound-body)
            (interp-waee (subst bound-body bound-id (num (interp-waee named-expr)))))
      (id (name) (error 'interp-waee"free id")))))

;;binop-rec
(define-type binop-recognizer
  (binop-rec (name symbol?) (op procedure?)))

;;lookup
(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (binop-rec-name (car op-table)) op-name)
                     (binop-rec-op (car op-table))
                     (lookup op-name (cdr op-table)))))))

;;op-table
(define op-table (list
                  (binop-rec 'plus +)
                  (binop-rec 'minus -)
                  (binop-rec 'mult *)
                  (binop-rec 'div /)))

;;eval-waee
;combine parser and interpreter---> value
(define eval-waee
  (lambda (sexp)
    (interp-waee (parse-waee sexp))))

;;testing
;(parse-waee '(with ((x 2) (y 3)) (+ x y)))
;(eval-waee '(with ((x 3) (x 4)) (+ x x)))

;(eval-waee 'with (v 5) (- 10 9))


(test (eval-waee '(with ((y 3) (x 4) (d 3) (a (with (v 5) (- 10 9))) (d 5)) (/ 8 (* x 5)))) 0)

(test (eval-waee '(with ((y 3) (x 4) (d 3)) (/ 8 (* x (with ((a (with (v 5) (- 10 9))) (a 7)) (- a a)))))) 0)

(test (eval-waee '(with ((y 3) (x 4) (d 3) (a (with (v 5) (- 10 9))) (c (+ 5 x))) (/ 8 (* x 5)))) 0)

;(1+8)/3 = 3
(test (eval-waee '(with ((y 3)(x (+ 1 8))) (/ x y))) 3)
;(3*8)/(5-2) = 24/3 =8
(test (eval-waee '(with (x (* 3 8)) (/ x (with (x 5) (- x 2))))) 8)
;(9-5)*7/(3*8) = 28/24 = 7/6 
(test (eval-waee '(with ((x (* 3 8)) (y (with ((z (with (a 5) (- 9 a))) (a 7)) (* z a)) )) (/ y x))) 7/6)
;(8/2) - ((9-5)*7/(3*8)) = 4 - 28/24 = 4 - 7/6 = 17/6
(test (eval-waee '(with ((x (* 3 8)) (y (with ((z (with (a 5) (- 9 a))) (a 7)) (* z a))) (z (/ 8 2))) (- z (/ y x)))) 17/6)
;(5/2) -(((10*2/5)+1+5)*7)/(3*8)+1 =5/2 - 70/24 +1= 5/2 -35/12 +1= 30/12 - 35/12 +1= - 5/12+1 = 7/12
(test (eval-waee '(with ((x (* 3 8)) (y (with ((z (with (a 5) (+ (with (c (with (d (with (e 10) (* e 2))) (/ d 5))) (+ c 1)) a))) (a 7)) (* z a))) (z (/ 5 2))) (+ (with (t 5) (/ 5 t)) (- z (/ y x))))) 7/12)

;;
(test (eval-waee '{with {{x 3}} {with {{x 2} {y {+ x 5}}} {+ x y}}}) 10)
(test (eval-waee '{with {{x 3}} {with {{x {with {x 2} {* x x}}} {y {+ x 5}}} {+ x y}}}) 12)
(test (eval-waee '{with {{x 3}} {with {{x {with {x 2} {* x x}}} {y {+ x 5}}} {with {z 10} {+ x {+ y z}}}}}) 22)
(test (eval-waee '{with {{u 3}} {with {{x {with {x 2} {* x {with {w 6} {+ u w}}}}} {y {+ u 5}}} {with {z 10} {+ x {+ y z}}}}}) 36)

#|(test (eval-waee '1) 1)
  (test (eval-waee '{+ 1 1}) 2)
  (test (eval-waee '{- 1 1}) 0)
  (test (eval-waee '{* 2 2}) 4)
  (test (eval-waee '{/ 4 2}) 2)
  (test (eval-waee '{with {{x 3}} {+ x x}}) 6)
  (test (eval-waee '{with {{x 3} {y 4}} {+ x y}}) 7)
  (test (eval-waee '{with {{x 3} {y 4}} {+ x y}}) 7)
  (test (eval-waee '{with {{x 3}} {with {{y 4}} {+ x y}}}) 7)
  (test (eval-waee '{with {{x 3}} {with {{y {+ x x}}} {+ x y}}}) 9)
  (test (eval-waee '{with {{x 3}} {with {{y {+ x x}}} {with {{x 1}} {+ x y}}}}) 7)
  (test (eval-waee '{with {{x 1} {y 2}} {with {{z x} {x x}} {with {{z {+ z 1}}} {+ z y}}}}) 4)|#

