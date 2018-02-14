#lang plai

;1. Write an type for representing
;   the abstract syntax of the WAE language using define-type.
(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (name symbol?) (named-expr WAE?) (body WAE?))
  (id (name symbol?)))

;parser: sexp ---> WAE
;To convert s-expressions into WAEs
(define parse-wae
  (lambda (sexp)
    (cond ((number? sexp) (num sexp))
          ((symbol? sexp) (id sexp))
          ((list? sexp)
           (case (car sexp)
             ((+) (add (parse-wae (cadr sexp)) (parse-wae (caddr sexp))))
             ((-) (sub (parse-wae (cadr sexp)) (parse-wae (caddr sexp))))
             ((with) (with (car(cadr sexp)) (parse-wae (cadr(cadr sexp))) (parse-wae(caddr sexp))))))
          (error 'parse-wae"parse-wae Error" ))))




;;subst: WAE symbol WAE --> WAE


(define subst
  (lambda (e i v)
    (type-case WAE e
      (num (n) (num n))
      (add (l r) (add (subst l i v) (subst r i v)))
      (sub (l r) (sub (subst l i v) (subst r i v)))
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




;;interp-wae   == calc
;;WAE --> value
;;evaluates WAE expressions by reducing them to numbers
(define interp-wae
  (lambda (expr)
    (type-case WAE expr
      (num (n) n)
      (add (l r) (+ (interp-wae l) (interp-wae r)))
      (sub (l r) (- (interp-wae l) (interp-wae r)))
      (with (bound-id named-expr bound-body)
            (interp-wae (subst bound-body bound-id (num (interp-wae named-expr)))))
      (id (name) (error 'interp-wae"free id")))))
            

;combine parser and interpreter---> value
(define eval-wae
  (lambda (sexp)
    (interp-wae (parse-wae sexp))))

;;Testing
(test (eval-wae '1) 1)
(test (eval-wae '{+ 1 1}) 2)
(test (eval-wae '{- 1 1}) 0)
(test (eval-wae '{with {x 3} {+ x x}}) 6)
(test (eval-wae '{with {x 3} {with {y 4} {+ x y}}}) 7)
(test (eval-wae '{with {x 3} {with {y 4} {+ x y}}}) 7)
(test (eval-wae '{with {x 3} {with {y {+ x x}} {+ x y}}}) 9)
(test (eval-wae '{with {x 3} {with {y {+ x x}} {with {x 1} {+ x y}}}}) 7)
(test (eval-wae '{with {x 1} {with {y 2} {with {z x} {with {x x} {with {z {+ z 1}} {+ z y}}}}}}) 4)

#|;;Test result
(good (eval-wae '1) 1 1 "at line 75")
(good (eval-wae '(+ 1 1)) 2 2 "at line 76")
(good (eval-wae '(- 1 1)) 0 0 "at line 77")
(good (eval-wae '(with (x 3) (+ x x))) 6 6 "at line 78")
(good (eval-wae '(with (x 3) (with (y 4) (+ x y)))) 7 7 "at line 79")
(good (eval-wae '(with (x 3) (with (y 4) (+ x y)))) 7 7 "at line 80")
(good (eval-wae '(with (x 3) (with (y (+ x x)) (+ x y)))) 9 9 "at line 81")
(good (eval-wae '(with (x 3) (with (y (+ x x)) (with (x 1) (+ x y))))) 7 7 "at line 82")
(good (eval-wae '(with (x 1) (with (y 2) (with (z x) (with (x x) (with (z (+ z 1)) (+ z y))))))) 4 4 "at line 83")|#



