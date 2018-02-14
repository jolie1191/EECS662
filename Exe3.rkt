#lang plai

;;CFAE
(define-type CFAE
  (num (n number?))
  (id (name symbol?))
  (binop (op symbol?) (lhs CFAE?) (rhs CFAE?))
  (fun (param symbol?) (body CFAE?))
  (if0 (test CFAE?) (truth CFAE?) (falsity CFAE?))
  (app (fun-expr CFAE?) (arg-expr CFAE?)))

;;binop-rec
(define-type binop-recognizer
  (binop-rec (name symbol?) (op procedure?)))


;;lookup operators 
(define lookupOp
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookupOp "Operator not found"))
           (else (if (symbol=? (binop-rec-name (car op-table)) op-name)
                     (binop-rec-op (car op-table))
                     (lookupOp op-name (cdr op-table)))))))

;;op-table, operators table
(define op-table (list
                  (binop-rec 'plus +)
                  (binop-rec 'minus -)
                  (binop-rec 'mult *)
                  (binop-rec 'div /)))

;;Type of defer-substitution
(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?)))

;;lookup symbol x in a ds list
(define lookup
  (lambda (x ds)
    (type-case DefrdSub ds
      (mtSub () (error 'lookup "symbol not found"))
      (aSub (n v dsl)
            (if (symbol=? n x)
                v
                (lookup x dsl))))))

;;parse-cfae sexpr
(define parse-cfae
  (lambda (sexpr)
    (cond ((number? sexpr) (num sexpr))
          ((symbol? sexpr) (id sexpr))
          ((list? sexpr)
           (if (eqv? (length sexpr) 2)
               (app (parse-cfae (car sexpr)) (parse-cfae (cadr sexpr)))
               (case (car sexpr)
                 ((+) (binop 'plus (parse-cfae (cadr sexpr)) (parse-cfae (caddr sexpr))))
                 ((-) (binop 'minus (parse-cfae (cadr sexpr)) (parse-cfae (caddr sexpr))))
                 ((*) (binop 'mult (parse-cfae (cadr sexpr)) (parse-cfae (caddr sexpr))))
                 ((/) (binop 'div (parse-cfae (cadr sexpr)) (parse-cfae (caddr sexpr))))
                 ((fun) (fun (cadr sexpr) (parse-cfae (caddr sexpr))))
                 ;((app) (app (parse-cfae (cadr sexpr)) (parse-cfae (caddr sexpr))))
                 ((if0) (if0 (parse-cfae (cadr sexpr)) (parse-cfae (caddr sexpr)) (parse-cfae (cadddr sexpr))))
                 ;((list?) (parse-cfae (car sexpr)))
                 (else (error 'parse-cfae "parse-cfae error")))))
             ;(else (parse-cfae (car sexpr))))))
          (error 'parse-cfae "parse-cfae error"))))

;;interp-cfae expr ds
(define interp-cfae
  (lambda (expr ds)
    (type-case CFAE expr
      (num (n) (num n))
      (binop (op-name l r) (op-cfae (lookupOp op-name op-table) (interp-cfae l ds) (interp-cfae r ds)))
      (id (name) (lookup name ds))
      (fun (param body) (fun param body))
      (app (fun-expr fun-arg)
           (local ((define fun-val (interp-cfae fun-expr ds)))
             (interp-cfae (fun-body fun-val)
                          (aSub (fun-param fun-val)
                                (interp-cfae fun-arg ds)
                                ds))))
      (if0 (test truth falsity) (if (equal? (num 0) (interp-cfae test ds))
                                    (interp-cfae truth ds)
                                    (interp-cfae falsity ds))))))

;;op-cfae
(define op-cfae
  (lambda (op lhs rhs)
    (num (op (num-n lhs) (num-n rhs)))))

;;eval-cfae sexpr
(define eval-cfae
  (lambda (sexpr)
    (interp-cfae (parse-cfae sexpr) (mtSub))))



;;test
(test (eval-cfae '{+ 1 2}) (num 3))
(test (eval-cfae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfae '{{fun x x} 3}) (num 3))
(test (eval-cfae '{{fun x {+ x 1} } 1}) (num 2))
(test (eval-cfae '{if0 0 1 2}) (num 1))
(test (eval-cfae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))

;;test result
#|
(good (eval-cfae '(+ 1 2)) (num 3) (num 3) "at line 99")
(good (eval-cfae '(+ 2 (* 2 3))) (num 8) (num 8) "at line 100")
(good (eval-cfae '((fun x x) 3)) (num 3) (num 3) "at line 101")
(good (eval-cfae '((fun x (+ x 1)) 1)) (num 2) (num 2) "at line 102")
(good (eval-cfae '(if0 0 1 2)) (num 1) (num 1) "at line 103")
(good (eval-cfae '(if0 ((fun x (- x 2)) 3) ((fun x (* 2 x)) 10) ((fun x (/ x 2)) 8))) (num 4) (num 4) "at line 104")
(good (eval-cfae '((if0 0 (fun x (+ x 1)) (fun x (+ x 2))) 0)) (num 1) (num 1) "at line 105")
(good (eval-cfae '((fun x ((fun y (+ x y)) 3)) 1)) (num 4) (num 4) "at line 106")
|#

