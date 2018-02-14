#lang plai


;;CFWAE
(define-type CFWAE
  (numW (n number?))
  (idW (name symbol?))
  (binopW (op symbol?) (lhs CFWAE?) (rhs CFWAE?))
  (funW (param symbol?) (body CFWAE?))
  (if0W (test CFWAE?) (truth CFWAE?) (falsity CFWAE?))
  (withW (name symbol?) (bound-expr CFWAE?) (bound-body CFWAE?))
  (cond0W (binding CondBind?))
  (appW (fun-expr CFWAE?) (arg-expr CFWAE?)))

;;CondBind
(define-type CondBind
  (cond-else (expr CFWAE?))
  (cond-bind (test CFWAE?) (truth CFWAE?) (falsity CondBind?)))

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

;;parse-cfwae sex[r
(define parse-cfwae
  (lambda (sexpr)
    (cond ((number? sexpr) (numW sexpr))
          ((symbol? sexpr) (idW sexpr))
          ((list? sexpr)
           (if (eqv? (length sexpr) 2)
               (appW (parse-cfwae (car sexpr)) (parse-cfwae (cadr sexpr)))
               (case (car sexpr)
                 ((+) (binopW 'plus (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((-) (binopW 'minus (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((*) (binopW 'mult (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((/) (binopW 'div (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((fun) (funW (cadr sexpr) (parse-cfwae (caddr sexpr))))
                 ((app) (appW (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((if0) (if0W (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr)) (parse-cfwae (cadddr sexpr))))
                 ((with) (withW (car(cadr sexpr)) (parse-cfwae (cadr(cadr sexpr))) (parse-cfwae(caddr sexpr))))
                 ((cond0) (cond0W (parse-cond0 (cdr sexpr))))
                 ;((list?) (parse-cfwae (car sexpr))))))
                (else (error 'parse-cfwae "parse-cfae error")))))
             ;(else (parse-cfae (car sexpr))))))
          (error 'parse-cfwae "parse-cfae error"))))


;;parse-cond0
(define parse-cond0
  (lambda (binding)
    (cond ((null? binding) (error 'parse-cond0 "Error on parse-condtion"))
          ((equal? 1 (length binding)) (cond-else (parse-cfwae (car binding))))
          (else (cond-bind (parse-cfwae (car(car binding))) (parse-cfwae (second (car binding))) (parse-cond0 (cdr binding)))))))

;;elab-cfwae expr
(define elab-cfwae
  (lambda (expr)
    (type-case CFWAE expr
      (numW (n) (num n))
      (idW (i) (id i))
      (binopW (op l r) (binop op (elab-cfwae l) (elab-cfwae r)))
      (withW (i v e) (app (fun i (elab-cfwae e)) (elab-cfwae v)))
      (if0W (test truth falsity) (if0 (elab-cfwae test) (elab-cfwae truth) (elab-cfwae falsity)))
      (cond0W (binding) (elab-cond0W binding)) 
      (funW (i body) (fun i (elab-cfwae body)))
      (appW (f v) (app (elab-cfwae f) (elab-cfwae v))))))

;;elab-cond0W
(define elab-cond0W
  (lambda (binding)
    (type-case CondBind binding
      (cond-else (e) (elab-cfwae e))
      (cond-bind (c t r) (if0 (elab-cfwae c)
                              (elab-cfwae t)
                              (elab-cond0W r))))))


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

;;op-cfae
(define op-cfae
  (lambda (op lhs rhs)
    (num (op (num-n lhs) (num-n rhs)))))

;;interp-cfwae
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



;;prelude
(define prelude
  (aSub 'pi (num 3.1415926535897932384)
  (aSub 'square (fun 'x (binop 'mult (id 'x) (id 'x)))
  (aSub 'area (fun 'r (binop 'mult (id 'pi) (app (id 'square) (id 'r))))
  (aSub 'inc (fun 'x (binop 'plus (id 'x) (num 1))) (mtSub))))))
  


;;eval-cfwae expr
(define eval-cfwae
  (lambda (expr)
    (interp-cfae (elab-cfwae (parse-cfwae expr)) prelude)))

;;test
(test (eval-cfwae '{+ 1 2}) (num 3))
(test (eval-cfwae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfwae '{{fun x x} 3}) (num 3))
(test (eval-cfwae '{{fun x {+ x 1}} 1}) (num 2))
(test (eval-cfwae '{if0 0 1 2}) (num 1))
(test (eval-cfwae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfwae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfwae '{with {x 10} {+ x 5}}) (num 15))
(test (eval-cfwae '{with {f {fun x {+ x 1}}} {f 2}}) (num 3))
(test (eval-cfwae '{cond0 {1 2} {0 15} 0}) (num 15))
(test (eval-cfwae '{with {add1 {fun x {+ x 1}}} {cond0 {{add1 0} 5} {3 4} {0 {add1 2}} 52} 2}) (num 3))
(test (eval-cfwae '{inc pi}) (num 4.141592653589793))
(test (eval-cfwae '{with {x 2} {with {inc {fun x {+ x 2}}} {inc x}}}) (num 4))
(test (eval-cfwae '{area 2}) (num 12.566370614359172))
(test (eval-cfwae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))
(test (eval-cfwae '{with {g {fun f {f 3}}} {g {fun x {+ x 1}}}}) (num 4))

;;test result
#|
(good (eval-cfwae '(+ 1 2)) (num 3) (num 3) "at line 158")
(good (eval-cfwae '(+ 2 (* 2 3))) (num 8) (num 8) "at line 159")
(good (eval-cfwae '((fun x x) 3)) (num 3) (num 3) "at line 160")
(good (eval-cfwae '((fun x (+ x 1)) 1)) (num 2) (num 2) "at line 161")
(good (eval-cfwae '(if0 0 1 2)) (num 1) (num 1) "at line 162")
(good (eval-cfwae '(if0 ((fun x (- x 2)) 3) ((fun x (* 2 x)) 10) ((fun x (/ x 2)) 8))) (num 4) (num 4) "at line 163")
(good (eval-cfwae '((if0 0 (fun x (+ x 1)) (fun x (+ x 2))) 0)) (num 1) (num 1) "at line 164")
(good (eval-cfwae '(with (x 10) (+ x 5))) (num 15) (num 15) "at line 165")
(good (eval-cfwae '(with (f (fun x (+ x 1))) (f 2))) (num 3) (num 3) "at line 166")
(good (eval-cfwae '(cond0 (1 2) (0 15) 0)) (num 15) (num 15) "at line 167")
(good (eval-cfwae '(with (add1 (fun x (+ x 1))) (cond0 ((add1 0) 5) (3 4) (0 (add1 2)) 52) 2)) (num 3) (num 3) "at line 168")
(good (eval-cfwae '(inc pi)) (num 4.141592653589793) (num 4.141592653589793) "at line 169")
(good (eval-cfwae '(with (x 2) (with (inc (fun x (+ x 2))) (inc x)))) (num 4) (num 4) "at line 170")
(good (eval-cfwae '(area 2)) (num 12.566370614359172) (num 12.566370614359172) "at line 171")
(good (eval-cfwae '((fun x ((fun y (+ x y)) 3)) 1)) (num 4) (num 4) "at line 172")
(good (eval-cfwae '(with (g (fun f (f 3))) (g (fun x (+ x 1))))) (num 4) (num 4) "at line 173")
|#


#|((fun (x)    
      (with (i (fun (n) (- n x)))
            (with (y (i 1))
                  (with (x (* (i y) (i x)))
                        (i x))))) 5)|#


(eval-cfwae '(with (x 3)    
                  (+ (with (x 2) (+ x 3)) 
                     x)))