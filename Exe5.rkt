#lang plai


;;CFWAE type
(define-type CFWAE
  (num (n number?))
  (id (name symbol?))
  (add (lhs CFWAE?) (rhs CFWAE?))
  (sub (lhs CFWAE?) (rhs CFWAE?))
  (mul (lhs CFWAE?) (rhs CFWAE?))
  (div (lhs CFWAE?) (rhs CFWAE?))
  (fun (param symbol?) (body CFWAE?))
  (app (fun-expr CFWAE?) (arg-expr CFWAE?))
  (if0 (test CFWAE?) (truth CFWAE?) (falsity CFWAE?))
  (with (name symbol?) (named-expr CFWAE?) (body CFWAE?)))


;;CFWAE-Value for funciton 
(define-type CFWAE-Value
  (numV (n number?))
  (closureV (arg symbol?)
            (body CFWAE?)
            (ds Env?)))

;;defer substitute
(define-type Env
  (mtSub)
  (aSub (name symbol?) (value CFWAE-Value?) (ds Env?)))

;;
;;lookup symbol x in a ds list
(define lookup
  (lambda (x ds)
    (type-case Env ds
      (mtSub () (error 'lookup "symbol not found"))
      (aSub (n v dsl)
            (if (symbol=? n x)
                v
                (lookup x dsl))))))

;;parse-cfwae
(define parse-cfwae
  (lambda (sexpr)
    (cond ((number? sexpr) (num sexpr))
          ((symbol? sexpr) (id sexpr))
          ((list? sexpr)
           (if (eqv? (length sexpr) 2)
               (app (parse-cfwae (car sexpr)) (parse-cfwae (cadr sexpr)))
               (case (car sexpr)
                 ((+) (add (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((-) (sub (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((*) (mul (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((/) (div (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr))))
                 ((fun) (fun (cadr sexpr) (parse-cfwae (caddr sexpr))))
                 ((if0) (if0 (parse-cfwae (cadr sexpr)) (parse-cfwae (caddr sexpr)) (parse-cfwae (cadddr sexpr))))
                 ((with) (with (caadr sexpr) (parse-cfwae (cadadr sexpr)) (parse-cfwae (caddr sexpr)))) 
                 (else (error 'parse-cfwae "parse-cfae error")))))
             ;(else (parse-cfae (car sexpr))))))
          (error 'parse-cfwae "parse-cfae error"))))



;;interp-cfwae
(define interp-cfwae
  (lambda (expr ds)
    (type-case CFWAE expr
      (num (n) (numV n))
      ;(binop (op-name l r) (op-cfae (lookupOp op-name op-table) (interp-cfwae l ds) (interp-cfae r ds)))
      (add (lhs rhs) (numV (+ (numV-n (interp-cfwae lhs ds)) (numV-n (interp-cfwae rhs ds)))))
      (sub (lhs rhs) (numV (- (numV-n (interp-cfwae lhs ds)) (numV-n (interp-cfwae rhs ds)))))
      (mul (lhs rhs) (numV (* (numV-n (interp-cfwae lhs ds)) (numV-n (interp-cfwae rhs ds)))))
      (div (lhs rhs) (numV (/ (numV-n (interp-cfwae lhs ds)) (numV-n (interp-cfwae rhs ds)))))
      (id (name) (lookup name ds))
      (fun (param body) (closureV param body ds))
      (app (fun-expr fun-arg)
           (local ((define fun-val (interp-cfwae fun-expr ds)))
             (interp-cfwae (closureV-body fun-val)
                          (aSub (closureV-arg fun-val)
                                (interp-cfwae fun-arg ds)
                                (closureV-ds fun-val)))))
      (if0 (test truth falsity) (if (equal? (numV 0) (interp-cfwae test ds))
                                    (interp-cfwae truth ds)
                                    (interp-cfwae falsity ds)))
      (with (bound-id named-expr bound-body)
            (local
              ((define fun-val (interp-cfwae (fun bound-id bound-body) ds)))
              (interp-cfwae (closureV-body fun-val)
                            (aSub (closureV-arg fun-val)
                                  (interp-cfwae named-expr ds)
                                  (closureV-ds fun-val))))))))


;;evaluation
(define (eval-cfwae expr)
  (interp-cfwae (parse-cfwae expr) (mtSub)))


;;----test
(test (eval-cfwae '{+ 1 2}) (numV 3))
(test (eval-cfwae '{+ 2 {* 2 3}}) (numV 8))
(test (eval-cfwae '{{fun x x} 3}) (numV 3))
(test (eval-cfwae '{{fun x {+ x 1}} 1}) (numV 2))
(test (eval-cfwae '{if0 0 1 2}) (numV 1))
(test (eval-cfwae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (numV 4))
(test (eval-cfwae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (numV 1))
(test (eval-cfwae '{with {x 10} {+ x 5}}) (numV 15))
(test (eval-cfwae '{with {f {fun x {+ x 1}}} {f 2}}) (numV 3))




