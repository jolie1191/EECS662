#lang plai

;;CFWAER type
(define-type CFWAER
  (num (n number?))
  (id (name symbol?))
  (add (lhs CFWAER?) (rhs CFWAER?))
  (sub (lhs CFWAER?) (rhs CFWAER?))
  (mul (lhs CFWAER?) (rhs CFWAER?))
  (div (lhs CFWAER?) (rhs CFWAER?))
  (fun (param symbol?) (body CFWAER?))
  (app (fun-expr CFWAER?) (arg-expr CFWAER?))
  (if0 (test CFWAER?) (truth CFWAER?) (falsity CFWAER?))
  (with (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (rec (name symbol?) (named-expr CFWAER?) (body CFWAER?)))

;;CFWAER-Value for funciton 
(define-type CFWAER-Value
  (numV (n number?))
  (closureV (arg symbol?)
            (body CFWAER?)
            (ds Env?)))

;;defer substitute
(define-type Env
  (mtSub)
  (aSub (name symbol?) (value CFWAER-Value?) (ds Env?))
  (aRecSub (name symbol?) (value boxed-CFWAER-Value?) (env Env?)))

;;boxed-CFWAER-Value
(define (boxed-CFWAER-Value? v)
  (and (box? v)
       (CFWAER-Value? (unbox v))))


;;lookup symbol x in a ds list
(define lookup
  (lambda (x ds)
    (type-case Env ds
      (mtSub () (error 'lookup "symbol not found"))
      (aSub (n v dsl)
            (if (symbol=? n x)
                v
                (lookup x dsl)))
      (aRecSub (bound-name bound-value rest-env)
               (if (symbol=? bound-name x)
                   (unbox bound-value)
                   (lookup x rest-env))))))


;;parse-cfwaer
(define parse-cfwaer
  (lambda (sexpr)
    (cond ((number? sexpr) (num sexpr))
          ((symbol? sexpr) (id sexpr))
          ((list? sexpr)
           (if (eqv? (length sexpr) 2)
               (app (parse-cfwaer (car sexpr)) (parse-cfwaer (cadr sexpr)))
               (case (car sexpr)
                 ((+) (add (parse-cfwaer (cadr sexpr)) (parse-cfwaer (caddr sexpr))))
                 ((-) (sub (parse-cfwaer (cadr sexpr)) (parse-cfwaer (caddr sexpr))))
                 ((*) (mul (parse-cfwaer (cadr sexpr)) (parse-cfwaer (caddr sexpr))))
                 ((/) (div (parse-cfwaer (cadr sexpr)) (parse-cfwaer (caddr sexpr))))
                 ((fun) (fun (cadr sexpr) (parse-cfwaer (caddr sexpr))))
                 ((if0) (if0 (parse-cfwaer (cadr sexpr)) (parse-cfwaer (caddr sexpr)) (parse-cfwaer (cadddr sexpr))))
                 ((with) (with (caadr sexpr) (parse-cfwaer (cadadr sexpr)) (parse-cfwaer (caddr sexpr))))
                 ((rec) (rec (caadr sexpr) (parse-cfwaer (cadadr sexpr)) (parse-cfwaer (caddr sexpr))))
                 (else (error 'parse-cfwaer "parse-cfae error")))))
             ;(else (parse-cfae (car sexpr))))))
          (error 'parse-cfwaer "parse-cfae error"))))

;;cyclically-bind-and-interp
(define (cyclically-bind-and-interp name expr env)
  (local ((define value-holder (box (numV 1729)))
          (define new-env (aRecSub name value-holder env))
          (define named-expr (interp-cfwaer expr new-env)))
    (begin
      (set-box! value-holder named-expr) new-env)))


;;interp-cfwaer
(define interp-cfwaer
  (lambda (expr ds)
    (type-case CFWAER expr
      (num (n) (numV n))
      ;(binop (op-name l r) (op-cfae (lookupOp op-name op-table) (interp-cfwae l ds) (interp-cfae r ds)))
      (add (lhs rhs) (numV (+ (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
      (sub (lhs rhs) (numV (- (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
      (mul (lhs rhs) (numV (* (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
      (div (lhs rhs) (numV (/ (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
      (id (name) (lookup name ds))
      (fun (param body) (closureV param body ds))
      (app (fun-expr fun-arg)
           (local ((define fun-val (interp-cfwaer fun-expr ds)))
             (interp-cfwaer (closureV-body fun-val)
                          (aSub (closureV-arg fun-val)
                                (interp-cfwaer fun-arg ds)
                                (closureV-ds fun-val)))))
      (if0 (test truth falsity) (if (equal? (numV 0) (interp-cfwaer test ds))
                                    (interp-cfwaer truth ds)
                                    (interp-cfwaer falsity ds)))
      (with (bound-id named-expr bound-body)
            (local
              ((define fun-val (interp-cfwaer (fun bound-id bound-body) ds)))
              (interp-cfwaer (closureV-body fun-val)
                            (aSub (closureV-arg fun-val)
                                  (interp-cfwaer named-expr ds)
                                  (closureV-ds fun-val)))))
      (rec (name named-expr body)
        (interp-cfwaer body (cyclically-bind-and-interp name named-expr ds))))))


;;evaluation
(define (eval-cfwaer expr)
  (interp-cfwaer (parse-cfwaer expr) (mtSub)))


;;---test---
(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaer '{with {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 0}})(numV 1))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 3}})(numV 6))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 5}})(numV 120))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 1} 1}})(numV 3))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 2} 2}})(numV 7))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 3}})(numV 61))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 0} 3}})(numV 4))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 0}})(numV 5))
(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {rec {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {rec {f {fun x {+ y x}}} {rec {y 100} {f 3}}}}) (numV 4))