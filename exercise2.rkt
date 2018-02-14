#lang plai
;EECS 662
;Project 3
;Exercise 2
;Author: Lecheng Zheng
;KUID: 2741448
;4/23/2016

;;; Define an AST type for CFWAER constructs.
(define-type CFWAER
  (num (n number?))
  (add (lhs CFWAER?) (rhs CFWAER?))
  (sub (lhs CFWAER?) (rhs CFWAER?))
  (mul (lhs CFWAER?) (rhs CFWAER?))
  (div (lhs CFWAER?) (rhs CFWAER?))
  (id (name symbol?))
  (with (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (rec (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (if0 (cond CFWAER?) (tarm CFWAER?) (farm CFWAER?))
  (fun (arg-name symbol?) (body CFWAER?))
  (app (fun-expr CFWAER?)(arg CFWAER?)))


(define-type RCWFAE-value
  (numV (n number?))
  (closureV (id symbol?)
            (body CFWAER?)
            (env DefrdSub?)))

(define boxed-RCWFAE-value?
  (lambda (v)
    (and (box? v) (RCWFAE-value? (unbox v)))))

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?)(value RCWFAE-value?)(ds DefrdSub?))
  (aRecSub (name symbol?)
           (value boxed-RCWFAE-value?)
           (env DefrdSub?)))

;;; Define a parser for CFWAER constructs.  This parser does no error checking at all. Simply converts
;;; concrete syntax to AST.
;;; provided by Professor Perry Alexander
(define parse-cfwaer
  (lambda (expr)
    (cond ((symbol? expr) (id expr))
          ((number? expr) (num expr))
          ((list? expr)
           (case (car expr)
             ((-) (sub (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((+) (add (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((*) (mul (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((/) (div (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((with) (with (car (cadr expr)) 
                            (parse-cfwaer (cadr (cadr expr))) 
                            (parse-cfwaer (caddr expr))))
             ((rec) (rec (car (cadr expr)) 
                            (parse-cfwaer (cadr (cadr expr))) 
                            (parse-cfwaer (caddr expr))))
             ((if0) (if0 (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))
                         (parse-cfwaer (cadddr expr))))
             ((fun) (fun (cadr expr) (parse-cfwaer (caddr expr))))
             (else (app (parse-cfwaer (car expr)) (parse-cfwaer (cadr expr))))))
          (else 'parse-cfwaer "Unexpected token"))))


(define plusV
  (lambda (l r)
    (type-case RCWFAE-value l
      (numV (lv) (type-case RCWFAE-value r
                  (numV (rv) (numV (+ lv rv)))
                   (closureV (x y z) (error 'plusV "Error!"))))
      (closureV (x y z) (error 'plusV "error!")))))

(define minusV
  (lambda (l r)
    (type-case RCWFAE-value l
      (numV (lv) (type-case RCWFAE-value r
                  (numV (rv) (numV (- lv rv)))
                   (closureV (x y z) (error 'minusV "Error!"))))
      (closureV (x y z) (error 'minusV "error!")))))

(define mulV
  (lambda (l r)
    (type-case RCWFAE-value l
      (numV (lv) (type-case RCWFAE-value r
                  (numV (rv) (numV (* lv rv)))
                   (closureV (x y z) (error 'mulV "Error!"))))
      (closureV (x y z) (error 'mulV "error!")))))

(define divV
  (lambda (l r)
    (type-case RCWFAE-value l
      (numV (lv) (type-case RCWFAE-value r
                  (numV (rv) (numV (/ lv rv)))
                   (closureV (x y z) (error 'divV "Error!"))))
      (closureV (x y z) (error 'divV "error!")))))

(define (lookup-ds name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup-ds "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup-ds name rest-ds))]
    [aRecSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              (unbox bound-value)
              (lookup-ds name rest-ds))]))

;remove numV (numV n -> n)
(define rm-num
  (lambda (expr)
    (type-case RCWFAE-value expr
      (numV (n) n)
      (else (error 'rm-num "Error!")))))

(define interp-cfwae
  (lambda (expr env)
    (type-case CFWAER expr
      (num (n) (numV n))
      (add (l r) (plusV (interp-cfwae l env) (interp-cfwae r env)))
      (sub (l r) (minusV (interp-cfwae l env) (interp-cfwae r env)))
      (mul (l r) (mulV (interp-cfwae l env) (interp-cfwae r env)))
      (div (l r) (divV (interp-cfwae l env) (interp-cfwae r env)))
      (id (name) (lookup-ds name env))
      (fun (id body) (closureV id body env))
      (if0 (e v u) (if (equal? 0 (rm-num (interp-cfwae e env)))
                       (interp-cfwae v env)
                       (interp-cfwae u env)))
      (with (bound-id named-expr bound-body) (interp-cfwae (app (fun bound-id bound-body) named-expr) env))
      (app (fun-expr arg-expr) ;fun-val is the closure
           (local ((define fun-val (interp-cfwae fun-expr env))
                   (define arg-val (interp-cfwae arg-expr env))) ;lazy evaluation: change (interp arg-expr env))) to arg-expr
             (interp-cfwae (closureV-body fun-val) (aSub (closureV-id fun-val) arg-val (closureV-env fun-val)))))
      (rec (id named-expr body) (interp-cfwae body (cyclically-bind-and-interp id named-expr env))))))

(define cyclically-bind-and-interp ;cyclical bind and interpret
  (lambda (bound-id named-expr env)
    (local
      [(define value-holder (box (numV 1729))) ;we define a random number in the box
      (define new-env (aRecSub bound-id value-holder env))  ;we create a new environments
      (define named-expr-value (interp-cfwae named-expr new-env))] ;interp the named-expr with the new environment
    (begin (set-box! value-holder named-expr-value) new-env) ;since we get the value in the named-expr, we replace the number in the box
    )))

(define eval-cfwaer
  (lambda (sexpr)
    (if (CFWAER? (parse-cfwaer sexpr))
        (interp-cfwae (parse-cfwaer sexpr) (mtSub))
        (error 'eval-cfwae "Error: eval-cfwae fails!")
        )))


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