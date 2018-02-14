
#lang plai

;abstract syntax definition
(define-type CFWAER
  (num (n number?))
  (id (name symbol?))
  (add (lhs CFWAER?) (rhs CFWAER?))
  (sub (lhs CFWAER?) (rhs CFWAER?))
  (mul (lhs CFWAER?) (rhs CFWAER?))
  (div (lhs CFWAER?) (rhs CFWAER?))
  (fun (param symbol?) (body CFWAER?))
  (app (fun-expr CFWAER?) (arg-expr CFWAER?))
  (if0 (c CFWAER?) (t CFWAER?) (e CFWAER?))
  (with (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (rec (name symbol?) (named-expr CFWAER?) (body CFWAER?)))

;CFWAE-Value definition
(define-type CFWAER-Value
  (numV (n number?))
  (closureV (arg symbol?)
            (body CFWAER?)
            (env Env?)))

;boxed CFWAE-Value definition
(define (boxed-CFWAER-Value? v)
  (and (box? v)
       (CFWAER-Value? (unbox v))))

;recursive environment definition
(define-type Env
  (mtSub)
  (aSub (name symbol?)
        (value CFWAER-Value?)
        (ds Env?))
  (aRecSub (name symbol?) (value boxed-CFWAER-Value?) (env Env?)))

;lookup function for substitution list
(define (lookup name ds)
  (type-case Env ds 
    (mtSub () (error 'lookup "no binding for identifier"))
    (aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds)))
    (aRecSub (bound-name bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox bound-value)
                 (lookup bound-name rest-env)))))

;parser function
(define (parse-cfwaer expr)
  (cond ((symbol? expr) (id expr))
        ((number? expr) (num expr))
        ((list? expr)
         (case (car expr)
           ((+) (add (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
           ((-) (sub (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
           ((*) (mul (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
           ((/) (div (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
           ((if0) (if0 (parse-cfwaer (cadr expr))
                       (parse-cfwaer (caddr expr))
                       (parse-cfwaer (cadddr expr))))
           ((with) (with (caadr expr)
                         (parse-cfwaer (cadadr expr))
                         (parse-cfwaer (caddr expr))))
           ((fun) (fun (caadr expr) (parse-cfwaer (caddr expr))))
           ((rec) (rec (caadr expr)
                    (parse-cfwaer (cadadr expr))
                    (parse-cfwaer (caddr expr))))
           (else (app (parse-cfwaer (car expr)) (parse-cfwaer (cadr expr))))))
        (else 'parse-cfwaer "unexpected token")))

;cyclically-bind-and-interp function
(define (cyc-bind-interp name expr env)
  (local ((define value-holder (box (numV 1729)))
          (define new-env (aRecSub name value-holder env))
          (define named-expr (interp-cfwaer expr new-env)))
    (begin
      (set-box! value-holder named-expr) new-env)))

;interpreter function
(define (interp-cfwaer expr ds)
  (type-case CFWAER expr
    (num (n) (numV n))
    (id (name) (lookup name ds))
    (add (lhs rhs) (numV (+ (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
    (sub (lhs rhs) (numV (- (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
    (mul (lhs rhs) (numV (* (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
    (div (lhs rhs) (numV (/ (numV-n (interp-cfwaer lhs ds)) (numV-n (interp-cfwaer rhs ds)))))
    (if0 (c t e) (cond ((= (numV-n (interp-cfwaer c ds)) 0)
                        (interp-cfwaer t ds))
                       (else (interp-cfwaer e ds))))
    (with (bound-id named-expr bound-body) 
          (local
            ((define fun-val (interp-cfwaer (fun bound-id bound-body) ds)))
            (interp-cfwaer (closureV-body fun-val)
                          (aSub (closureV-arg fun-val)
                                (interp-cfwaer named-expr ds)
                                (closureV-env fun-val)))))
    (fun (bound-id bound-body) (closureV bound-id bound-body ds))
    (app (fun-expr arg-expr)
         (local
           ((define fun-app (interp-cfwaer fun-expr ds)))
           (interp-cfwaer (closureV-body fun-app)
                         (aSub (closureV-arg fun-app)
                               (interp-cfwaer arg-expr ds)
                               (closureV-env fun-app)))))
    (rec (name named-expr body)
      (interp-cfwaer body (cyc-bind-interp name named-expr ds)))))

;factorial example from lecture
(define fac5
  '(rec (fac (fun (n) (if0 n 1 (* n (fac (+ n -1)))))) (fac 5)))

;evaluation function
(define (eval-cfwaer expr)
  (interp-cfwaer (parse-cfwaer expr) (mtSub)))



(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
