#lang plai


;;Define types: for representing the abstract syntax, values, environment, and store associated the CFWAES language.

;;define type for abstract syntax
(define-type CFWAES
  (num (n number?))
  (id (name symbol?))
  (binop (op symbol?) (lhs CFWAES?) (rhs CFWAES?))
  (if0 (test CFWAES?) (truth CFWAES?) (falsity CFWAES?))
  (app (fun-expr CFWAES?) (arg-expr CFWAES?))
  (fun (param symbol?) (body CFWAES?))
  (with (bound-id symbol?) (bound-expr CFWAES?) (bound-body CFWAES?))
  (seq (expr0 CFWAES?) (expr1 CFWAES?))
  (assign (id symbol?) (expr CFWAES?)))

;;define type for environment
(define-type Env
  (mtSub)
  (aSub (name symbol?) (loc number?) (rest Env?)))

;;define type for Store
(define-type Store
  (mtSto)
  (aSto (loc number?) (value CFWAES-Value?) (rest Store?)))


;;define type for CFWAES-Value
(define-type CFWAES-Value
  (numV (n number?))
  (closureV (param symbol?) (body CFWAES?) (env Env?)))


;;define type for Store associated with CFWAES-Vlaue language
(define-type Value*Store
  (v*s (value CFWAES-Value?) (store Store?)))


;;define type for binop (add,sub, mul, div)
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


;;calOp-cfwaes
(define calcalOp-cfwaes
  (lambda (op lhs rhs)
    (numV (op (numV-n lhs) (numV-n rhs)))))


;;parse-cfwaes accpts the concrete syntax and parse to CFWAES abstract syntax
(define parse-cfwaes
  (lambda (sexpr)
    (cond ((number? sexpr) (num sexpr))
          ((symbol? sexpr) (id sexpr))
          ((list? sexpr)
           (if (eqv? (length sexpr) 2)
               (app (parse-cfwaes (car sexpr)) (parse-cfwaes (cadr sexpr)))
               (case (car sexpr)
                 ((+) (binop 'plus (parse-cfwaes (cadr sexpr)) (parse-cfwaes (caddr sexpr))))
                 ((-) (binop 'minus (parse-cfwaes (cadr sexpr)) (parse-cfwaes (caddr sexpr))))
                 ((*) (binop 'mult (parse-cfwaes (cadr sexpr)) (parse-cfwaes (caddr sexpr))))
                 ((/) (binop 'div (parse-cfwaes (cadr sexpr)) (parse-cfwaes (caddr sexpr))))
                 ((fun) (fun (cadr sexpr) (parse-cfwaes (caddr sexpr))))
                 ((if0) (if0 (parse-cfwaes (cadr sexpr)) (parse-cfwaes (caddr sexpr)) (parse-cfwaes (cadddr sexpr))))
                 ((with) (with (caadr sexpr) (parse-cfwaes (cadadr sexpr)) (parse-cfwaes (caddr sexpr))))
                 ((assign) (assign (cadr sexpr) (parse-cfwaes (caddr sexpr))))
                 ((seq) (seq (parse-cfwaes (cadr sexpr)) (parse-cfwaes (caddr sexpr))))
                 (else (error parse-cfwaes "parse-cfae error")))))
          (error parse-cfwaes "parse-cfae error"))))



;;store-lookup: given location -> to find corresponding value
(define (store-lookup loc-index sto)
  (type-case Store sto
    (mtSto () (error 'store-lookup"no value at location"))
    (aSto (location value rest-store)
          (if (equal? location loc-index)
              value
              (store-lookup loc-index rest-store)))))

;;env-lookup: given variable name -> return corresponding location
(define (env-lookup name env)
  (type-case Env env
    (mtSub () (error 'env-lookup"no binding for identifier"))
    (aSub (bound-name bound-location rest-env)
          (if (symbol=? bound-name name)
              bound-location
              (env-lookup name rest-env)))))
 

;;define new-location
(define new-loc
  (local((define loc (box 0))) 
    (lambda ()
      (begin (set-box! loc (+ (unbox loc) 1)) (unbox loc)))))



;;interp-cfwaes take CFWAES data structure + Env + Store -> Value*Store
(define interp-cfwaes
  (lambda (expr env sto)
    (type-case CFWAES expr
      (num (n) (v*s (numV n) sto))
      (id (name) (v*s (store-lookup (env-lookup name env) sto)sto))
      ;(binop (op lhs rhs) ;(calcalOp-cfwaes (lookupOp op-name op-table) ;;(interp-cfwaes lhs env sto) (interp-cfwaes rhs env sto)))
      (binop (op lhs rhs)
             (type-case Value*Store (interp-cfwaes lhs env sto)
               (v*s (lhs-value lhs-store)
                    (type-case Value*Store (interp-cfwaes rhs env lhs-store)
                      (v*s (rhs-value rhs-store)
                           (v*s (calcalOp-cfwaes (lookupOp op op-table) lhs-value rhs-value) rhs-store))))))
      (fun (bound-id bound-body) (v*s (closureV bound-id bound-body env) sto))
      (if0 (test truth falsity)
           (type-case Value*Store (interp-cfwaes test env sto)
             (v*s (test-value test-store)
                  (if (equal? (numV 0) test-value)
                      (interp-cfwaes truth env test-store)
                      (interp-cfwaes falsity env test-store)))))
      (app (fun-expr arg-expr)
           (type-case Value*Store (interp-cfwaes fun-expr env sto)
             (v*s (fun-value fun-store)
                  (type-case Value*Store (interp-cfwaes arg-expr env fun-store)
                    (v*s (arg-value arg-store)
                         (local ((define new-location (new-loc)))
                           (interp-cfwaes (closureV-body fun-value)
                                   (aSub (closureV-param fun-value)
                                         new-location
                                         (closureV-env fun-value))
                                   (aSto new-location arg-value arg-store))))))))
      (seq (e1 e2)
            (type-case Value*Store (interp-cfwaes e1 env sto)
              (v*s (e1-value e1-store)
                   (interp-cfwaes e2 env e1-store))))
      (assign (i expr) (type-case Value*Store (interp-cfwaes expr env sto)
                         (v*s (expr-value expr-store)
                              (v*s expr-value (aSto (env-lookup i env) expr-value expr-store)))))
      (with (id expr body) (interp-cfwaes (app (fun id body) expr) env sto)))))


;;eval-cfwaes
(define eval-cfwaes
  (lambda (expr)
    (v*s-value (interp-cfwaes (parse-cfwaes expr) (mtSub) (mtSto)))))
      

;;----test---
(test (eval-cfwaes '{+ 5 6}) (numV 11))
(test (eval-cfwaes '{+ 2 {* 1 2}}) (numV 4))
(test (eval-cfwaes '{with {x 2} {+ x 5}}) (numV 7))
(test (eval-cfwaes '{with {f {fun x {+ x 1}}} {f 3}}) (numV 4))
(test (eval-cfwaes '{if0 0 1 2}) (numV 1))
(test (eval-cfwaes '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaes '{with {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaes '{with {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaes '{with {y 1} {with {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaes '{with {x 3} {seq {assign x {+ x 1}} {assign x {+ x 1}}}}) (numV 5))
(test (eval-cfwaes '{with {x 1} {seq {seq {assign x {+ x 1}} {assign x {+ x 1}}} {assign x {+ x 1}}}}) (numV 4))
(test (eval-cfwaes '{with {x 6} {seq x {assign x {+ x 1}}}}) (numV 7))
(test (eval-cfwaes '{with {y 1} {with {inc {fun x {+ x y}}}{inc 3}}}) (numV 4))
(test (eval-cfwaes '{with {y 1} {with {inc {fun x {+ x y}}}{seq {assign y 2} {inc 3}}}}) (numV 5))
(test (eval-cfwaes '{with {y 1} {with {inc {seq {assign y 2} {fun x {+ x y}}}}{inc 3}}}) (numV 5))
(test (eval-cfwaes '{with {x 3} {seq {seq {assign x {+ x 1}} {assign x {+ x 1}}} {assign x {+ x 1}}}}) (numV 6))
(test (eval-cfwaes '{with {y 0} {with {inc {fun x {+ x 1}}} {seq {seq {assign y {inc y}}{assign y {inc y}}} {seq {assign y {inc y}} {assign y {inc y}}}}}}) (numV 4))



;;--test result--
#|
(good (eval-cfwaes '(+ 5 6)) (numV 11) (numV 11) "at line 166")
(good (eval-cfwaes '(+ 2 (* 1 2))) (numV 4) (numV 4) "at line 167")
(good (eval-cfwaes '(with (x 2) (+ x 5))) (numV 7) (numV 7) "at line 168")
(good (eval-cfwaes '(with (f (fun x (+ x 1))) (f 3))) (numV 4) (numV 4) "at line 169")
(good (eval-cfwaes '(if0 0 1 2)) (numV 1) (numV 1) "at line 170")
(good (eval-cfwaes '((fun x (+ 1 3)) 1)) (numV 4) (numV 4) "at line 171")
(good (eval-cfwaes '(with (y 1) ((fun x (+ y x)) 3))) (numV 4) (numV 4) "at line 172")
(good (eval-cfwaes '(with (y 1) (with (f (fun x (+ y x))) (f 3)))) (numV 4) (numV 4) "at line 173")
(good (eval-cfwaes '(with (y 1) (with (f (fun x (+ y x))) (with (y 100) (f 3))))) (numV 4) (numV 4) "at line 174")
(good (eval-cfwaes '(with (x 3) (seq (assign x (+ x 1)) (assign x (+ x 1))))) (numV 5) (numV 5) "at line 175")
(good (eval-cfwaes '(with (x 1) (seq (seq (assign x (+ x 1)) (assign x (+ x 1))) (assign x (+ x 1))))) (numV 4) (numV 4) "at line 176")
(good (eval-cfwaes '(with (x 6) (seq x (assign x (+ x 1))))) (numV 7) (numV 7) "at line 177")
(good (eval-cfwaes '(with (y 1) (with (inc (fun x (+ x y))) (inc 3)))) (numV 4) (numV 4) "at line 178")
(good (eval-cfwaes '(with (y 1) (with (inc (fun x (+ x y))) (seq (assign y 2) (inc 3))))) (numV 5) (numV 5) "at line 179")
(good (eval-cfwaes '(with (y 1) (with (inc (seq (assign y 2) (fun x (+ x y)))) (inc 3)))) (numV 5) (numV 5) "at line 180")
(good (eval-cfwaes '(with (x 3) (seq (seq (assign x (+ x 1)) (assign x (+ x 1))) (assign x (+ x 1))))) (numV 6) (numV 6) "at line 181")
(good (eval-cfwaes '(with (y 0) (with (inc (fun x (+ x 1))) (seq (seq (assign y (inc y)) (assign y (inc y))) (seq (assign y (inc y)) (assign y (inc y))))))) (numV 4) (numV 4) "at line 182")
|#