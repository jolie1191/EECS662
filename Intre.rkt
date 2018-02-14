#lang plai

(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (sub (lhs AE?) (rhs AE?))
 ; (with (name symbol?) (name-expr AE?) (body AE?))
 ;(id (name symbols?))
  )


(define calc
  (lambda (an-ae)
    (type-case AE an-ae
      (num (n) n)
      (add (l r) (+ (calc l) (calc r)))
      (sub (l r) (- (calc l) (calc r)))
     ; (with (id value exp))
      ;(id (name))
      )))

(define parse
  (lambda (sexp)
    (cond ((number? sexp) (num sexp))
          ((list? sexp)
           (case (car sexp)
             ((+) (add (parse (cadr sexp)) (parse (caddr sexp))))
             ((-) (sub (parse (cadr sexp)) (parse (caddr sexp)))))))))

(define interp-AE
  (lambda (sexp)
    (calc (parse sexp))))
  
                  
                     
           