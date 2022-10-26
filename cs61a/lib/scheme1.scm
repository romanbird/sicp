;; Simple evaluator for Scheme without DEFINE, using substitution model.
;; Version 1: Adds first-class primitive procedures to Scheme-0.

;; The "read-eval-print loop":

(define (scheme-1)
  (display "Scheme-1: ")
  (flush)
  (print (eval-1 (read)))
  (scheme-1))

;; As in Scheme-0:
;; EVAL-1 takes an expression and returns its value.
;; APPLY-1 takes a procedure and a list of actual argument values, and
;;  calls the procedure.

;; Comments on EVAL-1 vs. EVAL-0:

;; There are the same four basic expression types:
;;    1. self-evaluating (a/k/a constant) expressions: numbers, #t, etc.
;;    2. symbols (variables)
;;    3. special forms (in this evaluator, just QUOTE, IF, and LAMBDA)
;;    4. procedure calls (can call a primitive or a LAMBDA-generated procedure)

;; 1.  The value of a constant is itself.  Changed in Scheme-1: An STk
;; procedure is considered a constant expression.  You can't type in
;; procedure values, but the value of a global variable can be a procedure,
;; and that value might get substituted for a parameter in the body of a
;; higher-order function such as MAP, so the evaluator has to be ready to
;; see a built-in procedure as an "expression."  We modify the procedure
;; CONSTANT? to add a check for (PROCEDURE? EXP).

;; 2.  As in Scheme-0, we never see a local variable name as an expression,
;; because they've been substituted away before we evaluate the body.

;; But in Scheme-1, we do have *global* variables, namely, the names of
;; Scheme primitive procedures.  We cheat a little by using STk's
;; EVAL to get the values of these variables.

;; 3.  The handling of special forms is unchanged.

;; 4.  In Scheme-0, the first element of a procedure call list (the one
;; that says what procedure to call) was not evaluated; it had to be
;; either a LAMBDA expression or a symbol (the name of a primitive).
;; In Scheme-1, the first element is evaluated, just as the others are;
;; the first argument to APPLY-1 will be an actual procedure (either an
;; STk procedure or a LAMBDA-expression representing a procedure).

(define (eval-1 exp)
  (cond ((constant? exp) exp)
	((symbol? exp) (EVAL EXP))	; use underlying Scheme's EVAL
	((quote-exp? exp) (cadr exp))
	((if-exp? exp)
	 (if (eval-1 (cadr exp))
	     (eval-1 (caddr exp))
	     (eval-1 (cadddr exp))))
	((lambda-exp? exp) exp)
	((pair? exp) (apply-1 (EVAL-1 (car exp))      ; eval the operator
			      (map eval-1 (cdr exp))))
	(else (error "bad expr: " exp))))


;; Comments on APPLY-1:

;; There are two kinds of procedures: primitive and LAMBDA-created.

;; We recognize a primitive procedure using the PROCEDURE? predicate in
;; the underlying STk interpreter.  (In Scheme-0, we called SYMBOL? to
;; recognize the *name* of the primitive procedure as our argument.)

;; If the procedure isn't primitive, then it must be LAMBDA-created.
;; These are handled as in Scheme-0.

(define (apply-1 proc args)
  (cond ((PROCEDURE? proc)	; use underlying Scheme's APPLY
	 (apply PROC args))
	((lambda-exp? proc)
	 (eval-1 (substitute (caddr proc)
			     (cadr proc)
			     args
			     '())))
	(else (error "bad proc: " proc))))


;; Some trivial helper procedures:

(define (constant? exp)
  (or (number? exp) (boolean? exp) (string? exp) (PROCEDURE? EXP)))

(define (exp-checker type)
  (lambda (exp) (and (pair? exp) (eq? (car exp) type))))

(define quote-exp? (exp-checker 'quote))
(define if-exp? (exp-checker 'if))
(define lambda-exp? (exp-checker 'lambda))


;; SUBSTITUTE substitutes actual arguments for *free* references to the
;; corresponding formal parameters.

;; The only change from Scheme-0 is in MAYBE-QUOTE, which must handle the
;; case of a primitive procedure as the actual argument value; these
;; procedures shouldn't be quoted.

(define (substitute exp params args bound)
  (cond ((constant? exp) exp)
	((symbol? exp)
	 (if (memq exp bound)
	     exp
	     (lookup exp params args)))
	((quote-exp? exp) exp)
	((lambda-exp? exp)
	 (list 'lambda
	       (cadr exp)
	       (substitute (caddr exp) params args (append bound (cadr exp)))))
	(else (map (lambda (subexp) (substitute subexp params args bound)) exp))))

(define (lookup name params args)
  (cond ((null? params) name)
	((eq? name (car params)) (maybe-quote (car args)))
	(else (lookup name (cdr params) (cdr args)))))

(define (maybe-quote value)
  (cond ((lambda-exp? value) value)
	((constant? value) value)
	((PROCEDURE? VALUE) VALUE)	; real Scheme primitive procedure
	(else (list 'quote value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample evaluation, using a primitive as argument to MAP:

; Scheme-1: ((lambda (f n)
;	       ((lambda (map) (map map f n))
;		   (lambda (map f n)
;		     (if (null? n)
;		         '()
;			 (cons (f (car n)) (map map f (cdr n))) )) ))
;	      first
;	      '(the rain in spain))
; (t r i s)
