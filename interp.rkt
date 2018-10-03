#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser. El
;; intérprete requiere un ambiente de evaluación en esta versión para buscar el valor de los 
;; identificadores.
;; interp: ERCFBAEL/L Env -> ERCFBAEL/L-Value
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(op f lst) (let* ([li (map-interp lst env)]
                       [e (find-op li)]
                       [s (strict (first li))]
                       [sm (map strict (rest li))])
                  (cond
                    [(exceptionV? li) li]
                    [(exceptionV? e) e]
                    [else
                     (cond
                       [(equal? f mexpt) (numV (mexpt s sm))]
                       [(equal? f and-aux) (boolV (and-aux s sm))]
                       [(equal? f or-aux) (boolV (or-aux s li))]
                       [(equal? f not-aux) (boolV (not-aux s))]
                       [(equal? f /=) (boolV (/= s sm))]
                       [(equal? f =) (boolV (op-aux #t = s sm))]
                       [(equal? f <=) (boolV (op-aux #t <= s sm))]
                       [(equal? f >=) (boolV (op-aux #t >= s sm))]
                       [(equal? f <) (boolV (op-aux #t < s sm))]
                       [(equal? f >) (boolV (op-aux #t > s sm))]
                       [(equal? f zero?) (boolV (zero? (numV-n s)))]
                       [(equal? f empty?) (boolV (app-list (car lst) f))]
                       [(equal? f head) (interp (app-list (car lst) f) env)]
                       [(equal? f tail) (interp (app-list (car lst) f) env)]
                       [else (numV (opera f (numV-n s) sm))])]))]
    [(iF ce t e)
     (if (eq? (boolV-b (interp ce env)) #t) (interp t env) (interp e env))]
    [(rec lb bod) (interp bod (make-env-rec lb env))]
    [(lisT elems) 
     (listV (map (lambda (x) (interp x env)) elems))]
    [(fun parm bod) (closureV parm bod env)]
    [(throws exception-id) (let/cc k (exceptionV exception-id k))]
    [(try/catch lb b) (let ([expr-val (interp b env)])
                        (let ([iexp (find-exc lb expr-val)])
                          (if (and (exceptionV? expr-val)
                                   (not (eq? iexp -1)))
                              ((exceptionV-continuation expr-val)
                               (interp (binding-value iexp) env))
                              expr-val)))]
    [(app fe la)
     (cond
       [(id? fe)
        (let ([clou (lookup (id-i fe) env)])
          (if (closureV? clou)
              (interp (app (fun (closureV-params clou) (closureV-body clou))
                           (make-param la env)) env)
              
              (strict
               (interp
                (app
                 (fun
                  (closureV-params(strict (lookup (id-i fe) env)))
                  (closureV-body(strict (lookup (id-i fe) env)))) la) env))))]
       [else
        (strict (interp (fun-body fe) (make-env (fun-params fe) la env)))])]))

;; [Auxiliar]. Función recursiva que busca una excepcion dentro de una lista dada.
(define (find-op lst)
  (match lst
    ['() -1]
    [(cons x xs) (if (exceptionV? x) x (find-op xs))]))

;; [Auxiliar]. Función recursiva que busca si un id está asociado a una excepción.
(define (find-exc lb exc)
  (if (not (exceptionV? exc)) exc
      (match lb
        ['() -1]
        [(cons x xs) (if (equal? (binding-name x) (exceptionV-exception-id exc))
                         x (find-exc xs exc))])))
           
;; [Auxiliar]. Función recursiva que obtiene dados los parametros de una aplicación
;; de función se encarga de buscarlos en el ambiente si son id's y convertirlos para
;; que puedan ser usado por interp.
;; ERCFBAEL/L-Value -> ERCFBAEL /L
(define (make-param params env)
  (match params
    ['() '()]
    [(cons x xs)
     (if (id? x)
         (let ([val (lookup (id-i x) env)])
           (match val
             [(numV n) (append (list (num n)) (make-param xs env))]
             [(boolV b) (append (list (bool b)) (make-param xs env))]
             [(listV lst) (append (list (lisT lst)) (make-param xs env))]
             [else (append (list val) (make-param xs env))]))
         (append (list x) (make-param xs env)))]))

;; Función para forzar la evaluación de un punto estrícto dentro de una
;; expresión.
;; strict: ERCFBAEL/L -> ERCFBAEL/L-Value
(define (strict expV)
  (match expV
    [(exprV exp en) (strict (interp (exprV-expr expV) (exprV-env expV)))]
    [else expV]))

;; Función que genera el ambiente posterior correspondiente.
(define (make-env lp args env)
  (match lp
    ['() env]
    [(cons x xs)
     (aSub x (exprV (first args) env) (make-env xs (rest args) env))]))

;; Función que busca un identificador en el ambiente.
(define (lookup id env)
  (match env
    [(mtSub) (error "error: Free identifier")]
    [(aSub idd val envv)
     (if (eq? id idd) val
         (lookup id envv))]
    [(aRecSub sub-id value rest-env)
     (if (symbol=? id sub-id)
         (unbox value) ; Se obtiene un closure que hace referencia al mismo ambiente.
         (lookup id rest-env))]))

;; Función n-aria para operadores aritméticos.
(define (opera f n l)
  (match l
    ['() n]
    [(cons x xs) (opera f (f n (numV-n (strict x))) xs)]))

;; [Auxiliar]. Función que interpreta los elementos de una lista
;; de expresiones dada.
;; interp: ERCFBAE -> ERCFBAE-Value
(define (map-interp l env)
  (match l
    ['() '()]
    [(cons x xs) (append (list (interp x env)) (map-interp xs env))]))

;; [Auxiliar]. Función recursiva que crea el ambiente recursivo de las lista
;; de bindings
;; ERCFBAEL/L-Value -> ERCFBAEL/L-Value
(define (make-env-rec lb env)
  (match lb
    ['() env]
    [(cons x xs) (make-env-rec xs (genera x env))]))

;; [Auxiliar]. Función que crea el ambiente recursivo de un binding
;; ERCFBAEL/L-Value -> ERCFBAEL/L-Value
(define (genera b env)
  (let* ([value-holder (box (numV 1729))]
         [new-env (aRecSub (binding-name b) value-holder env)]
         [named-expr-val (interp (binding-value b) new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

;; [Auxiliar]. Función que aplica la función recibida al la lista de elementos de
;; una lisT dada.
(define (app-list lst fun)
  (fun (lisT-elems lst)))