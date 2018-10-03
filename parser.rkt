#lang plai

(require "grammars.rkt")

;; Analizador sintáctico para ERCFWBAEL/L.
;; Dada una s-expression, costruye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> ERCFWBAEL/L.
(define (parse sexp)
  (cond
      [(symbol? sexp)
       (cond
         [(equal? 'true sexp) (boolS #t)]
         [(equal? 'false sexp) (boolS #f)]
         [else (idS sexp)])] 
      [(number? sexp) (numS sexp)] 
      [(boolean? sexp) (boolS sexp)]
      [(list? sexp)
       (case (car sexp)
         [(+ - * - / % max min pow not and or < > <= >= = /=
             head tail empty? zero?) 
          (opS
           (elige (car sexp))
           (map parse (rest sexp)))]
         [(with) 
          (withS
           (parse-bindings (cadr sexp))
           (parse (caddr sexp)))]
         [(with*)
          (withS*
           (parse-bindings (cadr sexp))
           (parse (caddr sexp)))]
         [(fun) 
          (funS
           (second sexp)
           (parse (third sexp)))]
         [(if)
          (ifS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
         [(rec)
          (recS (parse-bindings (cadr sexp)) (parse (caddr sexp)))]
         [(list)
          (listS (map parse (rest sexp)))]
         [(cond)
          (condS (make-cond (rest sexp)))]
         [(throws) (throwsS (cadr sexp))]
         [(try/catch) (try/catchS (parse-bindings (cadr sexp)) (parse (caddr sexp)))]
         [else
          (appS
           (parse (first sexp))
           (foldr cons empty (map parse (rest sexp))))])]))

;; [ Auxiliar ]. Función que construye el árbol de síntaxis correspondiente
;; a la condición dada.
(define (make-cond lst)
  (match lst
    ['() empty]
    [(cons x xs)
     (if (equal? (first x) 'else)
         (append (list (else-cond (parse (second x)))) (make-cond xs))
         (append (list (condition (parse (first x)) (parse (second x)))) (make-cond xs)))]))

;; [ Auxiliar ]. Función que hace un mapeo entre los operadores en
;; sintaxis concreta y los operadores de Racket . Esto con el fin de
;; apli car la operación más adelante .
;; elige : symbol - > procedure
(define (elige sexp)
  (match sexp
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['% modulo]
    ['< <]
    ['> >]
    ['<= <=]
    ['>= >=]
    ['= =]
    ['/= /=]
    ['pow mexpt]
    ['max max]
    ['min min]
    ['and and-aux]
    ['or or-aux]
    ['not not-aux]
    ['head head]
    ['tail tail]
    ['empty? empty?]
    ['zero? zero?]))

;; [Auxiliar]. Función que construye la expresión de tipo abstracto
;; Binding asociando un identificador con su valor.
;; parse : s-expresion - > Binding.
(define (parse-binding l)
  (match l
    ['() '()]
    [(list name value) (list (bindingS name (parse value)))]))

;; Función que construye la expresiòn correpondiente a la lista de 
;; Bindings asociando sus identificadores con sus valores.
(define (parse-bindings l)
  (match l
    ['() '()]
    [(cons x xs) (append (parse-binding x) (parse-bindings xs))]))


;; Función que elimina el azúcar sintáctica de las expresiones de ERCFWBAE/L,
;; es decir las convierte a expresiones de ERCFBAE/L.
;; desugar: ERCFWBAE/L -> ERCFBAE/L
(define (desugar expr)
 (match expr
   [(idS i) (id i)]
   [(numS n) (num n)]
   [(listS elems) (lisT (map desugar elems))]
   [(boolS b) (bool b)]
   [(opS f l) (op f (map desugar l))]
   [(ifS ie t e) (iF (desugar ie) (desugar t) (desugar e))]
   [(condS lst) (desugar-cond lst)]
   [(withS lb body) (app (fun (nombres lb)(desugar body)) (params lb))]
   [(withS* lb body) (desugar (desugar-with lb body))]
   [(recS lb body) (rec (BindingS-list->Binding-list lb) (desugar body))]
   [(funS param body) (fun param (desugar body))]
   [(throwsS i) (throws i)]
   [(try/catchS lb b) (try/catch (BindingS-list->Binding-list lb) (desugar b))]
   [(appS fun-expr arg-expr)
    (cond
      [(idS? fun-expr) (app (desugar fun-expr) (map desugar arg-expr))]
      [else (app (fun (funS-params fun-expr)
                      (desugar (funS-body fun-expr))) (map desugar arg-expr))])]))

;; [Auxiliar]. Función que elimina la azúcar sintáctica de la expresión
;; cond, convirtiéndolo a expresiones 
;; desugar: ERCFWBAE/L -> ERCFBAE/L
(define (desugar-cond lst)
  (match (first lst)
    [(condition ce t) (iF (desugar ce) (desugar t) (desugar-cond (rest lst)))]
    [(else-cond t) (desugar t)]))

;; [Auxiliar].Función que devuelve una lista de Bindings
;; dada una lista de BindingS.
;; BindingS-list->Binding-list : (listof BindingS) -> (listof Binding)
(define (BindingS-list->Binding-list lst)
  (match lst
    ['() '()]
    [(cons (bindingS name value) xs)
     (cons (binding name (desugar value)) (BindingS-list->Binding-list xs))]))

;; [Auxiliar]. Función que devuelve los identificadores de la lista de Bindings
;; dada.
(define (nombres lb)
  (match lb
    ['() '()]
    [(cons x xs) (append (list (bindingS-name x)) (nombres xs))]))

;; [Auxiliar]. Función que devuelve lo valores asociados a los identificadores de
;; la lista de Bindings dada.
(define (params lb)
  (match lb
    ['() '()]
    [(cons x xs) (append (list (desugar (bindingS-value x))) (params xs))]))

;; [Auxiliar]. Función que descompone un with* en whitS anidados.
(define (desugar-with lb body)
  (match lb
    ['() body]
    [(cons x xs) (withS (list x) (desugar-with xs body))]))

;; Función lógica 'and' n-aria.
(define (and-aux b lo)
 (match lo
   ['() b]
   [(cons x xs) (and (and (boolV-b b) (boolV-b x)) (and-aux x xs))]))

;; Función lógica 'or' n-aria.
(define (or-aux b lo)
  (match lo
    ['() #f]
    [(cons x xs) (or (or (boolV-b b) (boolV-b x)) (or-aux x xs))]))

;; Función lógica 'not'.
(define (not-aux b)
  (not (boolV-b b)))

;; Función n-aria que determina si los elementos de una lista son distintos.
(define (/= n lst)
  (match lst
    ['() #t]
    [(cons x xs) (and (not (eq? (numV-n n) (numV-n x))) (/= x xs))]))

;; Función n-aria que determina si los elementos de una lista son cumple con
;; el operando lógico dado, es decir si se cumple que sean <=,<,>=,> o =.
(define (op-aux t op f lst)
  (match lst
    ['() t]
    [(cons x xs) (and t (op-aux (op (numV-n f) (numV-n x)) op x xs))]))

;; Función exponencial n-aria.
(define (mexpt n l)
  (match l
    ['() n]
    [(cons x xs) (mexpt (expt (numV-n n) (numV-n x)) xs)]))

;; Función que te devuelve la cabeza de la lista.
(define (head lst)
  (first lst))

;; Función que te devuelve la cola de la lista.
(define (tail lst)
  (first (reverse lst)))