#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 7 |#

;; Identificadores.
(define id1 'foo)
(define id2 'milw0rm)

;; Números.
(define n1 -1)
(define n2 1729)
(define n3 1/8)
(define n4 1.234567)
(define n5 1+2i)

;; Operaciones lógicas o de comapración.
(define ol1 false)
(define ol2 '{or true})
(define ol3 '{not {and true false true false}})
(define ol4 '{<= 4 5 6 7 8})
(define ol5 '{> 1 7 500 -10 4})

;; Operaciones artiméticas.
(define op1 '{* 8 4 {- 4 2} {max -100 0 1}})
(define op2 '{+ 4 {% 109 5}})
(define op3 '{/ {* 1 8 5} {- 100 2}})
(define op4 '{max {+ 1 2 3} {- 10 1 2} 5})
(define op5 '{/ 1 2 {min 8 4 6}})

;; Listas
(define l1 '{empty? {list }})
(define l2 '{list 0 1 2 3})
(define l3 '{tail {list true false false true false}})
(define l4 '{list {fun {a} {+ a a}} {/= 1 2}  4})
(define l5 '{head {list {+ 8 1} {% 1 2 34} true}})

;; Funciones Recursivas
(define rec1
  '{rec {{fac
          {fun {n}
               {if {zero? n} 1
                   {* n {fac {- n 1}}}}}} {n 5}}
     {fac n}})

(define rec2
  '{rec {{fib
          {fun {n}
               {if {<= n 2} 1
                   {+ {fib {- n 1}} {fib {- n 2}}}}}} {n 7}}
     {fib n}})

(define rec3
  '{rec {{trib
          {fun {n}
               {cond
                 {{= n 0} 0}
                 {{= n 1} 0}
                 {{= n 2} 1}
                 {else {+ {trib {- n 1}}
                          {trib {- n 2}}
                          {trib {- n 3}}}}}}} {n 4}}
     {trib n}})

(define rec4
  '{rec
       {{diezn
         {fun {n}
              {if {<= n 0} 1 
                  {* 10 {diezn {- n 1}}}}}} {n 6}}
     {diezn n}})

(define rec5
  '{rec {{suman8s
          {fun {n}
               {if {<= n 0} 0
                   {+ 8 {suman8s {- n 1}}}}}} {n 7}}
     {suman8s n}})

;; If's

(define if1 '{if {< 1 8} 1997 1996})
(define if2 '{if {/= 2 10 14} 5 8})
(define if3 '{if {> {/ 8 4} {max 100 24}} {+ 5 11} 21})
(define if4 '{if {= {{fun {x} {- x x}} 5} 0} 7 {* 7 2}})
(define if5 '{if {<= {+ 1 2 3 4} {- 12 5 2}} {max 4 3}
                 {{fun {x} {pow x 3}} 4}})
(define if6 '{if {<= 22 10 14} {/ 0 0} 44+3i})

;; Cond's
(define cond1 '{cond {{> 100 {{fun {x y} {- {pow x x} {pow y y}} } 5 4}} 1}
                    {else 189}})
(define cond2 '{cond {{< 0 1} {max 10 11}} {{/= 10 2} 5} {else {+ 4 5}}})
(define cond3 '{cond
                 {{= {- 1 1} 0} {cond {{/= 1 2 3} 4} {else -1.5}}}
                 {{and true false} 74}
                 {else 0}})
(define cond4 '{cond {{or false true false} 5}
                     {{< 8 1 2 34} 7}
                     {{= {/ 4 8} {/ 1 2} {/ 2 4}} {max 30.2 99.9}}
                     {else {{fun {x} {/ {+ x 1} x}} 2}}})
(define cond5 '{cond {{= {max 10 1} 1} 80}
                     {{> 72 {pow 2 4}} 74}
                     {{/= 1 {- 2 1}} 1024} {else 98}})
(define cond6 '{cond {{>= {max 1 -121} 12 24} {- 45 5}}
                     {{= {with {{areaR {fun {base altura} {* base altura}}}
                                {base 5} {altura 8}}
                               {areaR base altura}} {* 5 8}} 1968+4i}
                     {{> 8 {with {{g {fun {x} {* 1 2 x}}} {x 3}}
                                 {g x}} 2+3i} 100}
                     {else -5.0}})
(define cond7 '{cond {{< 8 1 2 34} 7}
                     {{or false false false} {+ 7.1 c:}}
                     {{= {/ 4 8} {/ 1 2} {/ 2 4}} {max 99.9 1}}
                     {else {{fun {x} {/ {+ x 1} x}} 2}}})

;; Funciones (with y with*).
(define w1 '{with {{a 3}} {+ a 4}})
(define w2 '{with {{x 0}} {* x x}})
(define w3 '{with {{a 3} {b 8}} {+ a b}})
(define w4 '{with {{p true} {q false}} {and p q}})
(define w5 '{with {{p true} {q false} {r true}} {or {or p q} r}})

(define w*1 '{with* {{ a 2} {b {+ a a }}} b})
(define w*2 '{with* {{a 8} {b 2}} {+ a b 7}})
(define w*3 '{with* {{a 2} {c {- a 1}} {b {pow c 3}}} {min a b c}})
(define w*4 '{with* { {a 10} {b {- a 5}} {c {* b b}} } {- c {+ a b b}}})
(define w*5 '{with* {{a {+ 3 1}} {b {- a 2}} {c {* b b}}} {/ b {* a c}}})

;; Funciones y applicación de funciones.
(define f1 '{fun {x} {* x 2}})
(define f2 '{fun {x} {+ x 3}})
(define f3 '{fun {x} {+ {* x 2} {pow x 3}}})
(define f4 '{fun {x y z} {min x y z}})
(define f5 '{fun {r s} {% r s}})
(define f6 '{fun {x} {* x x}})

(define ap1 '{{fun {x} {+ x 1}} 2})
(define ap2 '{{fun {a b} {+ a {/ b 4} 10}} 3 4})
(define ap3 '{{fun {a b} {pow a b}} 2 10})
(define ap4 '{{fun {b h} {/ {* b h} 2}} 2 4})
(define ap5 '{with {{f {fun {x} {pow x 2}}} {a 3}} {f a}})
(define ap6 '{with {{f {fun {x} {% x 10}}}
                    {g {fun {x y} {+ x y}}}
                    {a 5} {b 2}} {g {f a} {f b}}})
(define ap7 '{{fun {x y} {min x y}}
              {{fun {x y} {max x y}} 21 24} 21})
(define ap8 '{with {{areaC {fun {r} {* 3.1416 {pow r 2}}}}
                              {radio 3}} {areaC radio}})
(define ap9 '{with {{f {fun {x} {% x 10}}}
                              {g {fun {x y} {+ x y}}} {a 5} {b 2}}
                             {g {f a} {f b}}})
(define ap10 '{{fun {x y} {min x y}} {{fun {x y} {max x y}} 21 24} 21})

;; Excepciones.

(define ex1 '{throws Milw0rmExcepcion})
(define ex2 '{try/catch {{DivisionEntreCero 2}}
                        {throws DivisionEntreCero}})
(define ex3 '{try/catch
              {{k {+ 1 2}}}
              {+ {throws k} 3 4 5}})
(define ex4 '{try/catch {{DivisionPorCero 2} {Milw0rmE 9796}}
                        {* 1 {throws Milw0rmE}}})
(define ex5 '{try/catch {{E1 {% 2 7}} {E2 8} {E3 {+ 7 8 9}}}
                        {cond {{or false false} {throws E1}}
                              {{/= 7 -7} {throws E2}}
                              {else {throws E3}}}})
(define ex6 '{try/catch {{Ex 3}}
                        {- 1 {throws
                         Ex}}})
(define ex7 '{try/catch {{e1 true} {e2 1245}}
                        {if {< -10 9}
                            {or {throws e1} false}
                            {+ {throws e2} 7}}})
(define ex8 '{try/catch {{E0 1000}}
                        {throws Excepcion}})
;; Pruebas parse.

(test (parse id1) (idS 'foo))
(test (parse id2) (idS 'milw0rm))

(test (parse n1) (numS -1))
(test (parse n2) (numS 1729))
(test (parse n3) (numS 1/8))
(test (parse n4) (numS 1.234567))
(test (parse n5) (numS 1+2i))

(test (parse ol1) (boolS false))
(test (parse ol2)
      (opS or-aux (list (boolS true))))
(test (parse ol3)
      (opS not-aux
           (list
            (opS and-aux
                 (list (boolS #t) (boolS #f) (boolS #t) (boolS #f))))))

(test (parse ol4)
      (opS <=
           (list (numS 4) (numS 5) (numS 6) (numS 7) (numS 8))))
(test (parse ol5)
      (opS > (list (numS 1) (numS 7) (numS 500) (numS -10) (numS 4))))


(test (parse op1)
      (opS *
           (list
            (numS 8)
            (numS 4)
            (opS - (list (numS 4) (numS 2)))
            (opS max (list (numS -100) (numS 0) (numS 1))))))
(test (parse op2)
      (opS + (list (numS 4) (opS modulo (list (numS 109) (numS 5))))))
(test (parse op3)
      (opS /
           (list (opS * (list (numS 1) (numS 8) (numS 5)))
                 (opS - (list (numS 100) (numS 2))))))
(test (parse op4)
      (opS max
           (list (opS + (list (numS 1) (numS 2) (numS 3)))
                 (opS - (list (numS 10) (numS 1) (numS 2)))
                 (numS 5))))
(test (parse op5)
      (opS / (list (numS 1) (numS 2)
                   (opS min (list (numS 8) (numS 4) (numS 6))))))


(test (parse l1) (opS empty? (list (listS '()))))
(test (parse l2)(listS (list (numS 0) (numS 1) (numS 2) (numS 3))))
(test (parse l3)
      (opS
       tail
       (list
        (listS
         (list (boolS #t) (boolS #f) (boolS #f) (boolS #t) (boolS #f))))))
(test (parse l4)
      (listS
       (list
        (funS '(a) (opS + (list (idS 'a) (idS 'a))))
        (opS /= (list (numS 1) (numS 2))) (numS 4))))
(test (parse l5)
      (opS head
           (list
            (listS
             (list
              (opS + (list (numS 8) (numS 1)))
              (opS modulo (list (numS 1) (numS 2) (numS 34))) (boolS #t))))))

(test
 (parse rec1)
 (recS
  (list
   (bindingS
    'fac
    (funS '(n)
          (ifS (opS zero? (list (idS 'n))) (numS 1)
               (opS *
                    (list (idS 'n) (appS (idS 'fac)
                                         (list (opS - (list (idS 'n) (numS 1))))))))))
   (bindingS 'n (numS 5)))
  (appS (idS 'fac) (list (idS 'n)))))

(test
 (parse rec2)
 (recS
  (list
   (bindingS
    'fib
    (funS '(n)
          (ifS
           (opS <= (list (idS 'n) (numS 2))) (numS 1)
           (opS +
                (list (appS (idS 'fib)
                            (list (opS - (list (idS 'n) (numS 1)))))
                      (appS (idS 'fib)
                            (list (opS - (list (idS 'n) (numS 2))))))))))
   (bindingS 'n (numS 7)))
  (appS (idS 'fib) (list (idS 'n)))))

(test
 (parse rec3)
 (recS
  (list
   (bindingS
    'trib
    (funS
     '(n)
     (condS
      (list
       (condition (opS = (list (idS 'n) (numS 0))) (numS 0))
       (condition (opS = (list (idS 'n) (numS 1))) (numS 0))
       (condition (opS = (list (idS 'n) (numS 2))) (numS 1))
       (else-cond
        (opS +
             (list
              (appS (idS 'trib) (list (opS - (list (idS 'n) (numS 1)))))
              (appS (idS 'trib) (list (opS - (list (idS 'n) (numS 2)))))
              (appS (idS 'trib) (list (opS - (list (idS 'n) (numS 3))))))))))))
   (bindingS 'n (numS 4)))
  (appS (idS 'trib) (list (idS 'n)))))

(test (parse rec4)
      (recS
       (list
        (bindingS
         'diezn
         (funS
          '(n)
          (ifS
           (opS <= (list (idS 'n) (numS 0)))
           (numS 1)
           (opS * (list (numS 10)
                        (appS (idS 'diezn)
                              (list (opS - (list (idS 'n) (numS 1))))))))))
        (bindingS 'n (numS 6)))
       (appS (idS 'diezn) (list (idS 'n)))))

(test (parse rec5)
      (recS
       (list
        (bindingS
         'suman8s
         (funS '(n)
               (ifS
                (opS <= (list (idS 'n) (numS 0)))
                (numS 0)
                (opS +
                     (list
                      (numS 8)
                      (appS
                       (idS 'suman8s) (list (opS - (list (idS 'n) (numS 1))))))))))
        (bindingS 'n (numS 7)))
       (appS (idS 'suman8s) (list (idS 'n)))))

(test (parse if1)
      (ifS (opS < (list (numS 1) (numS 8)))  (numS 1997) (numS 1996)))
(test (parse if2)
      (ifS (opS /= (list (numS 2) (numS 10) (numS 14))) (numS 5) (numS 8)))
(test (parse if3)
      (ifS
       (opS > (list (opS / (list (numS 8) (numS 4)))
                    (opS max (list (numS 100) (numS 24)))))
       (opS + (list (numS 5) (numS 11)))
       (numS 21)))
(test (parse if4)
      (ifS
       (opS = (list (appS (funS '(x) (opS - (list (idS 'x) (idS 'x))))
                          (list (numS 5))) (numS 0)))
       (numS 7)
       (opS * (list (numS 7) (numS 2)))))
(test (parse if5)
      (ifS
       (opS <=
            (list (opS + (list (numS 1) (numS 2) (numS 3) (numS 4)))
                  (opS - (list (numS 12) (numS 5) (numS 2)))))
       (opS max (list (numS 4) (numS 3)))
       (appS (funS '(x) (opS mexpt (list (idS 'x) (numS 3))))
             (list (numS 4)))))
(test (parse cond1)
      (condS
       (list
        (condition
         (opS >
              (list
               (numS 100)
               (appS
                (funS '(x y) (opS -
                                  (list (opS mexpt (list (idS 'x) (idS 'x)))
                                        (opS mexpt (list (idS 'y) (idS 'y))))))
                (list (numS 5) (numS 4)))))
         (numS 1))
        (else-cond (numS 189)))))

(test (parse cond2)
 (condS
  (list
   (condition (opS < (list (numS 0) (numS 1))) (opS max (list (numS 10)
                                                              (numS 11))))
   (condition (opS /= (list (numS 10) (numS 2))) (numS 5))
   (else-cond (opS + (list (numS 4) (numS 5)))))))

(test (parse cond3)
      (condS
       (list
        (condition
         (opS = (list (opS - (list (numS 1) (numS 1))) (numS 0)))
         (condS
          (list (condition (opS /= (list (numS 1) (numS 2) (numS 3))) (numS 4))
                (else-cond (numS -1.5)))))
        (condition (opS and-aux (list (boolS #t) (boolS #f))) (numS 74))
        (else-cond (numS 0)))))

(test (parse cond4)
(condS
 (list
  (condition (opS or-aux (list (boolS #f) (boolS #t) (boolS #f))) (numS 5))
  (condition (opS < (list (numS 8) (numS 1) (numS 2) (numS 34))) (numS 7))
  (condition
   (opS = (list (opS / (list (numS 4) (numS 8)))
                (opS / (list (numS 1) (numS 2)))
                (opS / (list (numS 2) (numS 4)))))
   (opS max (list (numS 30.2) (numS 99.9))))
  (else-cond
   (appS
    (funS '(x) (opS / (list (opS + (list (idS 'x) (numS 1))) (idS 'x))))
    (list (numS 2)))))))

(test (parse cond5)
     (condS
      (list
       (condition
        (opS = (list (opS max (list (numS 10) (numS 1))) (numS 1))) (numS 80))
       (condition
        (opS > (list (numS 72) (opS mexpt (list (numS 2) (numS 4))))) (numS 74))
       (condition
        (opS /= (list (numS 1) (opS - (list (numS 2) (numS 1))))) (numS 1024))
       (else-cond (numS 98)))))

(test (parse cond6)
      (condS
       (list
        (condition (opS >=
                        (list (opS max (list (numS 1) (numS -121)))
                              (numS 12)
                              (numS 24)))
                   (opS - (list (numS 45) (numS 5))))
        (condition
         (opS =
              (list
               (withS
                (list
                 (bindingS 'areaR
                           (funS '(base altura)
                                 (opS * (list (idS 'base) (idS 'altura)))))
                 (bindingS 'base (numS 5)) (bindingS 'altura (numS 8)))
                (appS (idS 'areaR) (list (idS 'base) (idS 'altura))))
               (opS * (list (numS 5) (numS 8)))))
         (numS 1968+4i))
        (condition
         (opS >
              (list
               (numS 8)
               (withS
                (list
                 (bindingS 'g
                           (funS '(x)
                                 (opS * (list (numS 1) (numS 2) (idS 'x)))))
                 (bindingS 'x (numS 3))) (appS (idS 'g) (list (idS 'x))))
               (numS 2+3i)))
         (numS 100))
        (else-cond (numS -5.0)))))

(test (parse w1)
      (withS (list (bindingS 'a (numS 3))) (opS + (list (idS 'a) (numS 4)))))
(test (parse w2)
      (withS (list (bindingS 'x (numS 0))) (opS * (list (idS 'x) (idS 'x)))))
(test (parse w3)
      (withS (list (bindingS 'a (numS 3)) (bindingS 'b (numS 8)))
             (opS + (list (idS 'a) (idS 'b)))))
(test (parse w4)
      (withS (list (bindingS 'p (boolS true)) (bindingS 'q (boolS false)))
             (opS and-aux (list (idS 'p) (idS 'q)))))
(test (parse w5)
      (withS
       (list (bindingS 'p (boolS #t))
             (bindingS 'q (boolS #f))
             (bindingS 'r (boolS #t)))
       (opS or-aux (list (opS or-aux (list (idS 'p) (idS 'q))) (idS 'r)))))

(test (parse w*1)
      (withS* (list (bindingS 'a (numS 2))
                    (bindingS 'b (opS + (list (idS 'a) (idS 'a))))) (idS 'b)))
(test (parse w*2)
      (withS* (list
               (bindingS 'a (numS 8))
               (bindingS 'b (numS 2)))
              (opS + (list (idS 'a) (idS 'b) (numS 7)))))
(test (parse w*3)
      (withS*
       (list
        (bindingS 'a (numS 2))
        (bindingS 'c (opS - (list (idS 'a) (numS 1))))
        (bindingS 'b (opS mexpt (list (idS 'c) (numS 3)))))
       (opS min (list (idS 'a) (idS 'b) (idS 'c)))))
(test (parse w*4)
      (withS*
       (list
        (bindingS 'a (numS 10))
        (bindingS 'b (opS - (list (idS 'a) (numS 5))))
        (bindingS 'c (opS * (list (idS 'b) (idS 'b)))))
       (opS - (list (idS 'c) (opS + (list (idS 'a) (idS 'b) (idS 'b)))))))
(test (parse w*5)
      (withS*
       (list
        (bindingS 'a (opS + (list (numS 3) (numS 1))))
        (bindingS 'b (opS - (list (idS 'a) (numS 2))))
        (bindingS 'c (opS * (list (idS 'b) (idS 'b)))))
       (opS / (list (idS 'b) (opS * (list (idS 'a) (idS 'c)))))))
(test (parse f1)
      (funS '(x) (opS * (list (idS 'x) (numS 2)))))
(test (parse f2)
      (funS '(x) (opS + (list (idS 'x) (numS 3)))))
(test (parse f3)
      (funS '(x)
            (opS + (list
                    (opS * (list (idS 'x) (numS 2)))
                    (opS mexpt (list (idS 'x) (numS 3)))))))
(test (parse f4)
      (funS '(x y z) (opS min (list (idS 'x) (idS 'y) (idS 'z)))))
(test (parse f5)
      (funS '(r s) (opS modulo (list (idS 'r) (idS 's)))))

(test (parse ap1)
      (appS (funS '(x) (opS + (list (idS 'x) (numS 1)))) (list (numS 2))))
(test (parse ap2)
      (appS (funS '(a b)
                  (opS + (list (idS 'a)
                               (opS / (list (idS 'b) (numS 4))) (numS 10))))
            (list (numS 3) (numS 4))))
(test (parse ap3)
      (appS (funS '(a b)
                  (opS mexpt (list (idS 'a) (idS 'b))))
            (list (numS 2) (numS 10))))
(test (parse ap4)
      (appS (funS '(b h)
                  (opS /
                       (list (opS * (list (idS 'b) (idS 'h))) (numS 2))))
            (list (numS 2) (numS 4))))
(test (parse ap5)
      (withS
       (list (bindingS 'f (funS '(x)
                               (opS mexpt (list (idS 'x) (numS 2)))))
             (bindingS 'a (numS 3)))
       (appS (idS 'f) (list (idS 'a)))))
(test (parse ap6)
      (withS
       (list
        (bindingS 'f (funS '(x) (opS modulo (list (idS 'x) (numS 10)))))
        (bindingS 'g (funS '(x y) (opS + (list (idS 'x) (idS 'y)))))
        (bindingS 'a (numS 5))
        (bindingS 'b (numS 2)))
       (appS (idS 'g) (list
                       (appS (idS 'f) (list (idS 'a)))
                       (appS (idS 'f) (list (idS 'b)))))))
(test (parse ap7)
      (appS
       (funS '(x y) (opS min (list (idS 'x) (idS 'y))))
       (list
        (appS (funS '(x y)
                    (opS max (list (idS 'x) (idS 'y))))
              (list (numS 21) (numS 24))) (numS 21))))

(test (parse ex1) (throwsS 'Milw0rmExcepcion))
(test (parse ex2)
      (try/catchS
       (list (bindingS 'DivisionEntreCero (numS 2)))
       (throwsS 'DivisionEntreCero)))
(test (parse ex3)
      (try/catchS
       (list (bindingS 'k (opS + (list (numS 1) (numS 2)))))
       (opS + (list (throwsS 'k) (numS 3) (numS 4) (numS 5)))))
(test (parse ex4)
      (try/catchS
       (list (bindingS 'DivisionPorCero (numS 2)) (bindingS 'Milw0rmE (numS 9796)))
       (opS * (list (numS 1) (throwsS 'Milw0rmE)))))
(test (parse ex5)
      (try/catchS
       (list
        (bindingS 'E1 (opS modulo (list (numS 2) (numS 7))))
        (bindingS 'E2 (numS 8))
        (bindingS 'E3 (opS + (list (numS 7) (numS 8) (numS 9)))))
       (condS
        (list
         (condition (opS or-aux (list (boolS #f) (boolS #f))) (throwsS 'E1))
         (condition (opS /= (list (numS 7) (numS -7))) (throwsS 'E2))
         (else-cond (throwsS 'E3))))))
(test (parse ex8)
      (try/catchS (list (bindingS 'E0 (numS 1000))) (throwsS 'Excepcion)))

;; Pruebas desugar.

(test (desugar (parse id1)) (id 'foo))
(test (desugar (parse id2)) (id 'milw0rm))

(test (desugar (parse n1)) (num -1))
(test (desugar (parse n2)) (num 1729))
(test (desugar (parse n3)) (num 1/8))
(test (desugar (parse n4)) (num 1.234567))
(test (desugar (parse n5)) (num 1+2i))


(test (desugar (parse ol1)) (bool false))
(test (desugar (parse ol2))  (op or-aux (list (bool true))))
(test (desugar (parse ol3))
      (op not-aux
           (list (op and-aux
                     (list (bool #t) (bool #f) (bool #t) (bool #f))))))
(test (desugar (parse ol4))
      (op <=
           (list (num 4) (num 5) (num 6) (num 7) (num 8))))
(test (desugar (parse ol5))
      (op > (list (num 1) (num 7) (num 500) (num -10) (num 4))))

(test (desugar (parse op1))
      (op *
           (list
            (num 8)
            (num 4)
            (op - (list (num 4) (num 2)))
            (op max (list (num -100) (num 0) (num 1))))))
(test (desugar (parse op2))
      (op + (list (num 4) (op modulo (list (num 109) (num 5))))))
(test (desugar (parse op3))
      (op /
          (list (op * (list (num 1) (num 8) (num 5)))
                (op - (list (num 100) (num 2))))))
(test (desugar (parse op4))
      (op max
          (list
           (op + (list (num 1) (num 2) (num 3)))
           (op - (list (num 10) (num 1) (num 2)))
           (num 5))))
(test (desugar (parse op5))
      (op / (list (num 1) (num 2) (op min (list (num 8) (num 4) (num 6))))))


(test (desugar (parse l1)) (op empty? (list (lisT '()))))
(test (desugar (parse l2))
      (lisT (list (num 0) (num 1) (num 2) (num 3))))
(test (desugar (parse l3))
      (op
       tail
       (list (lisT (list (bool #t) (bool #f) (bool #f) (bool #t) (bool #f))))))
(test (desugar (parse l4))
      (lisT (list (fun '(a) (op + (list (id 'a) (id 'a))))
                   (op /= (list (num 1) (num 2))) (num 4))))
(test (desugar (parse l5))
      (op head (list (lisT (list (op + (list (num 8) (num 1)))
                   (op modulo (list (num 1) (num 2) (num 34))) (bool #t))))))

(test
 (desugar (parse rec1))
 (rec
     (list
      (binding
       'fac
       (fun '(n)
            (iF (op zero? (list (id 'n)))
                (num 1)
                (op *
                    (list (id 'n)
                          (app (id 'fac)
                               (list (op - (list (id 'n) (num 1))))))))))
      (binding 'n (num 5)))
   (app (id 'fac) (list (id 'n)))))

(test
 (desugar (parse rec2))
 (rec
     (list
      (binding
       'fib
       (fun
        '(n)
        (iF
         (op <= (list (id 'n) (num 2)))
         (num 1)
         (op + (list
                (app (id 'fib) (list (op - (list (id 'n) (num 1)))))
                (app (id 'fib) (list (op - (list (id 'n) (num 2))))))))))
      (binding 'n (num 7)))
   (app (id 'fib) (list (id 'n)))))

(test
 (desugar (parse rec3))
 (rec
     (list
      (binding
       'trib
       (fun
        '(n)
        (iF
         (op = (list (id 'n) (num 0)))
         (num 0)
         (iF
          (op = (list (id 'n) (num 1)))
          (num 0)
          (iF
           (op = (list (id 'n) (num 2)))
           (num 1)
           (op +
               (list
                (app (id 'trib) (list (op - (list (id 'n) (num 1)))))
                (app (id 'trib) (list (op - (list (id 'n) (num 2)))))
                (app (id 'trib) (list (op - (list (id 'n) (num 3))))))))))))
      (binding 'n (num 4)))
   (app (id 'trib) (list (id 'n)))))

(test
 (desugar (parse rec4))
 (rec
     (list
      (binding
       'diezn
       (fun
        '(n)
        (iF
         (op <= (list (id 'n) (num 0)))
         (num 1)
         (op * (list (num 10)
                     (app (id 'diezn)
                          (list (op - (list (id 'n) (num 1))))))))))
      (binding 'n (num 6)))
   (app (id 'diezn) (list (id 'n)))))

(test
 (desugar
  (parse rec5))
 (rec
     (list
      (binding
       'suman8s
       (fun
        '(n)
        (iF (op <= (list (id 'n) (num 0))) (num 0)
            (op + (list (num 8)
                        (app (id 'suman8s)
                             (list (op - (list (id 'n) (num 1))))))))))
      (binding 'n (num 7)))
   (app (id 'suman8s) (list (id 'n)))))

(test (desugar (parse if1))
      (iF (op < (list (num 1) (num 8))) (num 1997) (num 1996)))
(test (desugar (parse if2))
      (iF (op /= (list (num 2) (num 10) (num 14))) (num 5) (num 8)))
(test (desugar (parse if3))
      (iF
       (op >
           (list (op / (list (num 8) (num 4)))
                 (op max (list (num 100) (num 24)))))
       (op + (list (num 5) (num 11)))
       (num 21)))
(test (desugar (parse if4))
      (iF
       (op = (list (app (fun '(x) (op - (list (id 'x) (id 'x))))
                          (list (num 5))) (num 0)))
       (num 7)
       (op * (list (num 7) (num 2)))))

(test (desugar (parse if5))
      (iF
       (op <=
            (list (op + (list (num 1) (num 2) (num 3) (num 4)))
                  (op - (list (num 12) (num 5) (num 2)))))
       (op max (list (num 4) (num 3)))
       (app (fun '(x) (op mexpt (list (id 'x) (num 3))))
             (list (num 4)))))

(test
 (desugar
  (parse cond1))
 (iF (op >
         (list
          (num 100)
          (app (fun '(x y)
                    (op - (list (op mexpt (list (id 'x) (id 'x)))
                                (op mexpt (list (id 'y) (id 'y))))))
               (list (num 5) (num 4)))))
     (num 1)
     (num 189)))
(test
 (desugar
  (parse cond2))
 (iF
  (op < (list (num 0) (num 1)))
  (op max (list (num 10) (num 11)))
  (iF (op /= (list (num 10) (num 2))) (num 5)
      (op + (list (num 4) (num 5))))))
(test
 (desugar
  (parse cond3))
 (iF
  (op = (list (op - (list (num 1) (num 1))) (num 0)))
  (iF (op /= (list (num 1) (num 2) (num 3))) (num 4) (num -1.5))
  (iF (op and-aux (list (bool #t) (bool #f))) (num 74) (num 0))))
(test
 (desugar
  (parse cond4))
 (iF
  (op or-aux (list (bool #f) (bool #t) (bool #f)))
  (num 5)
  (iF
   (op < (list (num 8) (num 1) (num 2) (num 34)))
   (num 7)
   (iF
    (op =
        (list (op / (list (num 4) (num 8)))
              (op / (list (num 1) (num 2)))
              (op / (list (num 2) (num 4)))))
    (op max
        (list (num 30.2) (num 99.9)))
    (app (fun '(x)
              (op /
                  (list (op + (list (id 'x) (num 1))) (id 'x))))
         (list (num 2)))))))
(test
 (desugar
  (parse cond5))
 (iF
  (op = (list (op max (list (num 10) (num 1))) (num 1)))
  (num 80)
  (iF
   (op > (list (num 72) (op mexpt (list (num 2) (num 4)))))
   (num 74)
   (iF (op /=
           (list (num 1) (op - (list (num 2) (num 1))))) (num 1024)
                                                         (num 98)))))
(test
 (desugar
  (parse cond6))
 (iF
  (op >= (list (op max (list (num 1) (num -121))) (num 12) (num 24)))
  (op - (list (num 45) (num 5)))
  (iF
   (op =
       (list
        (app
         (fun '(areaR base altura)
              (app (id 'areaR) (list (id 'base) (id 'altura))))
         (list
          (fun '(base altura)
               (op * (list (id 'base) (id 'altura)))) (num 5) (num 8)))
        (op * (list (num 5) (num 8)))))
   (num 1968+4i)
   (iF
    (op >
        (list (num 8)
              (app (fun '(g x) (app (id 'g) (list (id 'x))))
                   (list
                    (fun '(x)
                         (op * (list (num 1) (num 2) (id 'x)))) (num 3)))
              (num 2+3i)))
    (num 100)
    (num -5.0)))))

(test (desugar (parse w1))
      (app (fun '(a) (op + (list (id 'a) (num 4)))) (list (num 3))))
(test (desugar (parse w2))
      (app (fun '(x) (op * (list (id 'x) (id 'x)))) (list (num 0))))
(test (desugar (parse w3))
      (app (fun '(a b)
                (op + (list (id 'a) (id 'b)))) (list (num 3) (num 8))))
(test (desugar (parse w4))
      (app (fun '(p q)
                (op and-aux (list (id 'p) (id 'q)))) (list (bool #t) (bool #f))))
(test
 (desugar
  (parse w5))
 (app (fun '(p q r)
           (op or-aux
               (list (op or-aux (list (id 'p) (id 'q))) (id 'r))))
      (list (bool #t) (bool #f) (bool #t))))

(test (desugar (parse w*1))
      (app (fun '(a)
                (app (fun '(b) (id 'b))
                     (list (op + (list (id 'a) (id 'a))))))
           (list (num 2))))
(test (parse w*2)
      (withS* (list (bindingS 'a (numS 8))
                    (bindingS 'b (numS 2)))
              (opS + (list (idS 'a) (idS 'b) (numS 7)))))
(test (desugar
       (parse w*3))
      (app (fun '(a)
                (app (fun '(c)
                          (app (fun '(b)
                                    (op min (list (id 'a) (id 'b) (id 'c))))
                               (list (op mexpt (list (id 'c) (num 3))))))
                     (list (op - (list (id 'a) (num 1))))))(list (num 2))))
(test (desugar
       (parse w*4))
      (app (fun '(a)
                (app (fun '(b)
                          (app (fun '(c)
                                    (op -
                                        (list (id 'c)
                                              (op +
                                                  (list
                                                   (id 'a) (id 'b) (id 'b))))))
                               (list (op * (list (id 'b) (id 'b))))))
                     (list (op - (list (id 'a) (num 5))))))(list (num 10))))
(test
 (desugar (parse w*5))
 (app
  (fun '(a)
       (app
        (fun '(b)
             (app
              (fun '(c)
                   (op / (list (id 'b)
                               (op * (list (id 'a) (id 'c))))))
              (list (op * (list (id 'b) (id 'b))))))
        (list (op - (list (id 'a) (num 2))))))
  (list (op + (list (num 3) (num 1))))))

(test (desugar (parse f1))
      (fun '(x) (op * (list (id 'x) (num 2)))))
(test (desugar (parse f2))
      (fun '(x) (op + (list (id 'x) (num 3)))))
(test (desugar (parse f3))
      (fun '(x)
           (op + (list
                  (op * (list (id 'x) (num 2)))
                  (op mexpt (list (id 'x) (num 3)))))))
(test (desugar (parse f4))
      (fun '(x y z) (op min (list (id 'x) (id 'y) (id 'z)))))
(test (desugar (parse f5))
      (fun '(r s) (op modulo (list (id 'r) (id 's)))))
(test (desugar (parse f6))
      (fun '(x) (op * (list (id 'x) (id 'x)))))

(test (desugar (parse ap1))
      (app (fun '(x) (op + (list (id 'x) (num 1)))) (list (num 2))))
(test (desugar (parse ap2))
       (app (fun '(a b) (op + (list (id 'a)
                                    (op / (list (id 'b) (num 4))) (num 10))))
            (list (num 3) (num 4))))

(test (desugar (parse ap3))
      (app (fun '(a b)
                (op mexpt (list (id 'a) (id 'b)))) (list (num 2) (num 10))))
(test (desugar (parse ap4))
      (app (fun '(b h)
                  (op /
                      (list (op * (list (id 'b) (id 'h))) (num 2))))
           (list (num 2) (num 4))))
(test (desugar (parse ap5))
      (app (fun '(f a)
                (app (id 'f) (list (id 'a))))
           (list (fun '(x) (op mexpt (list (id 'x) (num 2)))) (num 3))))
(test (desugar (parse ap8))
      (app
       (fun '(areaC radio) (app (id 'areaC) (list (id 'radio))))
       (list
        (fun '(r) (op * (list (num 3.1416)
                              (op mexpt (list (id 'r) (num 2))))))
        (num 3))))
(test (desugar
       (parse ap9))
      (app
       (fun '(f g a b) (app (id 'g) (list (app (id 'f) (list (id 'a)))
                                          (app (id 'f) (list (id 'b))))))
       (list (fun '(x)
                  (op modulo (list (id 'x) (num 10))))
             (fun '(x y)
                  (op + (list (id 'x) (id 'y)))) (num 5) (num 2))))
(test (desugar
       (parse ap10))
      (app (fun '(x y)
                (op min (list (id 'x) (id 'y))))
           (list (app (fun '(x y) (op max (list (id 'x) (id 'y))))
                      (list (num 21) (num 24))) (num 21))))

(test (desugar (parse ex1)) (throws 'Milw0rmExcepcion))
(test (desugar (parse ex2))
      (try/catch (list (binding 'DivisionEntreCero (num 2)))
                 (throws 'DivisionEntreCero)))
(test (desugar (parse ex3))
      (try/catch (list (binding 'k (op + (list (num 1) (num 2)))))
                 (op + (list (throws 'k) (num 3) (num 4) (num 5)))))
(test (desugar (parse ex4))
      (try/catch (list
                  (binding 'DivisionPorCero (num 2))
                  (binding 'Milw0rmE (num 9796)))
                 (op * (list (num 1) (throws 'Milw0rmE)))))
(test (desugar (parse ex5))
      (try/catch
       (list
        (binding 'E1 (op modulo (list (num 2) (num 7))))
        (binding 'E2 (num 8))
        (binding 'E3 (op + (list (num 7) (num 8) (num 9)))))
       (iF
        (op or-aux (list (bool #f) (bool #f))) (throws 'E1)
        (iF (op /= (list (num 7) (num -7))) (throws 'E2)
            (throws 'E3)))))
(test (desugar (parse ex8))
      (try/catch (list (binding 'E0 (num 1000))) (throws 'Excepcion)))

;; Pruebas interp.

(test/exn (interp (desugar (parse id1)) (mtSub)) "error: Free identifier")
(test/exn (interp (desugar (parse id2)) (mtSub)) "error: Free identifier")

(test (interp (desugar (parse n1)) (mtSub)) (numV -1))
(test (interp (desugar (parse n2)) (mtSub)) (numV 1729))
(test (interp (desugar (parse n3)) (mtSub)) (numV 1/8))
(test (interp (desugar (parse n4)) (mtSub)) (numV 1.234567))
(test (interp (desugar (parse n5)) (mtSub)) (numV 1+2i))

(test (interp (desugar (parse ol1)) (mtSub)) (boolV #f))
(test (interp (desugar (parse ol2)) (mtSub)) (boolV #t))
(test (interp (desugar (parse ol3)) (mtSub)) (boolV #t))
(test (interp (desugar (parse ol4)) (mtSub)) (boolV #t))
(test (interp (desugar (parse ol5)) (mtSub)) (boolV #f))

(test (interp (desugar (parse op1)) (mtSub)) (numV 64))
(test (interp (desugar (parse op2)) (mtSub)) (numV  8))
(test (interp (desugar (parse op3)) (mtSub)) (numV 20/49))
(test (interp (desugar (parse op4)) (mtSub)) (numV 7))
(test (interp (desugar (parse op5)) (mtSub)) (numV 1/8))

(test (interp (desugar (parse l1)) (mtSub)) (boolV #t))
(test (interp (desugar (parse l2)) (mtSub))
      (listV (list (numV 0) (numV 1) (numV 2) (numV 3))))
(test (interp (desugar (parse l3)) (mtSub)) (boolV #f))
(test (interp (desugar (parse l4)) (mtSub))
      (listV
       (list (closureV '(a)
                       (op +
                           (list (id 'a) (id 'a))) (mtSub))
             (boolV #t) (numV 4))))
(test (interp (desugar (parse l5)) (mtSub)) (numV 9))

(test (interp (desugar (parse rec1)) (mtSub)) (numV 120))
(test (interp (desugar (parse rec2)) (mtSub)) (numV 13))
(test (interp (desugar (parse rec3)) (mtSub)) (numV 2))
(test (interp (desugar (parse rec4)) (mtSub)) (numV 1000000))
(test (interp (desugar (parse rec5)) (mtSub)) (numV 56))

(test (interp (desugar (parse if1)) (mtSub)) (numV 1997))
(test (interp (desugar (parse if2)) (mtSub)) (numV 5))
(test (interp (desugar (parse if3)) (mtSub)) (numV 21))
(test (interp (desugar (parse if4)) (mtSub)) (numV 7))
(test (interp (desugar (parse if5)) (mtSub)) (numV 64))
(test (interp (desugar (parse if6)) (mtSub)) (numV 44+3i))

(test (interp (desugar (parse cond1)) (mtSub)) (numV 189))
(test (interp (desugar (parse cond2)) (mtSub)) (numV 11))
(test (interp (desugar (parse cond3)) (mtSub)) (numV 4))
(test (interp (desugar (parse cond4)) (mtSub)) (numV 5))
(test (interp (desugar (parse cond5)) (mtSub)) (numV 74))
(test (interp (desugar (parse cond6)) (mtSub)) (numV 1968+4i))
(test (interp (desugar (parse cond7)) (mtSub)) (numV 99.9))

(test (interp (desugar (parse w1)) (mtSub)) (numV 7))
(test (interp (desugar (parse w2)) (mtSub)) (numV 0))
(test (interp (desugar (parse w3)) (mtSub)) (numV 11))
(test (interp (desugar (parse w4)) (mtSub)) (boolV #f))
(test (interp (desugar (parse w5)) (mtSub)) (boolV #t))

(test (interp (desugar (parse w*1)) (mtSub)) (numV 4))
(test (interp (desugar (parse w*2)) (mtSub)) (numV 17))
(test (interp (desugar (parse w*3)) (mtSub)) (numV 1))
(test (interp (desugar (parse w*4)) (mtSub)) (numV 5))
(test (interp (desugar (parse w*5)) (mtSub)) (numV 1/8))

(test (interp (desugar (parse ap1)) (mtSub)) (numV 3))
(test (interp (desugar (parse ap2)) (mtSub)) (numV 14))
(test (interp (desugar (parse ap3)) (mtSub)) (numV 1024))
(test (interp (desugar (parse ap4)) (mtSub)) (numV 4))
(test (interp (desugar (parse ap5)) (mtSub)) (numV 9))

(test (interp (desugar (parse ex2)) (mtSub)) (numV 2))
(test (interp (desugar (parse ex3)) (mtSub)) (numV 15))
(test (interp (desugar (parse ex4)) (mtSub)) (numV 9796))
(test (interp (desugar (parse ex5)) (mtSub)) (numV 8))
(test (interp (desugar (parse ex6)) (mtSub)) (numV -2))
(test (interp (desugar (parse ex7)) (mtSub)) (boolV #t))

;(test (interp (desugar (parse ex1)) (mtSub)) (exceptionV Milw0rmExcepcion #<continuation>))
;(test (interp (desugar (parse ex8)) (mtSub)) (exceptionV 'Excepcion #<continuation>))