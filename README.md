# Programming-language-with-Racket

GRAMAR EBNF 
LANGUAGE ERCFWBAEL/L (Exceptions Recursive Conditionals functions With Boolean Arithmetic Expressions Lists and Lazy )

The gramar is defined as 
Is a lazy language, uses enviroments, has exceptions, and supports continuations
<code>
(define-type ERCFWBAEL/L
   [idS (i symbol?)]
   [numS (n number?)]
   [boolS (b boolean?)]
   [listS (elems (listof ERCFWBAEL/L?))]
   [opS (f procedure?) (args (listof ERCFWBAEL/L?))]
   [ifS (expr ERCFWBAEL/L?) (then-expr ERCFWBAEL/L?) (else-expr ERCFWBAEL/L?)]
   [condS (cases (listof Condition?))]
   [withS (bindings (listof bindingS?)) (body ERCFWBAEL/L?)]
   [withS* (bindings (listof bindingS?)) (body ERCFWBAEL/L?)]
   [recS (bindings (listof bindingS?)) (body ERCFWBAEL/L?)]
   [funS (params (listof symbol?)) (body ERCFWBAEL/L?)]
   [appS (fun-expr ERCFWBAEL/L?) (args (listof ERCFWBAEL/L?))]
   [throwsS (exception-id symbol?)]
   [try/catchS (bindings (listof bindingS?)) (body ERCFWBAEL/L?)])
   </code>
   
   That are the expresions acepted by the language 
   
   There is a function called desugar that tranforms this language to a version 
   without syntactic sugar 
   
   after that, in the file parser, is the parses, 
   
   receives an expresion ERCFWBAEL/L and an enviroment it returns an abstrac syntactic tree
   
   In the file Interp, is the interpreter 
   The interpreter receives an abstract syntactic tree and an enviroment, it returns an result, 
   interpreting the tree using the racket interpreter 
   
   
  


