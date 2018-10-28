;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;;*NOTE: we won't worry about using TCO for this HW

;; Problem 1

(define (racketlist->mupllist Rlst)
  (if (null? Rlst)
      (aunit)
      (apair (car Rlst) (racketlist->mupllist (cdr Rlst)))))

(define (mupllist->racketlist Mlst)
  (if (aunit? Mlst)
      null
      (cons (apair-e1 Mlst) (mupllist->racketlist (apair-e2 Mlst)))))

; didn't use (eval-exp (fst...) and (eval-exp (snd...) here because this would allow
; the function to work on non-MUPL values, and lead to "bad MUPL expression" error thrown
; in eval-exp, which is poor style (apparently, the grader thinks this...)
                              

;; CHANGE (put your solutions here)

;;*NOTE: the environment is a list of pairs (var -> val)

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))] ; if we find the matching variable name, extract the value from that var-val pair in th eenv
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        ;; CHANGE add more cases here

        [(int? e) e]

        [(closure? e) e]

        [(aunit? e) e]

        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        
        [(apair? e)
        (let ([v1 (eval-under-env (apair-e1 e) env)]
              [v2 (eval-under-env (apair-e2 e) env)])
          (apair v1 v2))]      

        [(fst? e)
        (let ([v (eval-under-env (fst-e e) env)])
          (if (apair? v)
              (apair-e1 v)
              (error "'fst' not applied to apair")))]

        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "'snd' not applied to apair")))]

        [(fun? e)
         (closure env e)]

        [(ifgreater? e) ; if e1 > e2 then e3 else e4
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               [v3 (eval-under-env (ifgreater-e3 e) env)]
               [v4 (eval-under-env (ifgreater-e4 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   v3
                   v4)
               (error "'ifgreater' not applied to two ints")))]

        [(mlet? e) ; (mlet var e body)
         (let* ([vName (mlet-var e)] ; get the variable name
               [val (eval-under-env (mlet-e e) env)] ; get the value
               [curEnv (append (list (cons vName val)) env)]) ; append the new vName-val pair to the env
           (eval-under-env (mlet-body e) curEnv))] ; evaluate the body of mlet in curEnv

        
        ; a call evaluates a function body in a particular environment
        [(call? e) ; (call funexp actual) ... clsr-env clsr-fun
         (let* ([clsr (eval-under-env (call-funexp e) env)]) ; get the current closure
           (if (not (closure? clsr))
               (error "'call' not applied to a valid closure")
               ; else, begin evaluation of clsr function body in env
               (let* ([f (closure-fun clsr)] ; create variable of function in closure
                      [clsrEnv (closure-env clsr)] ; create variable of environment in closure
                      [fName (fun-nameopt f)] ; create variable with function name of f
                      
                      ; now we need to extend the closure environment to include the given function
                      ; and we need to evaluate the function body in that extended environmeent
                      ; with the actual parameters given to the function

                      ; so, first: if there is a fName (i.e. not an anon function),
                      ; then we extend environment with fName and its closure
                      [clsrEnv (if fName (append (list (cons fName clsr)) clsrEnv) clsrEnv)]
                      ; then we want to get the formal parameters from the function in clsr
                      [formals (fun-formal f)]
                      ; then we want to get the values of the actual parameters in the current environment
                      [actuals (eval-under-env (call-actual e) env)]
                      ; then we want to add the formals-actuals pair to the environment
                      [clsrEnv (append (list (cons formals actuals) clsrEnv))]
                      ; then we want to get the function body in which we will evaluate this new, extended clsrEnv
                      [fBody (fun-body f)])
                 ; lastly we evaluate the new formals-actuals pairs in fBody as included the clsrEnv list of pairs 
                 (eval-under-env fBody clsrEnv))))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3 : writing racket functions that work like MUPL macros

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

; evaluate e2 in an environment where each s_i is bound to the evaluation of e_i
; and each pair is evaluated sequentially in an environment that is continuously updating
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      ; (mlet var e body)
      (mlet (car (car lstlst)) #|var s_i|# (cdr (car lstlst)) #|val e_i|# (mlet* (cdr lstlst) e2)))) #|expression to be evaluated stays the same|#  
 
; evaluate e3 only if e1 and e2 are equal
; would have been much easier if we could have added something like an "isequal"
; structure to our MUPL language, but not allowed to add any new structs

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 #|start of body of first mlet|#
        (mlet "_y" e2 #|start of body of second mlet|#
              ; we'll use a nested "ifgreater" to compare equality of
              ; "_x" and "_y":
              (ifgreater (var "_x") (var "_y")
                         e4 ; e3 in "ifgreater"
                         (ifgreater (var "_y") (var "_x") ; use entire nested "ifgreater" as e4 for outer "ifgreater"
                                    e4
                                    e3)))))

;; Problem 4

; mupl-map: a function that takes a MUPL function and returns a MUPL function
; that takes a MUPL list and applies this function to every element in the MUPL list,
; creating a new MUPL list (i.e. mupl-map is curried)
(define mupl-map
  ; we will have to use nested MUPL "fun" structures where we use MUPL "call"
  ; within in order to evaluate the nested function
  
  ; fun (nameopt formal body)
  ; call (funexp actual)

  ; we'll want to cons the head of mList evaluated with "fTaken" from "f1"
  ; in "f2", to the tail of mList evaluated with "f2" (recursively) 
  (fun "f1" "fTaken" ; nameopt and formal of outer function 
       (fun "f2" "mList" ; begin body of outer function (and nameopt and formal of nested function)
            (ifaunit (var "mList") ; if we're at the end of mList
                     (aunit) ; return aunit
                     (apair ; else
                      (call (var "fTaken") (fst (var "mList"))) ; call "fTaken" on first element of mList
                      (call (var "f2") (snd (var "mList")))))))) ; and cons it to calling "f2" on tail of mList


(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "i2"
                                    (add (var "i") (var "i2")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
