
;; SECTION 1: Noodling & basics

(print "hi")

(print (+ 2 (* 3 3)))

(print "hehe")




;;; DEFINING VARIBLES

;;; sc note this:

;;; CL-USER> (defvar aaa 5)
;;; CL-USER> (defvar aaa 7)
;;; CL-USER> (print aaa)
;;; 5
;;;
;;; CL-USER> (setf aaa 9)
;;; CL-USER> (print aaa)
;;; 9

;;; in other words, defvar is like orequals, except ||= will define if the object is
;;; nil, whereas I don't think defvar does that. Basically defvar will set the
;;; variable only if it isn't already defined e.g. useful for global variables
;;; so tl;dr someone like me who rarely uses globals probably should just stick
;;; to using defparameter rather than defvar


(defparameter aaa 5)
(print aaa)
;;; 5

(defparameter aaa 7)
(print aaa)
;;; 7
;;; sc: yup, for behaviour more like what I'm used to, use defparamater rather than defvar





(defvar x 6)

(print 5)

(print x)


;;; quick summary of 5 ways to define variables in common lisp


;;; Defines a global variable and initializes it with a value. The variable can be changed later.
(defparameter *var* 10)


;;; Similar to defparameter, but if the variable already exists, it does not reinitialize it.
(defvar *var* 10)


;;;setq sets the value of a variable. Can be used for both local and global variables.
(setq var 20)

;;; let defines local variables scoped within a block.

(let ((x 10)
      (y 20))
  (+ x y))


;;; let* similar to let, but allows for variables defined in the form to depend on those defined earlier
(let* ((x 10)
       (y (+ x 10)))
  (+ x y))




;;; some great examples from a stack overflow answer:
;;; https://stackoverflow.com/a/1475814/5783745
;;;
(set ls '(1 2 3 4)) => Error - ls has no value

(set 'ls '(1 2 3 4)) => OK

(setq ls '(1 2 3 4)) => OK - make ls to (quote ls) and then have the usual set

(setf ls '(1 2 3 4)) => OK - same as setq so far BUT

(setf (car ls) 10) => Makes ls '(10 2 3 4) - not duplicated by setq/set












;;; DEFINING FUNCTIONS



;; from derek banas
(defun hello-you (name)
  (format t "Hello ~a! ~%" name)
)

;; reads from prompt
(defvar *name* (read))
(hello-you *name*)

;; sets global variable *name* to "John"
(defvar *name* "John")
(hello-you *name*)


;; my tinkering
;; note: if you try to define the function before defining its variables, it errors. Wasn't expecting that tbh
(defun hello-yoooo (name age)
  (format t "Sup ~a! you're ~a years old! ~%" name age)
)


;; set this then run the above again, it will work
(defvar age)


(hello-yoooo "Sue" 22)





;; symbols (absolutely no clue what they do yet)
'milkshake



;; more from derek
(setq *print-case* :capitalize)
(format t "Number with commas ~:d" 10000)

(format t "PI to 5 characters ~5f" 3.141593)

(format t "PI to 4 decimals ~,4f" 3.141593)

(format t "10 Percent ~,,2f'" .10)

(format t "Number with commas ~:d" 10000)

;; observation: the common lisp REPL doesn't seem to wrap by default (it can print past the right edge of screen)



;; some arithmatic (from derek)


(format t "(expt 4 2) = ~d ~%" (expt 4 2))
(format t "(sqrt 81) = ~d ~%" (sqrt 81))
(format t "(expt 4 2) = ~d ~%" (expt 4 2))
(format t "(expt 4 2) = ~d ~%" (expt 4 2))
(format t "(expt 4 2) = ~d ~%" (expt 4 2))






;; paste this into a blank file and name it hello.lisp, then clisp hello.lisp
;; can also run with sbcl --script hello.lisp
;; it's from: https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/#s12-hello-lisp
(defun hello ()
  (write-line "What is your name?")
  (let ((name (read-line)))
    (format t "Hello, ~A.~%" name)))
(hello)






;; From: https://lisp-lang.org/
(reduce #'-
        (reverse (list 1 2 3)))
;; ⇒ 0
;; sc: explanation: first the list is reversed (so it's 3 2 1), then reduce applies the - (subtraction)
;; operator to each of the successive elements e.g. 3-2, then the result - 1, =0
;; in other words 'reduce' successively applies the function to the first and second elemnts and moves along
;; to go through all elements
;; + might have been a simpler (if less elegant) example:

(reduce #'+
        (list 1 2 3 4))
;; 10




;; From: https://lisp-lang.org/
(mapcar #'string-downcase
        (list "Hello" "World!"))
;; => ("hello" "world!")

;; what's it doing?

(string-downcase "Boom!")
;;  boom!



;; how to look up documentation for a lisp function

(documentation 'format 'function)

;; OR

(describe 'format)

;; apparently this site is really good: https://www.lispworks.com/documentation/lw50/CLHS/Front/Contents.htm




;; great tutorial: https://lisp-lang.org/learn/functions
;; another nice one pager: https://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html


(defun fib (n)
  "Return the nth Fibonacci number."
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


(defun fib (n)
  "Return the nth Fibonacci number."
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 20)
;;  6765
(fib 30)
;; 832040

;; another way to run it
(funcall #'fib 30)

;; and yet another way

(apply #'fib (list 30))

(mapcar #'fib (list 10 20 30))
;; (55 6765 832040)

(mapcar #'string-downcase
        (list "Hello" "World!"))

;; equalities and case statements (from derek ~19m)

(defvar age 18)

(if (= age 18)
    (format t "You can vote~%")
    (format t "You can't vote~%"))

;; how to do not equal to
(defvar age 15)
(if (not(= age 18))
    (format t "Too young~%")
    (format t "You can vote~%"))

;; multiple conditions

(defvar a 20)
(if (and (> a 10) (< a 50))
    (format t "a inbetween 10 and 50~%")
    (format t "a is outside of 10 and 50~%"))


(defvar b 10)
(if (or (>= b 1000) (= (mod b 3) 0))
    (format t "at least on condition met~%")
    (format t "neither condition met~%"))


;; This was a real surprise! tl;dr NIL is lisps false
;; CL-USER> (> 3 2)
;; T
;; CL-USER> (> 2 3)
;; NIL




;; tangent: exploring 'truthiness' in common lisp
;; note, use T for true, not (t) or (T), since those will be interpreted as function calls
(if (or T T)
  (format t "true thang~%")
  (format t "false thang~%"))


(defvar a "bob")
(defvar b "jen")

(defun hello (x y)
  (format t "hello there ~a and ~a~%" x y))

(hello a b)



;; multple lines in lisp if statement
(defvar x 22)
(if (= x 22)
    (progn
      (format t "x is equal to...~%")
      (format t "you guessed it... ~a~%" x)
      )
    (format t "x is not 22~%"))


(>= 2 3)
;; NIL
(>= 3 2)
;; T


;; test numeric equality:
(= 2 2)
;; T
(= 5 1)
;; NIL

;; this ERRORS
(== 2 2)



;; these DON'T work since (I think) = is used for numeric equality, not boolean
(= (>= 3 2) (>= 3 2))
(== (>= 3 2) (>= 3 2))

(eql (>= 3 2) T)
;; T

;;
(eql NIL NIL)
;; T



'()


;; define a NIL object
(defparameter obj nil)

;; check that it is indeed nil
(null obj)
;; T


;; define a function with an optional parameter

(defun mynums (a b &optional c d)
  (format t "nums: ~a!~%" (list a b c d)))

(mynums 1 2 3 4)

;; automatically gives the optional argument a value of NIL
(mynums 1 2 3)



;;; check if something is a list

(defvar thing 222)
(defvar thing2 (list 1 2 3))
(listp thing)
;;; NIL (aka false)
(listp thing2)
;;; T


;;; Loops that print

(loop for i from 1 to 5
      do (print i))

(loop for i from 1 to 100
     do (format t "iteration number: ~a~%" i))


;;; make a list using a loop
(loop for i from 1 to 5
      collect i)

;;; assign that list
(defvar a (loop for i from 1 to 5
      collect i))


(print a)
;;; (1 2 3 4 5)


;;; iterating over list items

(dolist (item '(a b c d))
  (print item))











;;; Macros!
;;; From: https://www.quora.com/What-is-symbolic-programming/answer/Vladislav-Zorov

(defmacro defun-logged (name arglist &body body)
  `(defun ,name ,arglist
     (format t "fn = ~a~%" ',name)
     (dolist (arg (mapcar #'cons ',arglist (list ,@arglist)))
       (format t "~a = ~a~%" (car arg) (cdr arg))) ,@body))


(defun-logged test (x y) (+ x y))

(test 2 3)
;;; 5


;;; Expand the macro
(macroexpand-1 '(defun-logged test (x y) (+ x y)))

;;; (DEFUN TEST (X Y)
;;;   (FORMAT T "fn = ~a~%" 'TEST)
;;;   (DOLIST (ARG (MAPCAR #'CONS '(X Y) (LIST X Y)))
;;;     (FORMAT T "~a = ~a~%" (CAR ARG) (CDR ARG)))
;;;   (+ X Y))
;;; T








;;; another macro example: https://lisp-lang.org/learn/macros




(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))


(macroexpand-1 '(while (> a -2)
       (progn
         (format t "~%a is: ~a~%" a)
         (setq a (- a 1)))))

;;; Not totally sure I got this right, but here's what it expands to
;;; (LOOP WHILE (> A -2)
;;;       DO (PROGN (PROGN (FORMAT T "~%a is: ~a~%" A) (SETQ A (- A 1)))))
;;; T

;;; Reminder of expansion of first macro above just for comparison
(defmacro defun-logged (name arglist &body body)
  `(defun ,name ,arglist
     (format t "fn = ~a~%" ',name)
     (dolist (arg (mapcar #'cons ',arglist (list ,@arglist)))
       (format t "~a = ~a~%" (car arg) (cdr arg))) ,@body))


(macroexpand-1 '(defun-logged test (x y) (+ x y)))





;;; remidner of how equality works hehe


(defparameter a 2)
(print a)
(= a 2)

;;; my attempt to use it
(format t "~%~%~%~%--------------")
(while (> a -2)
       (progn
         (format t "~%a is: ~a~%" a)
         (setq a (- a 1))))
;;; if this ^^ does nothing, check that it hasn't already run. If it has, then a will
;;; already be -2, hence it won't do anything
;;; Also check that the sly repl hasn't done that thang where it doesn't move down
;;; with code evaluation


;;; Anatomy of a macro:
;;;
;;; Backquote: This is a special quoting mechanism used in Lisp macros.
;;; It allows parts of the quoted list to be evaluated and interpolated into the list,
;;; which is crucial for constructing new code dynamically.
;;;
;;; Comma: The comma is used within a backquoted expression to indicate that
;;; what follows is an expression that should be evaluated and the result spliced
;;; into the code during macro expansion.
;;;
;;; Splice (,@): The ,@ operator is used before an expression that results in a list,
;;; and it means that the list should be "spliced" into the surrounding code.
;;; That is, instead of inserting the list itself,
;;; the elements of the list are inserted.

