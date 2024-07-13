
;; SECTION 0: How to look things up, i.e. documentation

;; BEST WAY TO LOOK UP DOCS
;;
;; M-x sly-describe-symbol ; MUST have cursor on the thing you want documentation for
;; NOTE you can just use SPC m h h, or even better, just K in doom emacs!
;; incredible. So tl;dr to look up documentation, just move the point
;; over the first character of the function call, and press K in emacs (evil mode)
;; Reference is made to precisely this method of looking up documentation in
;; default ~/.config/doom/config.el file! - here's the important bit:

;; | To get information about any of these functions/macros, move the cursor over
;; | the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; | This will open documentation for it, including demos of how they are used.

;; EXAMPLE
;; on line 35 of ~/.config/doom/config.el we see:
;; (setq doom-theme 'doom-one)
;; to get documentation for the (setq) function, move the point (cursor) over
;; the first 's' in setq, and press K on the keyboard. Boom!


;; SECOND BEST WAY? I think the above is for elisp, so I can use M-x sly-describe-symbol which seems to do the same thing but for common lisp!


;; THIRD BEST WAY TO LOOK UP DOCS
(documentation 'cons 'function) ; where cons can be any function








;; SECTION 1: Noodling & basics

(print "hi")

(print (+ 2 (* 3 3)))

(print "hehe")




;;; DEFINING VARIBLES

;;; sc note this:

;;; (defvar aaa 5)
;;; (defvar aaa 7)
;;; (print aaa)
;;; 5
;;;
;;; (setf aaa 9)
;;; (print aaa)
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
;;; sc: yup, for behaviour more like what I'm used to, use defparamater
;;; rather than defvar





(defvar x 6)

(print 5)

(print x)


;;; quick summary of 5 ways to define variables in common lisp


;;; Defines a global variable and initializes it with a value. The variable can be changed later.
(defparameter *var* 10)

(documentation 'defparameter 'function)

;;; "Define a parameter that is not normally changed by the program,
;;; but that may be changed without causing an error. Declare the
;;; variable special and sets its value to VAL, overwriting any
;;; previous...[sly-elided string of length 283]"

;;; Similar to defparameter, but if the variable already exists, it does not reinitialize it.
(defvar *var* 10)


;;;setq sets the value of a variable. Can be used for both local and global variables.
(setq var 20)

;;; let defines local variables scoped within a block.

(let ((x 10)
      (y 20))
  (+ x y))


;;; let* similar to let, but allows for variables defined in the form to
;;; depend on those defined earlier
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
;; note: if you try to define the function before defining its variables,
;; it errors. Wasn't expecting that tbh
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

;; observation: the common lisp REPL doesn't seem to wrap by
;; default (it can print past the right edge of screen)



;; some arithmatic (from derek)

(terpri)
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
;; note that #' is shorthand for function (i.e. whatever
;; follows it is a function, in this case the minus sign)

(reduce #'-
        (reverse (list 1 2 3)))

(mapcar #'string-downcase '("Hello, World!"))

;;; these could be rewritten as

(reduce (function -)
        (reverse (list 1 2 3)))
(mapcar (function string-downcase) '("Hello, World!"))
;;;;;;;;;;;;;;;;;;;; Side note: Delete this ^ parenthesis and try to replace it
;;;;;;;;;;;;;;;;;;;; Emacs does some weirdness, moving cursor to corresponding paren at the end
;;;;;;;;;;;;;;;;;;;; But only if you have a third (erroneous) paren at the end

(reduce '- (reverse (list 1 2 3)))
(mapcar 'string-downcase '("Hello, World!"))

;;; tl;dr there's actually three ways to do the same thing
;;; #' and (function) are the same, but ' does something slightly
;;; different which might result in the same outcome most the time

;; ⇒ 0
;; sc: explanation of the reduce/reverse example above:
;; first the list is reversed (so it's 3 2 1), then reduce applies the - (subtraction)
;; operator to each of the successive elements e.g. 3-2, then the result - 1, =0
;; in other words 'reduce' successively applies the function to the first and second elemnts and moves along
;; to go through all elements
;; + might have been a simpler (if less elegant) example:

(reduce #'+
        (list 1 2 3 4))
;; 10


;; a bit more on using #' (alisas for (function)) vs just '
;; from common lisp discord
(defun foo () (write-line "Global foo"))

(foo)

(funcall #'foo) ; => Global foo
(funcall 'foo) ; => Global foo

(flet ((foo () (write-line "Local foo")))
  (funcall #'foo) ; => Local foo
  (funcall 'foo)) ; => Global foo


;; Use #' to get the function object from the relevant lexical scope.
;; This means that if there's a local binding (e.g., within a `flet`
;; or `labels` block), it will take precedence over a global definition.

;; sc: (my 2c) interesting meta-learning googling (static|dynamic|lexical) scoping
;; is to research categories (labels), but understanding the underlying concept
;; of 'scope resolution order' (a term not found so commonly on literature)
;; seems a much more fruitful to me, since you stop trying to understand each label
;; and realise each language can do whatever the hell it wants (it just happens)
;; that most languages fit into one of a few different categories above


;; From: https://lisp-lang.org/
(mapcar #'string-downcase
        (list "Hello" "World!"))
;; => ("hello" "world!")

;; what's it doing?

(string-downcase "Boom!")
;;  boom!



;;; how to look up documentation for a lisp function
;;; my tldr:
;;; use (describe) since it has one less argument than (documentation)
;;; e.g.
(describe 'defun)
(describe '*print-base*)
(describe '*print-case*)
(setq *print-case* :capitalize)
(print *print-case*) ; this prints the new value (:Capitalize)
(describe '*print-case*) ; but this says no value. Maybe a lisp quirk. Investigate later.

(documentation 'format 'function)

;; OR

(describe 'format)
;;; Note that this ^^^ doesn't seem to work
;; apparently this site is really good: https://www.lispworks.com/documentation/lw50/CLHS/Front/Contents.htm

;;; So you probably have to tell the (documentation) function the kind
;;; thing that it is. E.g.

;;; returns documentation for the + function (although not much documentation)
(documentation '+ 'function)
(documentation 'car 'function)

;;; returns documentaiton for a variable
(documentation '*print-base* 'variable)

(describe '+)

;;; documenting your own code
(defun my-function (x)
  "Calculate the square of X."
  (* x x))

(documentation 'my-function 'function)


;; great tutorial: https://lisp-lang.org/learn/functions
;; another nice one pager: https://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html


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
;; nums: (1 2 3 4)!

;; automatically gives the optional argument a value of NIL
(mynums 1 2 3)
;; nums: (1 2 3 Nil)!

(mynums 1 2)
;; nums: (1 2 Nil Nil)!

;;; check if something is a list

(defvar thing 222)
(defvar thing2 (list 1 2 3))
(listp thing)
;;; NIL (aka false)
(listp thing2)
;;; T

(documentation 'listp 'function)


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

(dolist (thing a) ; a is the list defined a few lines up ^^
  (print thing))




;;; sc quick primer on (dolist) and (mapcar)

(documentation 'dolist 'function) ; doesn't work for some reason
;;; Since it doesn't work, I looked documentation up online via a google
;;; search for dolist site:https://www.lispworks.com/
;;; (note that lispworks.com is called 'hyperspec')
;;; https://www.lispworks.com/documentation/lw60/CLHS/Body/m_dolist.htm

;; these also don't work, which I find surprising
(documentation 'dolist 'macro)
(documentation 'dolist 'symbol)



(dolist (element '(1 2 3 4))
  (print element))

(dolist (el '(1 2 3 4))
  (print el))
;;; got it. the first argument is the same as a block parameter
;;; in ruby and the second argument is the list to enumerate over
;;; 'el' in the above case is called a 'temporary holding cell'. See: https://www.youtube.com/watch?v=ymSq4wHrqyU&t=35m20s

;; So basically in lisp

;; (dolist (el '(1 2 3))
;;   (format t "~a~%" el))

;; is the same as this in ruby:

;; [1, 2, 3].each do |el|
;;   puts el
;; end

;; sc: i wonder if we can use an anonymous function with dolist?
;; ans, of course, just put it in directly, no need for anything fancy

(dolist (arg '(1 2 455))
  (print arg))
;; was literally doing this above :'( hehe





;;; Quick primer on mapcar
;;; mapcar takes a function and applies it to each list element
(defun plus_two (x)
  (+ x 2))
(plus_two 3)
(mapcar 'plus_two '(1 2 3)) ; sc something can confuse here
                            ; note '(1 2 3) only where you're defining
                            ; the list in-place, if using an-already
                            ; defined list, no ' is necessary on the list
                            ; argument (still used on the function tho)

(defparameter newlist (list 10 20 30))
(listp newlist)
(mapcar 'plus_two newlist)

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

;;; what is 'arg'

(documentation 'arg 'function)

;;; "Return the N'th argument's value if possible. Argument zero is the first
;;;  argument in a frame's default printed representation. Count keyword/value
;;;  pairs as separate arguments."

(documentation 'cons 'function)
;;; "Return a list with SE1 as the car and SE2 as the cdr"
;;; eg.
;;; (cons 'a 'b)
;;; (A . B) ; displayed in dot notation


;;; exploring cons
;;; quite a profound example. here we end up calling eval not
;;; on "2 + 2" but on
;;; a list (+ 2 2)
(defparameter operation '+)
(defparameter arguments (list 2 3))
(defparameter func-call (cons operation arguments))
(print func-call) ; (+ 2 2)
(type-of func-call) ; type-of gets the type of anything sc - THIS could be handy - get any object's type!
(eval func-call)
;; sc: note that chatgpt says there's something wrong with the way I define
;; the arguments variable (it thinks it shouldn't be a list for some reason)
;; so just be careful that the above example might be wrong even though it
;; appears to work

;; or another way to look at it
(eval (cons '+ '(2 3)))
;; 5

;;; another macro example: https://lisp-lang.org/learn/macros





(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))


(macroexpand-1 '(while (> a -2)
         (format t "~%a is: ~a~%" a)
         (setq a (- a 1))))

;;; Not totally sure I got this right, but here's what it expands to
;;; (LOOP WHILE (> A -2)
;;;      DO (PROGN (FORMAT T "~%a is: ~a~%" A) (SETQ A (- A 1))))
;;; T

;;; Reminder of expansion of first macro above just for comparison
(defmacro defun-logged (name arglist &body body)
  `(defun ,name ,arglist
     (format t "fn = ~a~%" ',name)
     (dolist (arg (mapcar #'cons ',arglist (list ,@arglist)))
       (format t "~a = ~a~%" (car arg) (cdr arg))) ,@body))


(macroexpand-1 '(defun-logged test (x y) (+ x y)))





;;; reminder of how equality works


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


;;; Note the difference between '(a b) and (list a b) is the former uses a quote before
;;; the parentheses, which inhibis the evaluation of the contents inside the parantheses
;;; e.g. it directly returns (a b) as a literal list, with symbols a and b unchanged.
;;; Whereas (list a b) is a function with a and b as arguments, so it will be evaluated
;;; but only after a and b are evaluated as variables adn their values used in the (list)
;;; function call to create a new list.
;;;




;;; paperclip math

(defparameter start 13107200)

;;; a function to double a number (x), n times

(defun double_me (x n)
  (* x (expt 2 n)))

(double_me 3200 2)

(mapcar #'double_me (list (list 3200 1) (list 3200 2)))

(mapcar 'double_me (list (list 3200 1) (list 3200 2)))


(defun double_me (x n)
  (* x (expt 2 n)))

(mapcar (lambda (x n) (double_me x n)) '(1 2 3 4 5) '(0 1 2 3 4))

(let ((n 3))
  (mapcar (lambda (x) (double_me x n)) '(1 2 3 4 5)))




(loop for i from 1 to 5
      do (print i))

(print start)
(loop for i from 1 to 5 do
      (print i)
      (print (expt start 2))
    ;;  (print (* start (exp 2 i)))
)


(loop
	(format t "~d ~%" x)
	(setq x (+ x 1))
	(when (> x 10) (return x))
)


(pp-eval-last-sexp)


(* 1500 1.75)
;; 2625
