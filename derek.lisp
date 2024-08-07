
;;; note: this doc grew of a life of its own. It's based on derek's tutorial
;;; but now has a lot of my notes imbued


;;; sc random quick notes
;; remember that in lisp, Nil is false, and anything other than
;; NIL and F is true
;; e.g.
(eql 2 2) ; T
(eql 2 3) ; Nil (false) <---- gives nil which means false

(eql T T)
(eql Nil Nil)
(eql T Nil)

;;; sc my notes on testing for equality in lisp

(documentation 'eq 'function)
;;; "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."

(documentation 'eql 'function)
;;; "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."

(documentation 'equal 'function)
;;; "Return T if X and Y are EQL or if they are structured components whose
;;; elements are EQUAL. Strings and bit-vectors are EQUAL if they are the same
;;; length and have identical components. Other arrays mus...[sly-elided string of length 220]"

(documentation 'equalp 'function)
;;; "Just like EQUAL, but more liberal in several respects.
;;;  Numbers may be of different types, as long as the values are identical
;;; after coercion. Characters may differ in alphabetic case. Vectors and
;;; ..[sly-elided string of length 370]"



;;; sc: short but thorough examples from chatgpt
(eq 'a 'a) ; True
(eq 'a 'b) ; Nil aka False
(eq 3 3)   ; Implementation-dependent (often False for numbers) sc: seems to be True in common lisp

(eql 'a 'a) ; True
(eql 3 3)   ; True
(eql 3.0 3) ; Nil aka false, different types (float vs integer)

(equal "abc" "abc") ; Tru(eq 'a 'a) ; True
(equal '(1 2) '(1 2)) ; True
(equal '(1 2) '(1 3)) ; (Nil) False

;;; sc: most common one, ignores differences in case for string/characters and treats numeric types as equal if values are equal
(equalp "abc" "ABC") ; True
(equalp 3 3.0)       ; True
(equalp '(a (b c)) '(A (B C))) ; True




;;; Defining things
(equal '(1 2) (list 1 2))
;; T

;; the ' is shorthand for (list)
;; just like #' is shorthand for (function)




;;; sc: most of the following vcode was taken from the link
;;; to code for derek's video: https://www.newthinktank.com/2015/07/learn-lisp-one-video/

;;;; Lisp stands for List Processing, not
;;;; Lots of Irritating Superflous Parentheses
;;;; Lisps great strength is that you can use data to generate code

;;; ---------- INTRO ----------


;;; sc: I researched why, and some editors/linters might display ; ;; and ;;; a bit differently,
;;; but by and large the REPL and compiler won't (so they're basically interchangable)

;;; Comment
;; Comment indented with code
; Comment after a line of code

#||
Multiline Comment
||#
;;; sc TODO - emacs wrecks havoc when trying to add new lines in a multiline comment
;;; (doom emacs keeps automatically adding more multiline comments)
;;; update: select area, press M-x (alt x), type comment-region



;;; ~% prints a newline with format
(format t "Hello world~%")

;;; The format statement starts with t to print to the console
;;; The control sequence begins with a ~
;;; ~a : Shows the value
;;; ~s : Shows quotes around the value
;;; ~10a : Adds 10 spaces for the value with extra space to the right
;;; ~10@a : Adds 10 spaces for the value with extra space to the left

;;; Print out a string without a newline
(print "What's your name ")

;;; Create a variable which receives the value passed by read
;;; A variable name or symbol is made of letters, numbers, and + - _ * = < > ? !
;;; and are lowercase because Lisp isn't case sensitive
;;; Convention is to use asterisks to surround global variable names
(defvar *name* (read))

;;; Create a function and say hello to value passed
;;; Your supposed to keep closing parentheses on the same line, but that
;;; is up to you if the code is easier to follow
(defun hello-you (*name*)
	(format t "Hello ~a!~%" *name*)
)

;;; Change the case to capitalize just the first letter (:upcase :downcase)
(setq *print-case* :capitalize)

(hello-you *name*)

;;; A form is a list with a command function name at the beginning
;;; Everything that follows the command is sent as parameters to the function
(+ 5 4) ; = 9

;;; You can nest a form inside of a form
(+ 5 (- 6 2)) ; = 9

;;; You define a Data Mode command by proceeding with a quote '
'(+ 5 4)

;;; Everything is a list in which each piece is held in a Cons Cell (Consecutive
;;; Cell) [+] [5] [4] [nil] with nil defining the end of the list

#||
sc note that this:
(cons '+ (cons 5 (cons 4 nil)))

will evaluate to
(+ 5 4)

Also note that in any linked list ("linked list" == "list"; same thing),
the first item is called the 'car' and all the other items are called the 'cdr'

E.g. in (+ 2 5), + is the car, and 2 and 5 are the cdr

||#

;;; a bit more on car

;;; here's a list
'(cat dog)
;;; we can get it's car (first value)
(car '(cat dog))
;;; -> CAT

;;; or its cdr
(cdr '(cat dog mouse))
;;; => (DOG MOUSE)

;;; TODO note: I don't quite grok the difference in symbol vs string in lists yet and how they can be used







;;; Change the value of a variable with setf
(setf *number* 6)
;;; sc: no idea why ^^ gives a massive warning

;;; You can define variables local to only the let body
;;; (let ((var-1 5) (var-2 10)) (... Body ...))

(let ((var-1 5)
	  (var-2 10))
	  	(print (+ var-1 var-2))
	  	(terpri) ; Prints a newline
)

;;; ---------- FORMAT ----------

(format t "Number with commas ~:d" 10000000)

(format t "PI to 5 characters ~5f" 3.141593)

(format t "PI to 4 decimals ~,4f" 3.141593)

(format t "10 Percent ~,,2f" .10)

(format t "10 Dollars ~$ ~%" 10)

;;; ---------- MATH FUNCTIONS ----------

(format t "(+ 5 4) = ~d ~%" (+ 5 4))
(format t "(- 5 4) = ~d ~%" (- 5 4))
(format t "(* 5 4) = ~d ~%" (* 5 4))
(format t "(/ 5 4) = ~d ~%" (/ 5 4)) ; = 5/4
(format t "(/ 5 4.0) = ~d ~%" (/ 5 4.0)) ; = 1.25
(format t "(rem 5 4) = ~d ~%" (rem 5 4)) ; = 1 Returns the remainder
(format t "(mod 5 4) = ~d ~%" (mod 5 4)) ; = 1 Returns the remainder

(format t "(expt 4 2) = ~d ~%" (expt 4 2)) ; = Exponent 4^2
(format t "(sqrt 81) = ~d ~%" (sqrt 81)) ; = 9
(format t "(exp 1) = ~d ~%" (exp 1)) ; = e^1
(format t "(log 1000 10) = ~d ~%" (log 1000 10)) ; = 3 = Because 10^3 = 1000
(format t "(eq 'dog 'dog) = ~d ~%" (eq 'dog 'dog)) ; = T Check Equality
(format t "(floor 5.5) = ~d ~%" (floor 5.5)) ; = 5
(format t "(ceiling 5.5) = ~d ~%" (ceiling 5.5)) ; = 6
(format t "(max 5 10) = ~d ~%" (max 5 10)) ; = 10
(format t "(min 5 10) = ~d ~%" (min 5 10)) ; = 5
(format t "(oddp 15) = ~d ~%" (oddp 15)) ; = T Check if 15 is odd
(format t "(evenp 15) = ~d ~%" (evenp 15)) ; = NIL = FALSE Check if 15 is even
(format t "(numberp 2) = ~d ~%" (numberp 2)) ; = T Is 2 a number
(format t "(null nil) = ~d ~%" (null nil)) ; = T Is something equal to nil

;;; There is also sin, cos, tan, asin, acos, atan

;;; ---------- EQUALITY ----------

;;; Symbols are compared with eq

(defparameter *name* 'Derek)
(format t "(eq *name* 'Derek) = ~d ~%" (eq *name* 'Derek))

;;; Everything else is compared with equal for the most part

(format t "(equal 'car 'truck) = ~d ~%" (equal 'car 'truck))
(format t "(equal 10 10) = ~d ~%" (equal 10 10))
(format t "(equal 5.5 5.3) = ~d ~%" (equal 5.5 5.3))
(format t "(equal \"string\" \"String\") = ~d ~%" (equal "string" "String"))
(format t "(equal (list 1 2 3) (list 1 2 3)) = ~d ~%"
	(equal (list 1 2 3) (list 1 2 3)))

;;; equalp can compare strings of any case and integers to floats
(format t "(equalp 1.0 1) = ~d ~%" (equalp 1.0 1))
(format t "(equalp \"Derek\" \"derek\") = ~d ~%" (equalp "Derek" "derek"))


;;; sc me testing this ^^ out (update: did a bunch of tests at the top of this doc)

(equal 2 2.0)
(eq 2 2.0)
(equalp "mystring" "mystring")






;;; ---------- CONDITIONALS ----------

(defparameter *age* 18) ; Create variable age

;;; Relational Operators > < >= <= =

;;; Check if age is greater than or equal to 18


;;; sc figuring out when I should ctrl+c ctrl+c Vs when to C-M-x.
;;; In this case, use C-M-x. Not totally sure when to use which tbh.
(if (= *age* 18)
(format t "You can vote~%")
(format t "You can't vote~%"))




;;; How to check for not equal

(if (not (= *age* 18))
(format t "You can vote~%")
(format t "You can't vote~%"))

;;; Logical Operators : and, or, not

(if (and (>= *age* 18) (<= *age* 67) )
(format t "Time for work~%")
(format t "Work if you want~%"))

(if (or (<= *age* 14) (>= *age* 67) )
(format t "You shouldn't work~%")
(format t "You should work~%"))

(defparameter *num* 2)
(defparameter *num-2* 2)
(defparameter *num-3* 2)

;;; You can execute multiple statements in an if with progn

(if (= *num* 2)
	(progn
		(setf *num-2* (* *num-2* 2))
		(setf *num-3* (* *num-3* 3))
	)
	(format t "Not equal to 2~%"))

(format t "*num-2* = ~d ~%" *num-2*)
(format t "*num-3* = ~d ~%" *num-3*)

;;; Case performs certain actions depending on conditions
(defun get-school (age)
	(case age
		(5 (print "Kindergarten"))
		(6 (print "First Grade"))
		(otherwise '(middle school))
	))

(get-school 5)

(terpri) ; Newline

;;; when allows you to execute multiple statements by default

(when (= *age* 18)
	(setf *num-3* 18)
	(format t "Go to college you're ~d ~%" *num-3*)
)

;;; With unless code is executed if the expression is false

(unless (not (= *age* 18))
	(setf *num-3* 20)
	(format t "Something Random ~%")
)

;;; cond is like if else if else

(defvar *college-ready* nil)

(cond ( (>= *age* 18) ; If T do this
		(setf *college-ready* 'yes)
		(format t "Ready for College ~%"))
	  ( (< *age* 18) ; Else If T do this
	  	(setf *college-ready* 'no)
		(format t "Not Ready for College ~%"))
	  (t (format t "Don't Know ~%"))) ; Else do this by default (t is for true)

;;; ---------- LOOPING ----------

;;; loop executes code a defined number of times
;;; Create a list using numbers 1 through 10
(loop for x from 1 to 10
	do(print x))

;;; Loop until the when condition calls return
(setq x 1) ; sc note using setq will throw a warning if x isn't already defined - if x isn't already defined,
		   ; use (defparamter x 3) or whatever
(loop
	(format t "~d ~%" x)
	(setq x (+ x 1))
	(when (> x 10) (return x))
)

;;; loop for can cycle through a list or iterate commonly
;;; It will execute any number of statements after do
(loop for x in '(Peter Paul Mary) do
	(format t "~s ~%" x)
)

(loop for y from 100 to 110 do
	(print y)
)

;;; dotimes iterates a specified number of times
(dotimes (y 12)
	(print y))

;;; ---------- CONS CELLS / LISTS ----------

;;; Link together 2 objects of data
(cons 'superman 'batman)

;;; Create a list with list
(list 'superman 'batman 'flash)

;;; Add item to the front of another list
(cons 'aquaman '(superman batman))

;;; Get the first item out of a list with car
(format t "First = ~a ~%" (car '(superman batman aquaman)))

;;; Get everything but the first item with cdr
(format t "Everything Else = ~a ~%" (cdr '(superman batman aquaman)))

;;; Get the 2nd item d = (batman flash joker) a = (batman)
(format t "2nd Item = ~a ~%" (cadr '(superman batman aquaman flash joker)))

;;; Get the 3rd item = aquaman
(format t "3rd Item = ~a ~%" (caddr '(superman batman aquaman flash joker)))

;;; Get the 4th item (Max you can go)
(format t "4th Item = ~a ~%" (cadddr '(superman batman aquaman flash joker)))

;;; Get the 4th item = joker
(format t "4th Item = ~a ~%" (cddddr '(superman batman aquaman flash joker)))

;;; Get the 2nd item in the second list
;;; d : (aquaman flash joker) (wonderwoman catwoman)
;;; a : (aquaman flash joker)
;;; d : (flash joker)
;;; a : (flash)
(format t "2nd Item 2nd List = ~a ~%"
(cadadr '((superman batman) (aquaman flash joker) (wonderwoman catwoman))))

;;; Get the 3rd item in the 2nd list = joker
(format t "3rd Item 2nd List = ~a ~%"
(cddadr '((superman batman) (aquaman flash joker) (wonderwoman catwoman))))

;;; = T Is something a list
(format t "Is it a List = ~a ~%" (listp '(batman superman)))

;;; Is 3 a member of the list
(format t "Is 3 in the List = ~a ~%" (if (member 3 '(2 4 6)) 't nil))

;;; Combine lists into 1 list
(append '(just) '(some) '(random words))

;;; Push an item on the front of a list
(defparameter *nums* '(2 4 6))
(push 1 *nums*)

;;; Get the nth value from a list
(format t "2nd Item in the List = ~a ~%" (nth 2 *nums*))

;;; Create a plist which uses a symbol to describe the data
(defvar superman (list :name "Superman" :secret-id "Clark Kent"))

;;; This list will hold heroes
(defvar *hero-list* nil)

;;; Adds items to our list
(push superman *hero-list*)

;;; Cycle through all heros in the list and print them out
(dolist (hero *hero-list*)

	;; Surround with ~{ and ~} to automatically grab data from list
	(format t "~{~a : ~a ~}~%" hero)
)

;;; ---------- ASSOCIATION LIST ----------

;;; The hero name represents the key

(defparameter *heroes*
	'((Superman (Clark Kent))
	(Flash (Barry Allen))
	(Batman (Bruce Wayne))))

;;; sc note:
(print *heroes*)
;;; ((SUPERMAN (CLARK KENT)) (FLASH (BARRY ALLEN)) (BATMAN (BRUCE WAYNE)))
;;; sc: looks like a hash or dictionaries. See: https://docs.python.org/3/tutorial/datastructures.html#dictionaries

;;; Get the key value with assoc
(format t "Superman Data ~a ~%" (assoc 'superman *heroes*))

;;; Get secret identity
(format t "Superman is ~a ~%" (cadr (assoc 'superman *heroes*)))

(defparameter *hero-size*
	'((Superman (6 ft 3 in) (230 lbs))
	(Flash (6 ft 0 in) (190 lbs))
	(Batman (6 ft 2 in) (210 lbs))))

;;; Get height
(format t "Superman is ~a ~%" (cadr (assoc 'Flash *hero-size*)))

;;; Get weight
(format t "Batman is ~a ~%" (caddr (assoc 'Batman *hero-size*)))

;;; sc: i didn't grok cdr cadr caddr, so here's some explanation:

(setq mylist '(a b c d e))
(car mylist)
(cdr mylist)

;;; To access deeper into the list with a single function call, Lisp provides "composite" functions
;;; like cadr and caddr etc. These are shorthand for multiple applications of car and cdr. Each letter after the c
;;; and before the r represent an operation: a for car, and d for cdr.

;;; then why doesn't this work?
(caar mylist)
;;; read the error: "because object A is not of type list"

(car mylist) ; first element

(cadr mylist) ; equivalent to (car (cdr x))
(car (cdr mylist))

(caddr mylist) ; equivalent to (car (cdr (crd x)))






;;; ---------- FUNCTIONS ----------

;;; Create a function that says hello
(defun hello ()
	(print "Hello")
	(terpri)) ; Newline

(hello)

;;; Get average
(defun get-avg (num-1 num-2)
	(/ (+ num-1 num-2) 2 ))

(format t "Avg 10 & 50 = ~a ~%" (get-avg 10 50))

;;; You can define some parameters as optional in a function with &optional
(defun print-list (w x &optional y z)
	(format t "List = ~a ~%" (list w x y z))
)

(print-list 1 2 3)
;;; List = (1 2 3 NIL)
(print-list 1 3)
;;; List = (1 3 NIL NIL)

;;; Receive multiple values with &rest
(defvar *total* 0)

(defun sum (&rest nums)
	(dolist (num nums)
		(setf *total* (+ *total* num))
	)
	(format t "Sum: ~a ~%" *total*)
)

(sum 1 2 3 4 5)
(sum 10 20 30)
;;; note this is a bit dangerous using a global variable
;;; since it *keeps* summing the total cumulatively the more the function is run!


(defun sum2 (&rest nums)
  (defparameter local_total 0)
  (dolist (num nums)
	(setf local_total (+ local_total num))
	)
  (format t "Sum: ~a ~%" local_total)
  )

(sum2 1 2 3 4 5)
;;; sc: in this ^^ case, the variable the total is stored
;;; in is defined within the function, hence running the function
;;; multiple times doesn't cause it to accumulate


;;; Keyword parameters are used to pass values to specific variables
;;; sc: incidentally, this is a good example of applying multiple attributes/keywords to an argument in a function
(defun print-list (&optional &key x y z)
	(format t "List: ~a ~%" (list x y z))
)

(print-list :x 1 :y 2)


;;; sc defining my own function with keyword arguments


(defun mypets (&key pet name)
  (format t "My pet ~a is called ~a~%" pet name))

(mypets :pet "dog" :name "maxy")


;;; Functions by default return the value of the last expression
;;; You can also return a specific value with return-from followed by the
;;; function name
(defun difference (num1 num2)
	(return-from difference(- num1 num2))
)

(format t "10 - 2 = ~a ~%" (difference 10 2))

;;; Get Supermans data
;;; When you use ` you are using quasiquoting which allows you to switch from
;;; code to data mode
;;; The function between ,() is code mode

(defun get-hero-data (size)
	(format t "~a ~%"
	`(,(caar size) is ,(cadar size) and ,(cddar size))))

(defparameter *hero-size*
	'((Superman (6 ft 3 in) (230 lbs)) ; sc reminder ' is shorthand for creating a list
	(Flash (6 ft 0 in) (190 lbs))
	(Batman (6 ft 2 in) (210 lbs))))

(get-hero-data *hero-size*)
;;; (SUPERMAN IS (6 FT 3 IN) AND ((230 LBS)))


;;; sc: for my own understanding
(format t "A ~a ~%" "thing") ; result: A thing
(format t "A ~a ~%" ("thing")) ;  result: errors
(format t "A ~a ~%" `("thing")) ; result A (thing) ; note: not sure if `("thing") is being treated as a list or just looks like one when printed to terminal
(format t "A ~a ~%" '("thing")) ; result A (thing)

(format t "A ~a another ~a ~%" "thing" "second") ; result A thing another second
(format t "A ~a another ~a ~%" ("thing") ("second")) ; result: error. (guess: format might treat remaining args as a list?)
(format t "A ~a another ~a ~%" (list "thing") (list "second")) ; result: A (thing) another (second)
(format t "A ~a another ~a ~%" '("thing") '("second")) ; same as above, just different notation

;;; make the same list as above but using list instead of quote '
(list
  (list 'Superman)
  (list 6 'ft 3 'in)
  (list 230 'lbs))

;;; note that after seeing it like this, with a couple of the inner lists containing
;;; both integers and symbols (e.g. 'ft 'in 'lbs) that are kinda interpreted as strings just seems odd

;;; what if we did use strings instead?


(print(list
  (list "Superman")
  (list 6 "ft" 3 "in")
  (list 230 "lbs")))
;;; (("Superman") (6 "ft" 3 "in") (230 "lbs")) ;; this feels better but doesn't look as good

;;; sc my own definition of a more complex list
(defvar thingy '(vehicle (brand toyota) (make camry)))
(defvar thingy2 (list 'vehicle (list 'brand 'toyota) (list 'make 'camry)))
(print thingy)
(print thingy2)

(equal thingy thingy2) ; reminder: (equal) compares the values within the object
;; True




;;; Check if every item in a list is a number
(format t "A number ~a ~%" (mapcar #'numberp '(1 2 3 f g)))

;;; You can define functions local only to the flet body
;;; (flet ((func-name (arguments)
;;;	... Function Body ...))
;;;	... Body ...)


;;; sc: 'flet' presumably let (define a local variable) but for a function, hence 'f'let
(flet ((double-it (num)
		(* num 2)))
		(double-it 10))

;;; You can have multiple functions in flet
(flet ((double-it (num)
		(* num 2))
		(triple-it (num)
		(* num 3)))
		(format t "Double & Triple 10 = ~d~%" (triple-it (double-it 10)))
)

;;; labels is used when you want to have a function call itself, or if you want
;;; to be able to call another local function inside a function
(labels ((double-it (num)
			(* num 2))
		(triple-it (num)
			(* (double-it num) 3)))
		(format t "Double & Triple 3 = ~d~%" (triple-it 3))
)

;;; Return multiple values from a function
(defun squares (num)
	(values (expt num 2) (expt num 3)))

(squares 2)
;; => 4, 8

;;; Get multiple values from a function
(multiple-value-bind (a b) (squares 2)
	(format t "2^2 = ~d  2^3 = ~d~%" a b)
)

;;; Higher Order Functions
;;; You can use functions as data

(defun times-3 (x) (* 3 x))
(defun times-4 (x) (* 4 x))

;;; Pass in the function without attributes just like a variable
(defun multiples (mult-func max-num)

	;; Cycle through values up to the max supplied
	(dotimes (x max-num)

		;; funcall is used when you know the number of arguments
		(format t "~d : ~d~%" x (funcall mult-func x)) ;; wow - this is pretty bad ass (although I'm not sure what I'd use it for)
))

(multiples #'times-3 10)
(multiples #'times-4 10)

;;; ---------- LAMBDA ----------

;;; The lambda command allows you to create a function without giving it a name
;;; You can also pass this function just like you pass variables

;;; Multiply every item in a list
(mapcar (lambda (x) (* x 2)) '(1 2 3 4 5))


;;; ---------- MACROS ----------
;;; A function runs when it is called to execute, while a macro is compiled
;;; first and is available immediately like any other lisp built in function
;;; Macros are functions used to generate code rather then perform actions

(defvar *num* 2)
(defvar *num-2* 0)

;;; It can be irritating to have to use progn with if
(if (= *num* 2)
	(progn
		(setf *num-2* 2)
		(format t "*num-2* = ~d ~%" *num-2*)
	)
	(format t "Not equal to 2 ~%"))

(defmacro ifit (condition &rest body)

	;;; The backquote generates the code
	;;; The , changes the condition to code mode from data mode
	;;; The &rest body parameter will hold commands in a list
	;;; The "Can't Drive" Works as the else

	`(if ,condition (progn ,@body) (format t "Can't Drive ~%") ))

(setf *age* 16)

(ifit (>= *age* 16)
	(print "You are over 16")
	(print "Time to Drive")
	(terpri)
)

;; sc challenge. The macro above doesn't generalise that well
;; (since check out the if not option - it will always say
;; "Can't drive") - what if we could generalise it by parametising
;; that message?

(defmacro ifit (condition &rest body)
	`(if ,condition (progn ,@body) (format t "Can't Drive ~%") ))

(defmacro ifit (condition not_message &rest body)
  `(if ,condition (progn ,@body) (format t not_message)))

(defparameter message "underage")

(ifit (< *age* 16) message
	(print "You are over 16")
	(print "Time to Drive")
	(terpri)
)
;; sc DID NOT successfully get this ^^ working
;; can try again when I know more



;;; let can also get confusing with its parentheses

(defun add (num1 num2)
	(let ((sum (+ num1 num2))) ; sc: I'm not even sure why let two sets of ((  )) are required inside let here.
		(format t "~a + ~a = ~a ~%" num1 num2 sum)))

(add 2 3)

;;; Define a macro to clean up let

(defmacro letx (var val &body body)
	`(let ((,var ,val)) ,@body))

(defun subtract (num1 num2)
	(letx dif (- num1 num2)
		(format t "~a - ~a = ~a ~%" num1 num2 dif)))

(subtract 10 6)

;;; ---------- CLASSES ----------
;;; defclass defines your custom data type
;;; Define the class name and attributes it has

(defclass animal ()
	(name
	sound))

;;; Create an animal object
(defparameter *dog* (make-instance 'animal))

;;; Set the values for dog
(setf (slot-value *dog* 'name) "Spot")
(setf (slot-value *dog* 'sound) "Woof")

;;; Get the values for dog
(format t "~a says ~a ~%"
	(slot-value *dog* 'name)
	(slot-value *dog* 'sound))

;;; You can define initialization options for objects
;;; :initarg defines the key used to assign to the slot
;;; :initform defines a default value
;;; You can define an error message if an attribute isn't provided
;;; :accessor generates getter and setters for the slot using the name you
;;; provide.
;;; You could use :reader mammal-sound to generate only a getter
;;; You could use :writer (setf mammal-sound) to generate only a setter
(defclass mammal ()
	((name
		:initarg :name
		:initform (error "Must provide a name"))
	(sound
		:initarg :sound
		:initform "No Sound"
		:accessor mammal-sound)
	)
)

(defparameter *king-kong*
	(make-instance 'mammal :name "King Kong" :sound "Rawwwr")
)

;;; Output data on the mammal
(format t "~a says ~a ~%"
	(slot-value *king-kong* 'name)
	(slot-value *king-kong* 'sound))

;;; Displays an error because name wasn't defined
;;; (defparameter *king-kong* (make-instance 'mammal))

;;; Create mammal Fluffy
(defparameter *fluffy*
	(make-instance 'mammal :name "Fluffy" :sound "Meow")
)

;;; A generic function has a name and parameter list but no implementation
;;; In Lisp methods don't belong to classes, but instead belong to generic
;;; functions which are responsible for executing the correct method based
;;; on the data passed

(defgeneric make-sound (mammal))

(defmethod make-sound ((the-mammal mammal))
	(format t "~a says ~a ~%"
	(slot-value the-mammal 'name)
	(slot-value the-mammal 'sound))
)

(make-sound *king-kong*)
(make-sound *fluffy*)

;;; You can define your own getters and setters

;;; Define setter
(defgeneric (setf mammal-name) (value the-mammal))

(defmethod (setf mammal-name) (value (the-mammal mammal))
	(setf (slot-value the-mammal 'name) value))

;;; Define getter
(defgeneric mammal-name (the-mammal))

(defmethod mammal-name ((the-mammal mammal))
	(slot-value the-mammal 'name))

(setf (mammal-name *king-kong*) "Kong")

(format t "I am ~a ~%" (mammal-name *king-kong*))

;;; Use the auto generated sound getters and setters instead
(setf (mammal-sound *king-kong*) "Rawwwwwwwwr")
(format t "I say ~a ~%" (mammal-sound *king-kong*))

;;; Inheritance allows you to inherit all the attributes of the superclass
;;; and call methods that accept the superclass

(defclass dog (mammal)
	())

(defparameter *rover*
	(make-instance 'dog :name "Rover" :sound "Woof")
)

(make-sound *rover*)

;;; ---------- ARRAYS ----------

;;; Create an array with 3 storage areas
(defparameter names (make-array 3))

;;; Add a value to an array
(setf (aref names 1) 'Bob)

;;; Get a value in an index
(aref names 1)

;;; Make a 3 by 3 array
(setf num-array (make-array '(3 3)
	:initial-contents '((0 1 2) (3 4 5) (6 7 8))))

;;; Cycle through and print the array
(dotimes (x 3)
	(dotimes (y 3)
		(print (aref num-array x y))
	)
)

;;; ---------- HASH TABLE ----------
;;; A collection of key value pairs

;;; Create a hash table
(defparameter people (make-hash-table))

;;; Set the key as 102 and the value to Paul Smith
(setf (gethash '102 people) '(Paul Smith))
(setf (gethash '103 people) '(Sam Smith))

;;; Get the value stored in the key 102
(gethash '102 people)

;;; maphash executes a function on each item
;;; ~% = newline
(maphash #'(lambda (k v) (format t "~a = ~a~%" k v)) people)

;;; Remove an entry with the key
(remhash '103 people)

;;; ---------- STRUCTURES ----------
;;; A user defined data type with multiple different data types

;;; Define the data names in the struct
(defstruct customer name address id)

;;; Store data in the struct
(setq paulsmith (make-customer
	:name "Paul Smith"
	:address "123 Main St"
	:id 1000
))

;;; Get a value stored
(customer-name paulsmith)

;;; Change a value in the struct
(setf (customer-address paulsmith) "125 Main St")
(write paulsmith)

(setq sally-smith-1001 (make-customer
	:name "Sally Smith"
	:address "123 Main St"
	:id 1001
))

;;; ---------- FILE I O ----------

;;; Write text to a file
;;; A keyword symbol starts with a colon and it only means itself
(with-open-file (my-stream
				"test.txt"
				:direction :output ; We are writing to the file
				:if-exists :supersede) ; If the file exists delete it
	(princ "Some random Text" my-stream))

;;; Read data from a file
(let ((in (open "test.txt" :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      while line do (format t "~a~%" line))
      (close in)
   )
)
