
# Learning lisp from scratch

WHY?

From [here](https://news.ycombinator.com/threads?id=nomilk):

- PG makes the case for lisp here: https://paulgraham.com/icad.html
 
- Computerphile riffs on the importance of homoiconicity and the ability for a language to programatically alter itself: https://www.youtube.com/watch?v=dw-y3vNDRWk

- Uncle bob had tried many languages and settled on lisp: https://www.youtube.com/watch?v=UBXXw2JSloo&t=2m40s

> for 30 years I thought I don't want to learn lisp, it's a dumb language, and I read (Structure and Interpretation of Computer Programs) and it changed my mind... I thought I'm going to get (clojure) and dither around with it.. and I just fell in love with the language and I've been using it ever since 

- Douglas Lenat (who I hadn't heard of prior to Lex) [says](https://www.youtube.com/watch?v=cMMiaCtOzV0&t=2m):

> Development in lisp proceeds 1000-50000 times faster than modern programming languages


- John Carmack ([here](https://www.youtube.com/watch?v=RfWGJS7rckk&t=4m27s)) is cautious because lisp tends to be used as a language in which to write languages/dsl's, which detracts from the ability to bring new devs on to a lisp project:

> that's the downside of lisp, but people say how maluable it is, the idea of that for one of these long term projects kinda horrifies me.

RESOURCES:

- **Setup** Install clisp, start the interpreter with `clisp`, or run a `.lisp` file with `clisp myfile.lisp`
  - Two great youtube videos on how to install/setup (steel bank) common lisp + emacs + emacs-plus + doom emacs (they can be found toward top of search results)
- Fireship video [here](https://www.youtube.com/watch?v=INUHCQST7CU)
- Brilliant, long single-pager here: https://news.ycombinator.com/item?id=40435771


RANDOM NOTES:

-  The guy in the video says ([here](https://www.youtube.com/watch?v=xyXDE5gP2QI&t=19m)) that he uses `cl-project` for setting up new common lisp projects.  




### Noodling from various sources


From fireship video

(quick note: when using emacs from an .md file, the shortcuts will behave slightly differently. To send a line of lisp to the REPL via sly, you can still use C-M-x, or C-c C-c, but you must first switch to lisp-mode, by pressing M-x, the lisp-mode)
  - Also note: M-x (M: 'meta key' aka alt) is 'a gateway to all of emacs extensive functionality'



```lisp
(atom 2)
(list 1 2 3)


;; According to fireship the t is for true, but I think derek banas says it's for 'temrinal'
(format t "hi there")
(print "yo")

;; let will  lexically scope a variable



;; the 'message' variable will be available in this scope but not in the global context
(let ((message "something cool"))
  (print message)
)

;; 'message' variable won't be available in this context
(print message)


;; define global varaibles with defparameter or defvar
(defparameter *global_thing* "abcdef")

;; use it (remember lisp is case insensitive

*global_thing*
# "abcdef"
*GLOBAL_THING*
# "abcdef"


```


Some more basics

```lisp
;; arithmatic

(+ 3 7)
(- 10 4)
(* 6 7)





```



Note that if you ever make a typo you might see a different console prompt representing the debugger (e.g. `0]`) in which case you can type `:abort` to exit it. 













### How to install a package ('system') from Quicklisp


This took all of 2 seconds to run and install

```lisp
(ql:quickload "alexandria")
;; To load "alexandria":
;;   Load 1 ASDF system:
;;     asdf
;;   Install 1 Quicklisp release:
;;     alexandria
;; ; Fetching #<URL "http://beta.quicklisp.org/archive/alexandria/2023-10-21/alexandria-20231021-git.tgz">
;; ; 55.87KB
;; ==================================================
;; 57,207 bytes in 0.04 seconds (1553.00KB/sec)
;; ; Loading "alexandria"
;; [package alexandria]..............................
;; [package alexandria-2].
;; ("alexandria")
```

Incidentally, `:` is how you refer to a function from a package, e.g. 


```lisp
(hunchentoot:send-headers)
(ql:quickload "mypackage")
(ql:quickload "cl-project")
```

NOTE: AFAICT, `(ql:quickload)` loads a library, or installs then loads it depending on whether the library is already installed on the computer.





### Starting a project with cl-project package

Shown in the ~30 seconds from [here](https://www.youtube.com/watch?v=xyXDE5gP2QI&t=1205s)

Note that in ~/quicklisp is where it stores projects, including your own ~/quicklisp/local-projects


```lisp
(cl-project:make-project #p"~/quicklisp/local-projects/demo-project")
```


I think to reopen a project it's

```lisp
(ql:quickload "demo-project")
```






