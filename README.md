# A small collection of lisp notes

Note: lisp code examples in this file and in [lisp.lisp](lisp.lisp) (since I can't get emacs/SLY working with .md files yet - can always use nvim+vim-slime, but I want to use emacs as much as possible while learning lisp for the full repl experience)



### Random resources

- Paul Graham's *ANSI Common Lisp** [pdf](https://7chan.org/pr/src/ANSI_Common_Lisp_-_Paul_Graham.pdf), [first chapter](https://sep.turbifycdn.com/ty/cdn/paulgraham/acl1.txt?t=1688221954&), [second chapter](https://sep.turbifycdn.com/ty/cdn/paulgraham/acl2.txt)

- Excellent answer [here](https://www.quora.com/What-is-symbolic-programming/answer/Vladislav-Zorov) on what a 'symbolic programming' language is, and, inadvertantly, a great example of lisp macros. 
- Lisp is [lex's](https://www.youtube.com/watch?v=cMMiaCtOzV0) favourite language
- This video recommends starting with [scheme](https://www.youtube.com/watch?v=GWdf1flcLoM&t=4m50s) because it's so small it can be learned in ~ 1 hour. 
  - It also says some learn scheme by simply reading the [specification](https://standards.scheme.org/).
    - According to [here](https://stackoverflow.com/questions/6523396/scheme-core-language-specification/6523520#comment7680042_6523520) scheme 5 is famously small (50 pages), scheme 6 is a much larger language. (I presume scheme 7 is probably larger still). 
- 5 minute tutorial on how to run webserver (using hunchentoot), how to render html and pass a URL parameter ([here](https://www.youtube.com/watch?v=A4PzSsOD-CQ). 


- To **Setup** 'clisp' (note many sources seem to recommend sbcl over clisp, so I use `sbcl --script file.lisp` now) Install clisp, start the interpreter with `clisp`, or run a `.lisp` file with `clisp myfile.lisp`
  - Two great youtube videos on how to install/setup (steel bank) common lisp + emacs + emacs-plus + doom emacs (they can be found toward top of search results)
- Fireship video [here](https://www.youtube.com/watch?v=INUHCQST7CU)
- Brilliant, long single-pager here: https://news.ycombinator.com/item?id=40435771

- Racket online book: https://docs.racket-lang.org/guide/index.html


### Great tutorials and projects

- Drum n Bass in Common Lisp: [video](https://www.youtube.com/watch?v=jS84KmkkNkU) and [repo](https://github.com/byulparan/livecoding) 

### Installing and doing the basics in common lisp


- See Derek Banas tutorial [here](https://www.youtube.com/watch?v=ymSq4wHrqyU&t=1m). 
  - Tried to install clisp via [macports](https://www.macports.org/install.php) and following these instructions to [install common lisp](https://ports.macports.org/port/clisp/) (basically just `sudo port install clisp`), BUT it wouldn't work, possibly because of apple silicon, so I used homebrew and it worked a treat `HOMEBREW_NO_AUTO_UPDATE=1 brew install clisp` (the first bit is simply to prevent brew updating everything, see [here](https://apple.stackexchange.com/a/293252/290197)). 

- Run common lisp in the terminal with `clisp`. 
  - Some things to show it works `(+ 2 2)` (addition), `(* 9 7)` (multiplication). 
  - Exit interpreter with `(quit)`


- Run a lisp propgram
  - Create mylisp.lisp with this in it `(print "hello")`, then run it with `clisp mylisp.lisp`







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

- Amusing, but not untrue [statement](https://redirect.cs.umbc.edu/courses/331/resources/papers/Evolution-of-Lisp.pdf) from Guy Steele: 

> Overall, the evolution of Lisp has been guided more by institutional rivalry, one-upsmanship, and the glee born of technical cleverness that is characteristic of the \hacker culture" than by sober assessments of technical requirements. Nevertheless this process has eventually produced both an industrialstrength programming language, messy but powerful, and a technically pure dialect, small but powerful, that is suitable for use by programming-language theoreticians.


- Alan Kay is said to have said ([source](https://www.youtube.com/watch?v=OyfBQmvr2Hc&t=7m30s)) 

*When he first saw the lisp interpreter written in lisp at the bottom of page 13 of the lisp 1.5 manual it changed his life and he realised these are Maxwell's<sup>^</sup> equations of software.*

([page 13 of Lisp 1.5 manual](https://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf?page=21))


<sup>^</sup> [Feynman](https://www.youtube.com/watch?v=P1ww1IXRfTA&t=31m45s) on the Maxwell equations: "the full equations for everything were worked out by Maxell in 1873.. the most remarkable thing in history".


RESOURCES:


RANDOM NOTES:

-  This video says ([here](https://www.youtube.com/watch?v=xyXDE5gP2QI&t=19m)) that he uses `cl-project` for setting up new common lisp projects.  




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










