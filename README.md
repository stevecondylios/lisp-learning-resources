# A small collection of lisp notes



### Random resources

- Excellent answer [here](https://www.quora.com/What-is-symbolic-programming/answer/Vladislav-Zorov) on what a 'symbolic programming' language is, and, inadvertantly, a great example of lisp macros. 
- Lisp is [lex's](https://www.youtube.com/watch?v=cMMiaCtOzV0) favourite language
- This video recommends starting with [scheme](https://www.youtube.com/watch?v=GWdf1flcLoM&t=4m50s) because it's so small it can be learned in ~ 1 hour. 
  - It also says some learn scheme by simply reading the [specification](https://standards.scheme.org/).
    - According to [here](https://stackoverflow.com/questions/6523396/scheme-core-language-specification/6523520#comment7680042_6523520) scheme 5 is famously small (50 pages), scheme 6 is a much larger language. (I presume scheme 7 is probably larger still). 



### Installing and doing the basics in common lisp


- See Derek Banas tutorial [here](https://www.youtube.com/watch?v=ymSq4wHrqyU&t=1m). 
  - Tried to install clisp via [macports](https://www.macports.org/install.php) and following these instructions to [install common lisp](https://ports.macports.org/port/clisp/) (basically just `sudo port install clisp`), BUT it wouldn't work, possibly because of apple silicon, so I used homebrew and it worked a treat `HOMEBREW_NO_AUTO_UPDATE=1 brew install clisp` (the first bit is simply to prevent brew updating everything, see [here](https://apple.stackexchange.com/a/293252/290197)). 

- Run common lisp in the terminal with `clisp`. 
  - Some things to show it works `(+ 2 2)` (addition), `(* 9 7)` (multiplication). 
  - Exit interpreter with `(quit)`


- Run a lisp propgram
  - Create mylisp.lisp with this in it `(print "hello")`, then run it with `clisp mylisp.lisp`








