# command-history

## Problem
After quiting StumpWM, you command history is gone.
This simple plugin saves the
#+BEGIN_SRC lisp
*stumpwm::*input-history* to ~/.stumpwm.d/history and loads it again when starting StumpWM.
#+END_SRC

## Usage
Add this to your =.stumpwmrc=:

Load contrib module:
#+BEGIN_SRC lisp
  (load-module "command-history")
#+END_SRC
