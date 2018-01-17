# command-history

***** Problem
After quiting StumpWM, you command history is gone.
This simple plugin saves the *stumpwm::*input-history* to ~/.stumpwm.d/history and loads it again when starting StumpWM.

** Usage
Add this to your =.stumpwmrc=:

Load contrib module:
#+BEGIN_SRC lisp
  (load-module "command-history")
#+END_SRC

Moreover there is a couple of util functions exported for the aim of
user-defined extensions - see source code.

