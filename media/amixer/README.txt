 USAGE:

 Make sure you have your media keys (or whatever) mapped to the appropriate
 keysyms (using xmodmap), then put:

     (load "/path/to/amixer.lisp")

 ...in your ~/.stumpwmrc, followed by some keybindings (according
 to your preference)

 TODO:

 Make the `defvolcontrol' macro create all the necessary commands at once.

   - Should it just create, say, amixer-pcm, which would be passed an
     argument? i.e., (define-key *this-map* (kbd "e") "amixer-pcm 1-")

   - Else, figure out how to make the macro not error when converting a
     string to a symbol for the name of the command

