# Grabbable Modifier Keys

This module implements grabbable modifier keys, allowing the user to bind
actions to the press and/or release of a modifier key.

## Usage

After loading the module, use `SWM/GMK:SET-MODIFIER-ACTIONS` to set actions for
a modifier key (`:MOD1`, `:LOCK`, `:MOD4`, etc.). Setting an action
automatically registers the modifier key to be grabbed. As an example, to bind
mod4 (super on the authors machine) to toggle the mode line on press/release, we
would place the following in our `.stumpwmrc`

```
(flet ((toggler (c s)
         (declare (ignore c s))
         (let* ((s (stumpwm:current-screen))
                (g (stumpwm:current-group s))
                (h (stumpwm:current-head g)))
           (stumpwm:toggle-mode-line s h))))
  (swm/gmk:set-modifier-actions :mod4 #'toggler #'toggler))
```

## Quirks

Modifier key presses do not follow the normal key binding and command invocation
of StumpWM. Rather they expose the key event parameters `CODE` and `STATE` to
the user, and expect the user to do whatever legwork is neccessary based on
those parameters.

This minor mode *will* clobber any custom key event handler the user has
installed. A way around this is to modify the `:KEY-PRESS` event handler and
change the `UNLESS` form into an `IF`, which calls
`SWM/GMK:MODIFIER-KEY-PRESS-HANDLER` when a modifier code sneaks through.

Additionally, the key press will not have the modifier set in its state mask,
but on key release the modifier will be set in the state mask. E.g. from the
above example if only super is pressed with no other modifiers then the state
will be zero on press and 64 on release (on the authors machine).

Finally, if a key is defined in the top map that uses the modifier, then a press
to that key binding will prevent the firing of the release action. E.g. from our
above example, if `super-p` is bound then pressing super will toggle the mode
line, pressing p will fire whatever command it is bound to, and releasing super
will ***not*** fire the release action.
