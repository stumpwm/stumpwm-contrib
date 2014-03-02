WARNING:

This triggers a yet to be discovered bug in SBCL, which causes
stumpwm to freeze.

USAGE:

Place the following in your ~/.stumpwmrc file:

    (load-module "wifi")

Then you can use "%I" in your mode line format (both "w" and "W"
were taken. Think _I_EEE 802.11 :-)).

Notes: This gets information through sysfs, so it only works on
Linux with a mounted sysfs.

