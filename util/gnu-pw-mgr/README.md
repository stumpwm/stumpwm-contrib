# gnu-pw-mgr

This provides an interface to `gnu-pw-mgr`, GNU's password manager.
Specifically, this provides a command, `password-to-selection`, that
prompts you to enter a password ID and then offers a menu of seed
IDs.  Upon selecting a seed ID, the (re-)generated password is copied
to the X selection (regardless of whether or not the password ID that
you entered is linked to an actual password that you use).

No further configuration is necessary within Stumpwm.  You must use
the `gnu-pw-mgr` command-line tool to perform any password
manipulations; this package is only for retrieving passwords.

## Author

_Brandon Invergo <brandon@invergo.net>_

## License

GPL version 3 or, at your option, any later version.

