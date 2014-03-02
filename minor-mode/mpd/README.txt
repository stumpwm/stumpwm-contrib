USAGE:

Put:

    (load-module "mpd")

...into your ~/.stumpwmrc

Then you can use "%m" in your mode line format, as well as various commands
defined at the end of the file.

You can customize the modeline format (*mpd-modeline-fmt*), the status
message displayed by the command mpd-status (*mpd-status-fmt*; note that
this is also used on the modeline), and the status message displayed by the
command mpd-current-song (*mpd-current-song-fmt*). See the documentation for
*mpd-modeline-fmt* for more information.

NOTES:

See http://mpd.wikia.com/wiki/Protocol_Reference for full protocol

TODO:

- Implement optional shortening for formatting functions
- Implement notification window on song change etc...


