# stumpwm-mixer

This is a StumpWM interface to FreeBSD's built-in `mixer` command so
that you can control the volume from StumpWM and get some visual
feedback when the volume changes. It's deliberately designed to have a
similar interface to the `stumpwm-sndioctl` module.

Add something like this to your config to bind its commands to the
media buttons:

```
(define-key *top-map* (kbd "XF86AudioMute") "toggle-mute")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
```
