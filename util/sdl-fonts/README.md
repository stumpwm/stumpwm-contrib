A TTF renderer for StumpWM which uses SDL. SDL is actively developed by the
whole world so the TTF code is hopefully good as well.

This module requires the native packages SDL2, SDL2_ttf and libffi.
Make sure they are installed.

Load a font through by

```
(defparameter *the-font* (sdl-fonts:load-font "/usr/share/fonts/TTF/DroidSansMono.ttf" 14))
```

And use it through

```
(set-font *the-font*)
```

Of course you can also use multiple fonts. Additionally, you can specify
the hinting and kerning by passing

```
:hinting <:normal|:light|:mono|:none|:light-subpixel>
:kerning <boolean>
```

to `sdl-fonts:load-font`.

Have fun!

Use at your own risk and discretion. I assume no responsibility for any damage
resulting from using this project. I do use it myself.

This modules borrows ideas and code from clx-truetype.
