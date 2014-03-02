To load this module, place

    (load-module "battery-portable")

in your .stumpwmrc. Battery information is then available via %B
in your mode-line config.

If you have an older kernel and the above doesn't work, add

    (setf stumpwm.contrib.battery-portable:*prefer-sysfs* nil)

below the above line.

