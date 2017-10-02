#!/bin/bash

emacs --daemon # start emacs
setxkbmap -option ctrl:swap_lalt_lctl # swap left alt with right left control
emacsclient -c
