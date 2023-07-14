# Faster-Whichkey

A bunch of advice for making [which-key](https://github.com/justbur/emacs-which-key) a lot faster at startup, when using [general](https://github.com/noctuid/general.el).

It's been a while since I wrote this, but as far as I remember, which-key and general
spend quite a long time adding individual binding names to keymaps on startup,
and then when which-key is called to visualise a keymap, spend further time combining the names together.

This module caches binding names into a pseudo-keymap that is added to real keymaps.
It does nothing when emacs is following a keybinding to call,
but when which key wants to show binding names, it can use the cached values immediately.

Maybe I just have a lot of keybindings. 
Either way, wich this module makes a which-key summary popup immediately.
Without it, I spend about 2-3 minutes waiting. 
