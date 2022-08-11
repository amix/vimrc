" Copyright 2013 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

if exists('s:loaded') || !(exists('g:rainbow_active') || exists('g:rainbow_conf')) | finish | endif | let s:loaded = 1

command! RainbowToggle call rainbow_main#toggle()
command! RainbowToggleOn call rainbow_main#load()
command! RainbowToggleOff call rainbow_main#clear()

if (exists('g:rainbow_active') && g:rainbow_active)
	auto syntax * call rainbow_main#load()
	auto colorscheme * call rainbow_main#load()
endif
