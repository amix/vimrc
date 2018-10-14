# comfortable-motion.vim

Brings physics-based smooth scrolling to the Vim/Neovim world!

This is highly motivated by the lack of a plugin similar to my favorite Emacs package [emacs-inertial-scroll](https://github.com/kiwanami/emacs-inertial-scroll).

Scroll with `C-d`/`C-u`:
![Scroll with `C-d`/`C-u`](https://cloud.githubusercontent.com/assets/158553/21579969/841ab468-d013-11e6-8ce6-aa5442d52b6b.gif)

Scroll with `C-f`/`C-b`:
![Scroll with `C-f`/`C-b`](https://cloud.githubusercontent.com/assets/158553/21579968/841929ea-d013-11e6-82fb-e0f3d3e8e896.gif)


## Requirements
This plugin depends on the timer API, which requires Vim/Neovim to be at least the following version:

- Vim 7.4.1578 or above
- Neovim 0.1.5 or above

However, currently, this plugin is only tested on Vim 8.0 and Neovim 0.1.7, i.e. my current development environment.


## Installation

For example, with [vim-plug](https://github.com/junegunn/vim-plug):
```vim
Plug 'yuttie/comfortable-motion.vim'
```


## Configuration

Please note that the following mappings for `<C-d>` and `<C-u>` are not ones for you if you expect they scroll a buffer just half a window.

### Scrolling Method

This plugin relies on `<C-e>` and `<C-y>` by default to actually scroll a window.
You can customize these keys to other combinations like `j` and `k` as follows:

```vim
let g:comfortable_motion_scroll_down_key = "j"
let g:comfortable_motion_scroll_up_key = "k"
```

This results in:

![Different scrolling method](https://cloud.githubusercontent.com/assets/158553/24331179/ebc5b106-1269-11e7-90c2-747a68dec44b.gif)

Please note that you cannot choose complex keys consisting of multiple motions, e.g. `$j`.
This is because the current implementation prepends the number of scroll amount to the keys, e.g. `5$j`, and executes it once per simulation tick.


### Keys and Mouse Wheel

By default, the following key mappings are defined.

```vim
nnoremap <silent> <C-d> :call comfortable_motion#flick(100)<CR>
nnoremap <silent> <C-u> :call comfortable_motion#flick(-100)<CR>

nnoremap <silent> <C-f> :call comfortable_motion#flick(200)<CR>
nnoremap <silent> <C-b> :call comfortable_motion#flick(-200)<CR>
```

To prevent the plugin from defining those default key mappings,
you can set `g:comfortable_motion_no_default_key_mappings` to 1.

```vim
let g:comfortable_motion_no_default_key_mappings = 1
```


Additionally, if your Vim/NeoVim has mouse support, you can get mouse wheel to scroll a window by the following mappings:

```vim
noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>
```

You may need to enable the `mouse` option for the above to work, for example, by `set mouse=a`.


### Simulation Parameters

There are three configurable parameters:

* `g:comfortable_motion_interval` [default: 1000.0 / 60]
* `g:comfortable_motion_friction` [default: 80.0]
* `g:comfortable_motion_air_drag` [default: 2.0]

For example, with any of the following configurations, you can get `<C-u>`/`<C-d>` (with the
default impulse value of `-100`/`100`) to scroll a window about 25 lines, but
tastes are different.


### Friction & Air Resistance

```vim
let g:comfortable_motion_friction = 80.0
let g:comfortable_motion_air_drag = 2.0
```


### Friction Only

```vim
let g:comfortable_motion_friction = 200.0
let g:comfortable_motion_air_drag = 0.0
```


### Air Resistance Only

```vim
let g:comfortable_motion_friction = 0.0
let g:comfortable_motion_air_drag = 4.0
```


## Advanced Configurations

If you would like to use scrolling proportional to the window height,
you may use settings such as these:
```vim
let g:comfortable_motion_no_default_key_mappings = 1
let g:comfortable_motion_impulse_multiplier = 1  " Feel free to increase/decrease this value.
nnoremap <silent> <C-d> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 2)<CR>
nnoremap <silent> <C-u> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -2)<CR>
nnoremap <silent> <C-f> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 4)<CR>
nnoremap <silent> <C-b> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -4)<CR>
```


## License

MIT License
