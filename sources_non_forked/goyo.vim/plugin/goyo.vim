" Copyright (c) 2013 Junegunn Choi
"
" MIT License
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

let s:cpo_save = &cpo
set cpo&vim

function! s:get_color(group, attr)
  return synIDattr(synIDtrans(hlID(a:group)), a:attr)
endfunction

function! s:set_color(group, attr, color)
  let gui = has('gui_running')
  execute printf("hi %s %s%s=%s", a:group, gui ? 'gui' : 'cterm', a:attr, a:color)
endfunction

function! s:blank()
  let main = bufwinnr(t:goyo_master)
  if main != -1
    execute main . 'wincmd w'
  else
    call s:goyo_off()
  endif
endfunction

function! s:init_pad(command)
  execute a:command

  setlocal buftype=nofile bufhidden=wipe nomodifiable nobuflisted noswapfile
      \ nonu nocursorline nocursorcolumn winfixwidth winfixheight statusline=\ 
  if exists('&rnu')
    setlocal nornu
  endif
  if exists('&colorcolumn')
    setlocal colorcolumn=
  endif
  let bufnr = winbufnr(0)

  execute winnr('#') . 'wincmd w'
  return bufnr
endfunction

function! s:setup_pad(bufnr, vert, size)
  let win = bufwinnr(a:bufnr)
  execute win . 'wincmd w'
  execute (a:vert ? 'vertical ' : '') . 'resize ' . max([0, a:size])
  augroup goyop
    autocmd WinEnter,CursorMoved <buffer> nested call s:blank()
    autocmd WinLeave <buffer> call s:hide_statusline()
  augroup END

  " To hide scrollbars of pad windows in GVim
  let diff = winheight(0) - line('$') - (has('gui_running') ? 2 : 0)
  if diff > 0
    setlocal modifiable
    call append(0, map(range(1, diff), '""'))
    normal! gg
    setlocal nomodifiable
  endif
  execute winnr('#') . 'wincmd w'
endfunction

function! s:hmargin()
  let nwidth = max([len(string(line('$'))) + 1, &numberwidth])
  let width  = t:goyo_width + (&number ? nwidth : 0)
  return (&columns - width)
endfunction

function! s:resize_pads()
  let hmargin = s:hmargin()
  let tmargin = get(g:, 'goyo_margin_top', 4)
  let bmargin = get(g:, 'goyo_margin_bottom', 4)

  augroup goyop
    autocmd!
  augroup END
  call s:setup_pad(t:goyo_pads.t, 0, tmargin - 1)
  call s:setup_pad(t:goyo_pads.b, 0, bmargin - 2)
  call s:setup_pad(t:goyo_pads.l, 1, hmargin / 2 - 1)
  call s:setup_pad(t:goyo_pads.r, 1, hmargin / 2 - 1)
endfunction

function! s:tranquilize()
  let bg = s:get_color('Normal', 'bg')
  for grp in ['NonText', 'FoldColumn', 'ColorColumn', 'VertSplit',
            \ 'StatusLine', 'StatusLineNC', 'SignColumn']
    " -1 on Vim / '' on GVim
    if bg == -1 || empty(bg)
      call s:set_color(grp, 'fg', get(g:, 'goyo_bg', 'black'))
      call s:set_color(grp, 'bg', 'NONE')
    else
      call s:set_color(grp, 'fg', bg)
      call s:set_color(grp, 'bg', bg)
    endif
    call s:set_color(grp, '', 'NONE')
  endfor
endfunction

function! s:hide_statusline()
  let &l:statusline = repeat(' ', winwidth(0))
endfunction

function! s:goyo_on(width)
  let s:orig_tab = tabpagenr()

  " New tab
  tab split

  let t:goyo_master = winbufnr(0)
  let t:goyo_width  = a:width
  let t:goyo_pads = {}
  let t:goyo_revert =
    \ { 'laststatus':     &laststatus,
    \   'showtabline':    &showtabline,
    \   'fillchars':      &fillchars,
    \   'winminwidth':    &winminwidth,
    \   'winwidth':       &winwidth,
    \   'winminheight':   &winminheight,
    \   'winheight':      &winheight,
    \   'ruler':          &ruler,
    \   'sidescroll':     &sidescroll,
    \   'sidescrolloff':  &sidescrolloff
    \ }
  if has('gui_running')
    let t:goyo_revert.guioptions = &guioptions
  endif

  " vim-gitgutter
  let t:goyo_disabled_gitgutter = get(g:, 'gitgutter_enabled', 0)
  if t:goyo_disabled_gitgutter
    silent! GitGutterDisable
  endif

  " vim-signify
  let t:goyo_disabled_signify = exists('b:sy') && b:sy.active
  if t:goyo_disabled_signify
    SignifyToggle
  endif

  " vim-airline
  let t:goyo_disabled_airline = exists("#airline")
  if t:goyo_disabled_airline
    AirlineToggle
  endif

  " vim-powerline
  let t:goyo_disabled_powerline = exists("#PowerlineMain")
  if t:goyo_disabled_powerline
    augroup PowerlineMain
      autocmd!
    augroup END
    augroup! PowerlineMain
  endif

  " lightline.vim
  let t:goyo_disabled_lightline = exists('#LightLine')
  if t:goyo_disabled_lightline
    silent! call lightline#disable()
  endif

  if !get(g:, 'goyo_linenr', 0)
    setlocal nonu
    if exists('&rnu')
      setlocal nornu
    endif
  endif
  if exists('&colorcolumn')
    setlocal colorcolumn=
  endif

  " Global options
  let &winheight = max([&winminheight, 1])
  set winminheight=1
  set winheight=1
  set winminwidth=1 winwidth=1
  set laststatus=0
  set showtabline=0
  set noruler
  set fillchars+=vert:\ 
  set fillchars+=stl:.
  set fillchars+=stlnc:\ 
  set sidescroll=1
  set sidescrolloff=0

  " Hide left-hand scrollbars
  if has('gui_running')
    set guioptions-=l
    set guioptions-=L
  endif

  let t:goyo_pads.l = s:init_pad('vertical topleft new')
  let t:goyo_pads.r = s:init_pad('vertical botright new')
  let t:goyo_pads.t = s:init_pad('topleft new')
  let t:goyo_pads.b = s:init_pad('botright new')

  call s:resize_pads()
  call s:tranquilize()

  augroup goyo
    autocmd!
    autocmd BufWinLeave <buffer> call s:goyo_off()
    autocmd TabLeave    *        call s:goyo_off()
    autocmd VimResized  *        call s:resize_pads()
    autocmd ColorScheme *        call s:tranquilize()
    autocmd WinEnter,WinLeave <buffer> call s:hide_statusline()
  augroup END

  call s:hide_statusline()
  if exists('g:goyo_callbacks[0]')
    call g:goyo_callbacks[0]()
  endif
  silent! doautocmd User GoyoEnter
endfunction

function! s:goyo_off()
  if !exists('#goyo')
    return
  endif

  " Oops, not this tab
  if !exists('t:goyo_revert')
    return
  endif

  " Clear auto commands
  augroup goyo
    autocmd!
  augroup END
  augroup! goyo
  augroup goyop
    autocmd!
  augroup END
  augroup! goyop

  let goyo_revert             = t:goyo_revert
  let goyo_disabled_gitgutter = t:goyo_disabled_gitgutter
  let goyo_disabled_signify   = t:goyo_disabled_signify
  let goyo_disabled_airline   = t:goyo_disabled_airline
  let goyo_disabled_powerline = t:goyo_disabled_powerline
  let goyo_disabled_lightline = t:goyo_disabled_lightline
  let goyo_orig_buffer        = t:goyo_master
  let [line, col]             = [line('.'), col('.')]

  if tabpagenr() == 1
    tabnew
    normal! gt
    bd
  endif
  tabclose
  execute 'normal! '.s:orig_tab.'gt'
  if winbufnr(0) == goyo_orig_buffer
    execute printf('normal! %dG%d|', line, col)
  endif

  let wmw = remove(goyo_revert, 'winminwidth')
  let ww  = remove(goyo_revert, 'winwidth')
  let &winwidth     = ww
  let &winminwidth  = wmw
  let wmh = remove(goyo_revert, 'winminheight')
  let wh  = remove(goyo_revert, 'winheight')
  let &winheight    = max([wmh, 1])
  let &winminheight = wmh
  let &winheight    = wh

  for [k, v] in items(goyo_revert)
    execute printf("let &%s = %s", k, string(v))
  endfor
  execute 'colo '. get(g:, 'colors_name', 'default')

  if goyo_disabled_gitgutter
    silent! GitGutterEnable
  endif

  if goyo_disabled_signify
    silent! if !b:sy.active
      SignifyToggle
    endif
  endif

  if goyo_disabled_airline && !exists("#airline")
    AirlineToggle
    silent! AirlineRefresh
  endif

  if goyo_disabled_powerline && !exists("#PowerlineMain")
    doautocmd PowerlineStartup VimEnter
    silent! PowerlineReloadColorscheme
  endif

  if goyo_disabled_lightline
    silent! call lightline#enable()
  endif

  if exists('#Powerline')
    doautocmd Powerline ColorScheme
  endif

  if exists('g:goyo_callbacks[1]')
    call g:goyo_callbacks[1]()
  endif
  silent! doautocmd User GoyoLeave
endfunction

function! s:goyo(bang, ...)
  let width = a:0 > 0 ? a:1 : get(g:, 'goyo_width', 80)

  if a:bang
    if exists('#goyo')
      call s:goyo_off()
    endif
  else
    if exists('#goyo') == 0
      call s:goyo_on(width)
    elseif a:0 > 0
      let t:goyo_width = width
      call s:resize_pads()
    else
      call s:goyo_off()
    end
  end
endfunction

command! -nargs=? -bar -bang Goyo call s:goyo('<bang>' == '!', <args>)

let &cpo = s:cpo_save
unlet s:cpo_save

