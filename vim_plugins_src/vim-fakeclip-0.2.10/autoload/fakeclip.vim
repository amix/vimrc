" fakeclip - pseude clipboard register for non-GUI version of Vim
" Version: 0.2.10
" Copyright (C) 2008-2012 Kana Natsuno <http://whileimautomaton.net/>
" License: So-called MIT/X license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
" Platform detection  "{{{1

if has('macunix') || system('uname') =~? '^darwin'
  let s:PLATFORM = 'mac'
elseif has('win32unix')
  let s:PLATFORM = 'cygwin'
elseif $DISPLAY != '' && executable('xclip')
  let s:PLATFORM = 'x'
else
  let s:PLATFORM = 'unknown'
endif


if executable('tmux') && $TMUX != ''
  let g:fakeclip_terminal_multiplexer_type = 'tmux'
elseif executable('screen') && $STY != ''
  let g:fakeclip_terminal_multiplexer_type = 'gnuscreen'
elseif executable('tmux') && executable('screen')
  if exists('g:fakeclip_terminal_multiplexer_type')
    " Use user-defined value as is.
  else
    let g:fakeclip_terminal_multiplexer_type = 'tmux'
  endif
elseif executable('tmux') && !executable('screen')
  let g:fakeclip_terminal_multiplexer_type = 'tmux'
elseif !executable('tmux') && executable('screen')
  let g:fakeclip_terminal_multiplexer_type = 'gnuscreen'
else
  let g:fakeclip_terminal_multiplexer_type = 'unknown'
endif








" Interface  "{{{1
function! fakeclip#clipboard_delete(motion_type)  "{{{2
  return fakeclip#delete('clipboard', a:motion_type)
endfunction




function! fakeclip#clipboard_yank(motion_type)  "{{{2
  return fakeclip#yank('clipboard', a:motion_type)
endfunction




function! fakeclip#content(system_type)  "{{{2
  return s:read_{a:system_type}()
endfunction




function! fakeclip#delete(system_type, motion_type)  "{{{2
  call s:select_last_motion(a:motion_type)
  execute 'normal!' (a:motion_type == 'V' ? 'D' : 'd')
  call s:write_{a:system_type}(@@)
endfunction




function! fakeclip#pastebuffer_delete(motion_type)  "{{{2
  return fakeclip#delete('pastebuffer', a:motion_type)
endfunction




function! fakeclip#pastebuffer_yank(motion_type)  "{{{2
  return fakeclip#yank('pastebuffer', a:motion_type)
endfunction




function! fakeclip#put(system_type, motion_type, put_type)  "{{{2
  let r_ = s:save_register('"')
  let @@ = fakeclip#content(a:system_type)

  if a:motion_type == ''
    execute 'normal!' s:count().a:put_type
    call s:restore_register('"', r_)
  else
    call s:select_last_motion(a:motion_type)
    execute 'normal!' s:count().a:put_type
  endif
endfunction




function! fakeclip#yank(system_type, motion_type)  "{{{2
  let r0 = s:save_register('0')

  call s:select_last_motion(a:motion_type)
  normal! y
  call s:write_{a:system_type}(@@)

  call s:restore_register('0', r0)
endfunction




function! fakeclip#yank_Y(system_type)  "{{{2
  let diff = s:count() - 1
  normal! V
  if 0 < diff
    execute 'normal!' diff.'j'
  endif
  execute 'normal' (a:system_type ==# 'clipboard'
  \                 ? "\<Plug>(fakeclip-Y)"
  \                 : "\<Plug>(fakeclip-screen-Y)")
endfunction








" Core  "{{{1
function! s:read_clipboard()  "{{{2
  return s:read_clipboard_{s:PLATFORM}()
endfunction


function! s:read_clipboard_mac()
  return system('pbpaste')
endfunction


function! s:read_clipboard_cygwin()
  let content = ''
  for line in readfile('/dev/clipboard', 'b')
    let content = content . "\x0A" . substitute(line, "\x0D", '', 'g')
  endfor
  return content[1:]
endfunction


function! s:read_clipboard_x()
  return system('xclip -o')
endfunction


function! s:read_clipboard_unknown()
  echoerr 'Getting the clipboard content is not supported on this platform:'
  \       s:PLATFORM
  return ''
endfunction




function! s:read_pastebuffer()  "{{{2
  return s:read_pastebuffer_{g:fakeclip_terminal_multiplexer_type}()
endfunction


function! s:read_pastebuffer_gnuscreen()
  let _ = tempname()
  call system('screen -X writebuf ' . shellescape(_))
    " FIXME: Here we have to wait "writebuf" for writing, because the
    "        following readfile() may read the temporary file which is not
    "        flushed yet -- but, how to wait?
    " call system(printf('while ! test -f %s; do true; done',
    " \                  shellescape(_)))
    "
    " NB: "writebuf" creates an empty file if the paste buffer is emptied by
    "     "readbuf /dev/null", but it doesn't create any file is the paste
    "     buffer is emptied by "register . ''".  So here we have to check the
    "     existence of the temporary file.
  if exists('g:fakeclip_delay_to_read_pastebuffer_gnuscreen')
    execute 'sleep' g:fakeclip_delay_to_read_pastebuffer_gnuscreen
  endif
  let content = filereadable(_) ? join(readfile(_, 'b'), "\n") : ''
  call delete(_)
  return content
endfunction


function! s:read_pastebuffer_tmux()
  return system('tmux show-buffer')
endfunction


function! s:read_pastebuffer_unknown()
  echoerr 'Paste buffer is not available'
  return ''
endfunction




function! s:write_clipboard(text)  "{{{2
  call s:write_clipboard_{s:PLATFORM}(a:text)
  return
endfunction


function! s:write_clipboard_mac(text)
  call system('pbcopy', a:text)
  return
endfunction


function! s:write_clipboard_cygwin(text)
  call writefile(split(a:text, "\x0A", 1), '/dev/clipboard', 'b')
  return
endfunction


function! s:write_clipboard_x(text)
  call system('xclip', a:text)
  return
endfunction


function! s:write_clipboard_unknown(text)
  echoerr 'Yanking into the clipboard is not supported on this platform:'
  \       s:PLATFORM
  return
endfunction




function! s:write_pastebuffer(text)  "{{{2
  let lines = split(a:text, '\n', !0)
  return s:write_pastebuffer_{g:fakeclip_terminal_multiplexer_type}(lines)
endfunction


function! s:write_pastebuffer_gnuscreen(lines)
  let _ = tempname()
  call writefile(a:lines, _, 'b')
  call system('screen -X readbuf ' . shellescape(_))
  call delete(_)
  return
endfunction


function! s:write_pastebuffer_tmux(lines)
  let _ = tempname()
  call writefile(a:lines, _, 'b')
  call system('tmux load-buffer ' . shellescape(_))
  call delete(_)
  return
endfunction


function! s:write_pastebuffer_unknown(lines)
  echoerr 'Paste buffer is not available'
  return
endfunction








" Misc.  "{{{1
function! fakeclip#_local_variables()  "{{{2
  return s:
endfunction




function! fakeclip#_sid_prefix()  "{{{2
  nnoremap <SID>  <SID>
  return maparg('<SID>', 'n')
endfunction




function! s:count()  "{{{2
  return (v:count == v:count1) ? v:count : ''
endfunction




function! s:restore_register(regname, reginfo)  "{{{2
  call setreg(a:regname, a:reginfo[0], a:reginfo[1])
  return
endfunction




function! s:save_register(regname)  "{{{2
  return [getreg(a:regname), getregtype(a:regname)]
endfunction




function! s:select_last_motion(motion_type)  "{{{2
  let orig_selection = &selection
  let &selection = 'inclusive'

  if a:motion_type == 'char'
    normal! `[v`]
  elseif a:motion_type == 'line'
    normal! '[V']
  elseif a:motion_type == 'block'
    execute "normal! `[\<C-v>`]"
  else  " invoked from visual mode
    normal! gv
  endif

  let &selection = orig_selection
endfunction








" __END__  "{{{1
" vim: foldmethod=marker
