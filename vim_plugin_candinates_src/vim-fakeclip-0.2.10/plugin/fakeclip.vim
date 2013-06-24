" fakeclip - pseude clipboard register for non-GUI version of Vim
" Version: 0.2.10
" Copyright (C) 2007-2012 Kana Natsuno <http://whileimautomaton.net/>
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
" Prologue  "{{{1

if exists('g:loaded_fakeclip')
  finish
endif








" Key Mappings  "{{{1
" Interface key mappings  "{{{2

nnoremap <silent> <Plug>(fakeclip-y)
\ :<C-u>set operatorfunc=fakeclip#clipboard_yank<Return>g@
vnoremap <silent> <Plug>(fakeclip-y)
\ :<C-u>call fakeclip#yank('clipboard', visualmode())<Return>
nnoremap <silent> <Plug>(fakeclip-Y)
\ :<C-u>call fakeclip#yank_Y('clipboard')<Return>
vnoremap <silent> <Plug>(fakeclip-Y)
\ :<C-u>call fakeclip#yank('clipboard', 'V')<Return>

nnoremap <silent> <Plug>(fakeclip-p)
\ :<C-u>call fakeclip#put('clipboard', '', 'p')<Return>
nnoremap <silent> <Plug>(fakeclip-P)
\ :<C-u>call fakeclip#put('clipboard', '', 'P')<Return>
nnoremap <silent> <Plug>(fakeclip-gp)
\ :<C-u>call fakeclip#put('clipboard', '', 'gp')<Return>
nnoremap <silent> <Plug>(fakeclip-gP)
\ :<C-u>call fakeclip#put('clipboard', '', 'gP')<Return>
nnoremap <silent> <Plug>(fakeclip-]p)
\ :<C-u>call fakeclip#put('clipboard', '', ']p')<Return>
nnoremap <silent> <Plug>(fakeclip-]P)
\ :<C-u>call fakeclip#put('clipboard', '', ']P')<Return>
nnoremap <silent> <Plug>(fakeclip-[p)
\ :<C-u>call fakeclip#put('clipboard', '', '[p')<Return>
nnoremap <silent> <Plug>(fakeclip-[P)
\ :<C-u>call fakeclip#put('clipboard', '', '[P')<Return>
vnoremap <silent> <Plug>(fakeclip-p)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), 'p')<Return>
vnoremap <silent> <Plug>(fakeclip-P)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), 'P')<Return>
vnoremap <silent> <Plug>(fakeclip-gp)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), 'gp')<Return>
vnoremap <silent> <Plug>(fakeclip-gP)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), 'gP')<Return>
vnoremap <silent> <Plug>(fakeclip-]p)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), ']p')<Return>
vnoremap <silent> <Plug>(fakeclip-]P)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), ']P')<Return>
vnoremap <silent> <Plug>(fakeclip-[p)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), '[p')<Return>
vnoremap <silent> <Plug>(fakeclip-[P)
\ :<C-u>call fakeclip#put('clipboard', visualmode(), '[P')<Return>

noremap! <Plug>(fakeclip-insert)
\ <C-r>=fakeclip#content('clipboard')<Return>
noremap! <Plug>(fakeclip-insert-r)
\ <C-r><C-r>=fakeclip#content('clipboard')<Return>
noremap! <Plug>(fakeclip-insert-o)
\ <C-r><C-o>=fakeclip#content('clipboard')<Return>
inoremap <Plug>(fakeclip-insert-p)
\ <C-r><C-p>=fakeclip#content('clipboard')<Return>

nnoremap <silent> <Plug>(fakeclip-d)
\ :<C-u>set operatorfunc=fakeclip#clipboard_delete<Return>g@
vnoremap <silent> <Plug>(fakeclip-d)
\ :<C-u>call fakeclip#delete('clipboard', visualmode())<Return>
nnoremap <silent> <Plug>(fakeclip-dd)
\ :<C-u>set operatorfunc=fakeclip#clipboard_delete<Return>g@g@
nnoremap <silent> <Plug>(fakeclip-D)
\ :<C-u>set operatorfunc=fakeclip#clipboard_delete<Return>g@$
vnoremap <silent> <Plug>(fakeclip-D)
\ :<C-u>call fakeclip#delete('clipboard', 'V')<Return>


nnoremap <silent> <Plug>(fakeclip-screen-y)
\ :<C-u>set operatorfunc=fakeclip#pastebuffer_yank<Return>g@
vnoremap <silent> <Plug>(fakeclip-screen-y)
\ :<C-u>call fakeclip#yank('pastebuffer', visualmode())<Return>
nnoremap <silent> <Plug>(fakeclip-screen-Y)
\ :<C-u>call fakeclip#yank_Y('pastebuffer')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-Y)
\ :<C-u>call fakeclip#yank('pastebuffer', 'V')<Return>

nnoremap <silent> <Plug>(fakeclip-screen-p)
\ :<C-u>call fakeclip#put('pastebuffer', '', 'p')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-P)
\ :<C-u>call fakeclip#put('pastebuffer', '', 'P')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-gp)
\ :<C-u>call fakeclip#put('pastebuffer', '', 'gp')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-gP)
\ :<C-u>call fakeclip#put('pastebuffer', '', 'gP')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-]p)
\ :<C-u>call fakeclip#put('pastebuffer', '', ']p')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-]P)
\ :<C-u>call fakeclip#put('pastebuffer', '', ']P')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-[p)
\ :<C-u>call fakeclip#put('pastebuffer', '', '[p')<Return>
nnoremap <silent> <Plug>(fakeclip-screen-[P)
\ :<C-u>call fakeclip#put('pastebuffer', '', '[P')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-p)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), 'p')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-P)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), 'P')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-gp)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), 'gp')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-gP)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), 'gP')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-]p)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), ']p')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-]P)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), ']P')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-[p)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), '[p')<Return>
vnoremap <silent> <Plug>(fakeclip-screen-[P)
\ :<C-u>call fakeclip#put('pastebuffer', visualmode(), '[P')<Return>

noremap! <Plug>(fakeclip-screen-insert)
\ <C-r>=fakeclip#content('pastebuffer')<Return>
noremap! <Plug>(fakeclip-screen-insert-r)
\ <C-r><C-r>=fakeclip#content('pastebuffer')<Return>
noremap! <Plug>(fakeclip-screen-insert-o)
\ <C-r><C-o>=fakeclip#content('pastebuffer')<Return>
inoremap <Plug>(fakeclip-screen-insert-p)
\ <C-r><C-p>=fakeclip#content('pastebuffer')<Return>

nnoremap <silent> <Plug>(fakeclip-screen-d)
\ :<C-u>set operatorfunc=fakeclip#pastebuffer_delete<Return>g@
vnoremap <silent> <Plug>(fakeclip-screen-d)
\ :<C-u>call fakeclip#delete('pastebuffer', visualmode())<Return>
nnoremap <silent> <Plug>(fakeclip-screen-dd)
\ :<C-u>set operatorfunc=fakeclip#pastebuffer_delete<Return>g@g@
nnoremap <silent> <Plug>(fakeclip-screen-D)
\ :<C-u>set operatorfunc=fakeclip#pastebuffer_delete<Return>g@$
vnoremap <silent> <Plug>(fakeclip-screen-D)
\ :<C-u>call fakeclip#delete('pastebuffer', 'V')<Return>




" Default key mappings  "{{{2

command! -bang -bar -nargs=0 FakeclipDefaultKeyMappings
\ call s:cmd_FakeclipDefaultKeyMappings(<bang>0)

function! s:cmd_FakeclipDefaultKeyMappings(banged_p)
  let modifier = a:banged_p ? '' : '<unique>'
  " Clipboard
  if !has('clipboard')
    for _ in ['+', '*']
      execute 'silent! nmap '.modifier.' "'._.'y  <Plug>(fakeclip-y)'
      execute 'silent! nmap '.modifier.' "'._.'Y  <Plug>(fakeclip-Y)'
      execute 'silent! nmap '.modifier.' "'._.'yy  <Plug>(fakeclip-Y)'
      execute 'silent! vmap '.modifier.' "'._.'y  <Plug>(fakeclip-y)'
      execute 'silent! vmap '.modifier.' "'._.'Y  <Plug>(fakeclip-Y)'

      execute 'silent! nmap '.modifier.' "'._.'p  <Plug>(fakeclip-p)'
      execute 'silent! nmap '.modifier.' "'._.'P  <Plug>(fakeclip-P)'
      execute 'silent! nmap '.modifier.' "'._.'gp  <Plug>(fakeclip-gp)'
      execute 'silent! nmap '.modifier.' "'._.'gP  <Plug>(fakeclip-gP)'
      execute 'silent! nmap '.modifier.' "'._.']p  <Plug>(fakeclip-]p)'
      execute 'silent! nmap '.modifier.' "'._.']P  <Plug>(fakeclip-]P)'
      execute 'silent! nmap '.modifier.' "'._.'[p  <Plug>(fakeclip-[p)'
      execute 'silent! nmap '.modifier.' "'._.'[P  <Plug>(fakeclip-[P)'
      execute 'silent! vmap '.modifier.' "'._.'p  <Plug>(fakeclip-p)'
      execute 'silent! vmap '.modifier.' "'._.'P  <Plug>(fakeclip-P)'
      execute 'silent! vmap '.modifier.' "'._.'gp  <Plug>(fakeclip-gp)'
      execute 'silent! vmap '.modifier.' "'._.'gP  <Plug>(fakeclip-gP)'
      execute 'silent! vmap '.modifier.' "'._.']p  <Plug>(fakeclip-]p)'
      execute 'silent! vmap '.modifier.' "'._.']P  <Plug>(fakeclip-]P)'
      execute 'silent! vmap '.modifier.' "'._.'[p  <Plug>(fakeclip-[p)'
      execute 'silent! vmap '.modifier.' "'._.'[P  <Plug>(fakeclip-[P)'

      execute 'silent! map! '.modifier.' <C-r>'._.'  <Plug>(fakeclip-insert)'
      execute 'silent! map! '.modifier.' <C-r><C-r>'._.'  <Plug>(fakeclip-insert-r)'
      execute 'silent! map! '.modifier.' <C-r><C-o>'._.'  <Plug>(fakeclip-insert-o)'
      execute 'silent! imap '.modifier.' <C-r><C-p>'._.'  <Plug>(fakeclip-insert-p)'

      execute 'silent! nmap '.modifier.' "'._.'d  <Plug>(fakeclip-d)'
      execute 'silent! vmap '.modifier.' "'._.'d  <Plug>(fakeclip-d)'
      execute 'silent! nmap '.modifier.' "'._.'dd  <Plug>(fakeclip-dd)'
      execute 'silent! nmap '.modifier.' "'._.'D  <Plug>(fakeclip-D)'
      execute 'silent! vmap '.modifier.' "'._.'D  <Plug>(fakeclip-D)'
    endfor
  endif

  " Paste buffer
  execute 'silent! nmap '.modifier.' "&y  <Plug>(fakeclip-screen-y)'
  execute 'silent! nmap '.modifier.' "&Y  <Plug>(fakeclip-screen-Y)'
  execute 'silent! nmap '.modifier.' "&yy  <Plug>(fakeclip-screen-Y)'
  execute 'silent! vmap '.modifier.' "&y  <Plug>(fakeclip-screen-y)'
  execute 'silent! vmap '.modifier.' "&Y  <Plug>(fakeclip-screen-Y)'

  execute 'silent! nmap '.modifier.' "&p  <Plug>(fakeclip-screen-p)'
  execute 'silent! nmap '.modifier.' "&P  <Plug>(fakeclip-screen-P)'
  execute 'silent! nmap '.modifier.' "&gp  <Plug>(fakeclip-screen-gp)'
  execute 'silent! nmap '.modifier.' "&gP  <Plug>(fakeclip-screen-gP)'
  execute 'silent! nmap '.modifier.' "&]p  <Plug>(fakeclip-screen-]p)'
  execute 'silent! nmap '.modifier.' "&]P  <Plug>(fakeclip-screen-]P)'
  execute 'silent! nmap '.modifier.' "&[p  <Plug>(fakeclip-screen-[p)'
  execute 'silent! nmap '.modifier.' "&[P  <Plug>(fakeclip-screen-[P)'
  execute 'silent! vmap '.modifier.' "&p  <Plug>(fakeclip-screen-p)'
  execute 'silent! vmap '.modifier.' "&P  <Plug>(fakeclip-screen-P)'
  execute 'silent! vmap '.modifier.' "&gp  <Plug>(fakeclip-screen-gp)'
  execute 'silent! vmap '.modifier.' "&gP  <Plug>(fakeclip-screen-gP)'
  execute 'silent! vmap '.modifier.' "&]p  <Plug>(fakeclip-screen-]p)'
  execute 'silent! vmap '.modifier.' "&]P  <Plug>(fakeclip-screen-]P)'
  execute 'silent! vmap '.modifier.' "&[p  <Plug>(fakeclip-screen-[p)'
  execute 'silent! vmap '.modifier.' "&[P  <Plug>(fakeclip-screen-[P)'

  execute 'silent! map! '.modifier.' <C-r>&  <Plug>(fakeclip-screen-insert)'
  execute 'silent! map! '.modifier.' <C-r><C-r>&  <Plug>(fakeclip-screen-insert-r)'
  execute 'silent! map! '.modifier.' <C-r><C-o>&  <Plug>(fakeclip-screen-insert-o)'
  execute 'silent! imap '.modifier.' <C-r><C-p>&  <Plug>(fakeclip-screen-insert-p)'

  execute 'silent! nmap '.modifier.' "&d  <Plug>(fakeclip-screen-d)'
  execute 'silent! vmap '.modifier.' "&d  <Plug>(fakeclip-screen-d)'
  execute 'silent! nmap '.modifier.' "&dd  <Plug>(fakeclip-screen-dd)'
  execute 'silent! nmap '.modifier.' "&D  <Plug>(fakeclip-screen-D)'
  execute 'silent! vmap '.modifier.' "&D  <Plug>(fakeclip-screen-D)'
endfunction

if !exists('g:fakeclip_no_default_key_mappings')
  FakeclipDefaultKeyMappings
endif








" Epilogue  "{{{1

let g:loaded_fakeclip = 1








" __END__  "{{{1
" vim: foldmethod=marker foldlevel=0
