" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#keyify#Keyify()
  " Needs: https://github.com/dominikh/go-tools/pull/272
  "\ '-tags', go#config#BuildTags(),
  let l:cmd = ['keyify',
      \ '-json',
      \ printf('%s:#%s', fnamemodify(expand('%'), ':p:gs?\\?/?'), go#util#OffsetCursor())]

  let [l:out, l:err] = go#util#Exec(l:cmd)
  if l:err
    call go#util#EchoError("non-zero exit code: " . l:out)
    return
  endif
  silent! let result = json_decode(l:out)

  " We want to output the error message in case the result isn't a JSON
  if type(result) != type({})
    call go#util#EchoError(s:chomp(l:out))
    return
  endif

  " Because keyify returns the byte before the region we want, we goto the
  " byte after that
  execute "goto" result.start + 1
  let start = getpos('.')
  execute "goto" result.end
  let end = getpos('.')

  let vis_start = getpos("'<")
  let vis_end = getpos("'>")

  " Replace contents between start and end with `replacement`
  call setpos("'<", start)
  call setpos("'>", end)

  let select = 'gv'

  " Make sure the visual mode is 'v', to avoid some bugs
  normal! gv
  if mode() !=# 'v'
    let select .= 'v'
  endif

  silent! execute "normal!" select."\"=result.replacement\<cr>p"

  " Replacement text isn't aligned, so it needs fix
  normal! '<v'>=

  call setpos("'<", vis_start)
  call setpos("'>", vis_end)
endfunction

function! s:chomp(string)
    return substitute(a:string, '\n\+$', '', '')
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
