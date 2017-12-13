function! go#keyify#Keyify()
  let bin_path = go#path#CheckBinPath("keyify")
  let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')

  if empty(bin_path) || !exists('*json_decode')
    return
  endif

  " Get result of command as json, that contains `start`, `end` and `replacement`
  let command = printf("%s -json %s:#%s", go#util#Shellescape(bin_path),
    \ go#util#Shellescape(fname), go#util#OffsetCursor())
  let output = go#util#System(command)
  silent! let result = json_decode(output)

  " We want to output the error message in case the result isn't a JSON
  if type(result) != type({})
    call go#util#EchoError(s:chomp(output))
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

" vim: sw=2 ts=2 et
