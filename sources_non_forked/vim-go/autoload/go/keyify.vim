function! go#keyify#Keyify()
  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()
  let bin_path = go#path#CheckBinPath("keyify")
  let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')

  if empty(bin_path) || !exists('*json_decode')
    let $GOPATH = old_gopath
    return
  endif

  " Get result of command as json, that contains `start`, `end` and `replacement`
  let command = printf("%s -json %s:#%s", bin_path, fname, go#util#OffsetCursor())
  let output = go#util#System(command)
  silent! let result = json_decode(output)

  " We want to output the error message in case the result isn't a JSON
  if type(result) != type({})
    call go#util#EchoError(s:chomp(output))
    let $GOPATH = old_gopath
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
  let $GOPATH = old_gopath
endfunction

function! s:chomp(string)
    return substitute(a:string, '\n\+$', '', '')
endfunction
