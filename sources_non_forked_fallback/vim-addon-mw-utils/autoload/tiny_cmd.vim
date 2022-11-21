" vim suffers:

exec vam#DefineAndBind('s:c','g:vim_tiny_cmd', '{}')

fun! tiny_cmd#Put(a)
  let new = get(s:c,'next',0) +1
  let s:c['next'] = new
  let s:c[new] = a:a
  return new
endf

fun! tiny_cmd#Get(nr)
  return s:c[a:nr]
endf

" Get and remove item
fun! tiny_cmd#Pop(nr)
  let r = s:c[a:nr] | unlet s:c[a:nr] | return r
endf
