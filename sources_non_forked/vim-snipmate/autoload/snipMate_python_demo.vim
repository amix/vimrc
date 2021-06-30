" This file demonstrates
" - how to register your own snippet sources (call snipMate_python_demo#Activate() in ftplugin/python.vim)
" - implents a source which creates snippets based on python function
"   definitions found in the current file
"
" Example:
"
" def abc(a,b,c=None)
" will create a snippet on the fly which looks like this:
" abc(${1:a}, ${2:b}, ${3:c=None})

fun! snipMate_python_demo#Activate() abort
  if !exists('g:snipMateSources')
    let g:snipMateSources = {}
  endif

  let g:snipMateSources['python'] = funcref#Function('snipMate_python_demo#FunctionsFromCurrentFileAndTags')
endf

fun! s:Add(dict, line, source, trigger) abort
  let matched = matchlist(a:line,'def\s\+\([^( \t]\+\)[ \t]*(\([^)]*\)')
  if len(matched) > 2
    let name = matched[1]
    " TODO: is this a glob?
    if name !~ a:trigger | return  | endif
    let a:dict[name] = get(a:dict, name, {})
    let sd = a:dict[name]
    let args = []
    let nr=1
    for arg in split(matched[2], '\s*,\s*')
      call add(args, '${'.nr.':'.arg.'}')
      let nr+=1
    endfor
    let sd[a:source] = name.'('.join(args,', ').')'
  endif
endf
fun! snipMate_python_demo#FunctionsFromCurrentFileAndTags(scopes, trigger, result) abort
  " getting all might be too much
  if a:trigger == '*' | return | endif
  if index(a:scopes, 'python') < 0 | return | endif
  for t in taglist('^'.a:trigger)
    call s:Add(a:result, t.cmd, 'tags-' . t.filename, a:trigger)
  endfor
  for l in getline(0, line('$'))
    call s:Add(a:result, l, 'current-file', a:trigger)
  endfor
endf
