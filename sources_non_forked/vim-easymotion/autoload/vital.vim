function! vital#of(name) abort
  let files = globpath(&runtimepath, 'autoload/vital/' . a:name . '.vital', 1)
  let file = split(files, "\n")
  if empty(file)
    throw 'vital: version file not found: ' . a:name
  endif
  let ver = readfile(file[0], 'b')
  if empty(ver)
    throw 'vital: invalid version file: ' . a:name
  endif
  return vital#_{substitute(ver[0], '\W', '', 'g')}#new()
endfunction
