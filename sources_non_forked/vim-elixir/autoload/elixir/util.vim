function! elixir#util#get_filename(word) abort
  let word = a:word

  " get first thing that starts uppercase, until the first space or end of line
  let word = substitute(word,'^\s*\(\u[^ ]\+\).*$','\1','g')

  " remove any trailing characters that don't look like a nested module
  let word = substitute(word,'\.\U.*$','','g')

  " replace module dots with slash
  let word = substitute(word,'\.','/','g')

  " remove any special chars
  let word = substitute(word,'[^A-z0-9-_/]','','g')

  " convert to snake_case
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\l\|\d\)\(\u\)','\1_\2','g')
  let word = substitute(word,'-','_','g')
  let word = tolower(word)

  return word
endfunction
