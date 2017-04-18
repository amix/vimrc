" Vim and plug-in option handling.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: April 1, 2015
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#option#get(name, ...) " {{{1
  " Expects one or two arguments: 1. The name of a variable and 2. the default
  " value if the variable does not exist.
  "
  " Returns the value of the variable from a buffer local variable, global
  " variable or the default value, depending on which is defined.
  "
  " This is used by some of my Vim plug-ins for option handling, so that users
  " can customize options for specific buffers.
  if exists('b:' . a:name)
    " Buffer local variable.
    return eval('b:' . a:name)
  elseif exists('g:' . a:name)
    " Global variable.
    return eval('g:' . a:name)
  elseif exists('a:1')
    " Default value.
    return a:1
  endif
endfunction

function! xolox#misc#option#split(value) " {{{1
  " Given a multi-value Vim option like ['runtimepath'] [rtp] this returns a
  " list of strings. For example:
  "
  "     :echo xolox#misc#option#split(&runtimepath)
  "     ['/home/peter/Projects/Vim/misc',
  "      '/home/peter/Projects/Vim/colorscheme-switcher',
  "      '/home/peter/Projects/Vim/easytags',
  "      ...]
  "
  " [rtp]: http://vimdoc.sourceforge.net/htmldoc/options.html#'runtimepath'
  let values = split(a:value, '[^\\]\zs,')
  return map(values, 's:unescape(v:val)')
endfunction

function! s:unescape(s)
  return substitute(a:s, '\\\([\\,]\)', '\1', 'g')
endfunction

function! xolox#misc#option#join(values) " {{{1
  " Given a list of strings like the ones returned by
  " `xolox#misc#option#split()`, this joins the strings together into a
  " single value that can be used to set a Vim option.
  let values = copy(a:values)
  call map(values, 's:escape(v:val)')
  return join(values, ',')
endfunction

function! s:escape(s)
  return escape(a:s, ',\')
endfunction

function! xolox#misc#option#split_tags(value) " {{{1
  " Customized version of `xolox#misc#option#split()` with specialized
  " handling for Vim's ['tags' option] [tags].
  "
  " [tags]: http://vimdoc.sourceforge.net/htmldoc/options.html#'tags'
  let values = split(a:value, '[^\\]\zs,')
  return map(values, 's:unescape_tags(v:val)')
endfunction

function! s:unescape_tags(s)
  return substitute(a:s, '\\\([\\, ]\)', '\1', 'g')
endfunction

function! xolox#misc#option#join_tags(values) " {{{1
  " Customized version of `xolox#misc#option#join()` with specialized
  " handling for Vim's ['tags' option] [tags].
  let values = copy(a:values)
  call map(values, 's:escape_tags(v:val)')
  return join(values, ',')
endfunction

function! s:escape_tags(s)
  return escape(a:s, ', ')
endfunction

function! xolox#misc#option#eval_tags(value, ...) " {{{1
  " Evaluate Vim's ['tags' option] [tags] without looking at the file
  " system, i.e. this will report tags files that don't exist yet. Expects
  " the value of the ['tags' option] [tags] as the first argument. If the
  " optional second argument is 1 (true) only the first match is returned,
  " otherwise (so by default) a list with all matches is returned.
  let pathnames = []
  let first_only = exists('a:1') ? a:1 : 0
  for pattern in xolox#misc#option#split_tags(a:value)
    " Make buffer relative pathnames absolute.
    if pattern =~ '^\./'
      let suffix = matchstr(pattern, '^./\zs.*$')
      let directory = (&cpoptions =~# 'd') ? getcwd() : expand('%:p:h')
      let pattern = xolox#misc#path#merge(directory, suffix)
    endif
    " Make working directory relative pathnames absolute.
    if xolox#misc#path#is_relative(pattern)
      let pattern = xolox#misc#path#merge(getcwd(), pattern)
    endif
    " Ignore the trailing `;' for recursive upwards searching because we
    " always want the most specific pathname available.
    let pattern = substitute(pattern, ';$', '', '')
    " Expand the pattern.
    call extend(pathnames, split(expand(pattern), "\n"))
    if first_only && !empty(pathnames)
      return pathnames[0]
    endif
  endfor
  return first_only ? '' : pathnames
endfunction

" vim: ts=2 sw=2 et
