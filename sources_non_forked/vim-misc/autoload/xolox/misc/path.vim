" Pathname manipulation functions.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: July 7, 2014
" URL: http://peterodding.com/code/vim/misc/

let s:windows_compatible = xolox#misc#os#is_win()

function! xolox#misc#path#which(...) " {{{1
  " Scan the executable search path (`$PATH`) for one or more external
  " programs. Expects one or more string arguments with program names. Returns
  " a list with the absolute pathnames of all found programs. Here's an
  " example:
  "
  "     :echo xolox#misc#path#which('gvim', 'vim')
  "     ['/usr/local/bin/gvim',
  "      '/usr/bin/gvim',
  "      '/usr/local/bin/vim',
  "      '/usr/bin/vim']
  let extensions = s:windows_compatible ? split($PATHEXT, ';') : ['']
  let matches = []
  let checked = {}
  for program in a:000
    for directory in split($PATH, s:windows_compatible ? ';' : ':')
      let directory = xolox#misc#path#absolute(directory)
      if isdirectory(directory)
        let found = 0
        for extension in extensions
          let path = xolox#misc#path#merge(directory, program . extension)
          if executable(path)
            call add(matches, path)
            let found = 1
          endif
        endfor
        if s:windows_compatible && ! found
          " Maybe the extension is already contained in program; try without
          " $PATHEXT.
          let path = xolox#misc#path#merge(directory, program)
          if executable(path)
            call add(matches, path)
          endif
        endif
      endif
    endfor
  endfor
  return xolox#misc#list#unique(matches)
endfunction

function! xolox#misc#path#split(path) " {{{1
  " Split a pathname (the first and only argument) into a list of pathname
  " components.
  "
  " On Windows, pathnames starting with two slashes or backslashes are UNC
  " paths where the leading slashes are significant... In this case we split
  " like this:
  "
  " - Input: `'//server/share/directory'`
  " - Result: `['//server', 'share', 'directory']`
  "
  " Everything except Windows is treated like UNIX until someone has a better
  " suggestion :-). In this case we split like this:
  "
  " - Input: `'/foo/bar/baz'`
  " - Result: `['/', 'foo', 'bar', 'baz']`
  "
  " To join a list of pathname components back into a single pathname string,
  " use the `xolox#misc#path#join()` function.
  if type(a:path) == type('')
    if s:windows_compatible
      if a:path =~ '^[\/][\/]'
        " UNC pathname.
        return split(a:path, '\%>2c[\/]\+')
      else
        " If it's not a UNC pathname we can simply split on slashes and
        " backslashes, although we should preserve a leading slash (which
        " denotes a pathname that is 'absolute to the current drive').
        let absolute = (a:path =~ '^[\/]')
        let segments = split(a:path, '[\/]\+')
        return absolute ? insert(segments, a:path[0]) : segments
      endif
    else
      " Everything else is treated as UNIX.
      let absolute = (a:path =~ '^/')
      let segments = split(a:path, '/\+')
      return absolute ? insert(segments, '/') : segments
    endif
  endif
  return []
endfunction

function! xolox#misc#path#join(parts) " {{{1
  " Join a list of pathname components (the first and only argument) into a
  " single pathname string. This is the counterpart to the
  " `xolox#misc#path#split()` function and it expects a list of pathname
  " components as returned by `xolox#misc#path#split()`.
  if type(a:parts) == type([])
    if s:windows_compatible
      return join(a:parts, xolox#misc#path#directory_separator())
    elseif get(a:parts, 0) == '/'
      " Absolute path on UNIX (non-Windows).
      return '/' . join(a:parts[1:], '/')
    else
      " Relative path on UNIX (non-Windows).
      return join(a:parts, '/')
    endif
  endif
  return ''
endfunction

function! xolox#misc#path#directory_separator() " {{{1
  " Find the preferred directory separator for the platform and settings.
  return exists('+shellslash') && &shellslash ? '/' : '\'
endfunction

function! xolox#misc#path#absolute(path) " {{{1
  " Canonicalize and resolve a pathname, *regardless of whether it exists*.
  " This is intended to support string comparison to determine whether two
  " pathnames point to the same directory or file.
  if type(a:path) == type('')
    let path = a:path
    " Make the pathname absolute.
    if path =~ '^\~'
      " Expand ~ to $HOME.
      let path = $HOME . '/' . path[1:]
    elseif xolox#misc#path#is_relative(path)
      " Make relative pathnames absolute.
      let path = getcwd() . '/' . path
    endif
    " Resolve symbolic links to find the canonical pathname. In my tests this
    " also removes all symbolic pathname segments (`.' and `..'), even when
    " the pathname does not exist. Also there used to be a bug in resolve()
    " where it wouldn't resolve pathnames ending in a directory separator.
    " Since it's not much trouble to work around, that's what we do.
    let path = resolve(substitute(path, s:windows_compatible ? '[\/]\+$' : '/\+$', '', ''))
    " Normalize directory separators (especially relevant on Windows).
    let parts = xolox#misc#path#split(path)
    if s:windows_compatible && parts[0] =~ '^[\/][\/]'
      " Also normalize the two leading "directory separators" (I'm not
      " sure what else to call them :-) in Windows UNC pathnames.
      let parts[0] = repeat(xolox#misc#path#directory_separator(), 2) . parts[0][2:]
    elseif s:windows_compatible && parts[0] =~ '^[\/]$'
      " If a pathname is relative to the current drive we should add
      " the drive letter in order to make the pathname absolute.
      let parts[0] = matchstr(getcwd(), '^\a:')
    endif
    return xolox#misc#path#join(parts)
  endif
  return ''
endfunction

function! xolox#misc#path#relative(path, base) " {{{1
  " Make an absolute pathname (the first argument) relative to a directory
  " (the second argument).
  let path = xolox#misc#path#split(a:path)
  let base = xolox#misc#path#split(a:base)
  while path != [] && base != [] && path[0] == base[0]
    call remove(path, 0)
    call remove(base, 0)
  endwhile
  let distance = repeat(['..'], len(base))
  return xolox#misc#path#join(distance + path)
endfunction

function! xolox#misc#path#merge(parent, child, ...) " {{{1
  " Join a directory pathname and filename into a single pathname.
  if type(a:parent) == type('') && type(a:child) == type('')
    " TODO Use xolox#misc#path#is_relative()?
    if s:windows_compatible
      let parent = substitute(a:parent, '[\\/]\+$', '', '')
      let child = substitute(a:child, '^[\\/]\+', '', '')
      return parent . '\' . child
    else
      let parent = substitute(a:parent, '/\+$', '', '')
      let child = substitute(a:child, '^/\+', '', '')
      return parent . '/' . child
    endif
  endif
  return ''
endfunction

function! xolox#misc#path#commonprefix(paths) " {{{1
  " Find the common prefix of path components in a list of pathnames.
  let common = xolox#misc#path#split(a:paths[0])
  for path in a:paths
    let index = 0
    for segment in xolox#misc#path#split(path)
      if len(common) <= index
        break
      elseif common[index] != segment
        call remove(common, index, -1)
        break
      endif
      let index += 1
    endfor
  endfor
  return xolox#misc#path#join(common)
endfunction

function! xolox#misc#path#starts_with(a, b) " {{{1
  " Check whether the first pathname starts with the second pathname (expected
  " to be a directory). This does not perform a regular string comparison;
  " first it normalizes both pathnames, then it splits them into their
  " pathname segments and then it compares the segments.
  let a = xolox#misc#path#split(xolox#misc#path#absolute(a:a))
  let b = xolox#misc#path#split(xolox#misc#path#absolute(a:b))
  return a[0 : len(b) - 1] == b
endfunction

function! xolox#misc#path#encode(path) " {{{1
  " Encode a pathname so it can be used as a filename. This uses URL encoding
  " to encode special characters.
  if s:windows_compatible
    let mask = '[*|\\/:"<>?%]'
  elseif xolox#misc#os#is_mac()
    let mask = '[\\/%:]'
  else
    let mask = '[\\/%]'
  endif
  return substitute(a:path, mask, '\=printf("%%%x", char2nr(submatch(0)))', 'g')
endfunction

function! xolox#misc#path#decode(encoded_path) " {{{1
  " Decode a pathname previously encoded with `xolox#misc#path#encode()`.
  return substitute(a:encoded_path, '%\(\x\x\?\)', '\=nr2char("0x" . submatch(1))', 'g')
endfunction

" xolox#misc#path#equals(a, b) - Check whether two pathnames point to the same file. {{{1

if s:windows_compatible
  function! xolox#misc#path#equals(a, b)
    return a:a ==? a:b || xolox#misc#path#absolute(a:a) ==? xolox#misc#path#absolute(a:b)
  endfunction
else
  function! xolox#misc#path#equals(a, b)
    return a:a ==# a:b || xolox#misc#path#absolute(a:a) ==# xolox#misc#path#absolute(a:b)
  endfunction
endif

function! xolox#misc#path#is_relative(path) " {{{1
  " Returns true (1) when the pathname given as the first argument is
  " relative, false (0) otherwise.
  if a:path =~ '^\w\+://'
    return 0
  elseif s:windows_compatible
    return a:path !~ '^\(\w:\|[\\/]\)'
  else
    return a:path !~ '^/'
  endif
endfunction

function! xolox#misc#path#tempdir() " {{{1
  " Create a temporary directory and return the pathname of the directory.
  if !exists('s:tempdir_counter')
    let s:tempdir_counter = 1
  endif
  if exists('*mkdir')
    if s:windows_compatible
      let template = $TMP . '\vim_tempdir_'
    elseif filewritable('/tmp') == 2
      let template = '/tmp/vim_tempdir_'
    endif
  endif
  if !exists('template')
    throw "xolox#misc#path#tempdir() hasn't been implemented on your platform!"
  endif
  while 1
    let directory = template . s:tempdir_counter
    try
      call mkdir(directory, '', 0700)
      return directory
    catch /^Vim\%((\a\+)\)\=:E739/
      " Keep looking for a non-existing directory.
    endtry
    let s:tempdir_counter += 1
  endwhile
endfunction

" vim: ts=2 sw=2 et
