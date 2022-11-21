exec vam#DefineAndBind('s:c','g:glob_like', '{}')

" ignore vcs stuff, Don't think you want those..
let s:c['regex_ignore_directory'] = '\<\%([_.]darcs\|\.git\|.svn\|.hg\|.cvs\|.bzr\)\>'
let s:c['glob_cache'] = get(s:c, 'glob_cache', {})
let s:glob_cache = s:c['glob_cache']

fun! glob#Glob(pattern, ...)
  let pattern = a:pattern
  if pattern[0] == '~'
    let pattern = $HOME.pattern[1:]
  endif
  let opts = a:0 > 0 ? a:1 : {}
  " never cache current directory. You're very likely to edit files in it.

  let c = getcwd()
  let cachable = get(opts, 'cachable', 0) && pattern[:len(c)-1] != c
  if cachable && has_key(s:glob_cache, pattern)
    return s:glob_cache[pattern]
  endif

  " FIXME: don't recurse into \.git directory (thus reimplement glob in vimL!)
  let r = filter(split(glob(pattern),"\n"),'v:val !~ '.string(s:c['regex_ignore_directory']))
  if cachable | let s:glob_cache[pattern]  = r | endif
  return r
endf

