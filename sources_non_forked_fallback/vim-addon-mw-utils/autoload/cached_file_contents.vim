" cached_file_contents.vim
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2010-01-03.
" @Revision:    0.3.0

"exec vam#DefineAndBind('s:c','g:cache_dir_options','{}')
if !exists('g:cache_dir_options') | let g:cache_dir_options = {} | endif | let s:c = g:cache_dir_options 

let s:c['cache_dir'] = get(s:c, 'cache_dir', expand('$HOME').'/.vim-cache')
let s:c['scanned_files'] = get(s:c, 'scanned_files', {})
let s:scanned_files = s:c['scanned_files']


let s:define_cache_file = "let this_dir = s:c['cache_dir'].'/cached-file-contents' | let cache_file = expand(this_dir.'/'.substitute(string([func_as_string, a:file]),'[[\\]{}:/\\,''\"# ]\\+','_','g'))"

" read a file, run function to extract contents and cache the result returned
" by that function in memory. Optionally the result can be cached on disk as
" because VimL can be slow!
"
" file     : the file to be read
" func: { 'func': function which will be called by funcref#Call
"       , 'version' : if this version changes cache will be invalidate automatically
"       , 'ftime_check': optional, default 1. if set to 0 cache isn't updated when file changes and file is in cache
"       }
"
" default: what to return if file doesn't exist
" think twice about adding lines. This function is called many times.
function! cached_file_contents#CachedFileContents(file, func, ...) abort
  let ignore_ftime = a:0 > 0 ? a:1 : 0
  " using string for default so that is evaluated when needed only
  let use_file_cache = get(a:func, 'use_file_cache', 0)

  " simple kind of normalization. necessary when using file caching
  " this seems to be slower:
  " let file = fnamemodify(a:file, ':p') " simple kind of normalization. necessary when using file caching
  " / = assume its an absolute path
  " let file = a:file[0] == '/' ? a:file : expand(a:file, ':p')
  let file = a:file[0] == '/' ? a:file : fnamemodify(a:file, ':p') " simple kind of normalization. necessary when using file caching
  let func_as_string = string(a:func['func'])

  if (!has_key(s:scanned_files, func_as_string))
    let s:scanned_files[func_as_string] = {}
  endif
  let dict = s:scanned_files[func_as_string]
  if use_file_cache && !has_key(dict, a:file)
    exec s:define_cache_file
    if filereadable(cache_file)
      let dict[file] = eval(readfile(cache_file,'b')[0])
    endif
  endif
  if has_key(dict, a:file)
    let d = dict[a:file]
    if use_file_cache
          \ && (ignore_ftime || getftime(a:file) <= d['ftime'])
          \ && d['version'] == a:func['version']
      return dict[a:file]['scan_result']
    endif
  endif
  let scan_result = funcref#Call(a:func['func'], [a:file] )
  let  dict[a:file] = {"ftime": getftime(a:file), 'version': a:func['version'], "scan_result": scan_result }
  if use_file_cache
    if !exists('cache_file') | exec s:define_cache_file | endif
    if !isdirectory(this_dir) | call mkdir(this_dir,'p',0700) | endif
    call writefile([string(dict[a:file])], cache_file)
  endif
  return scan_result
endfunction

fun! cached_file_contents#ClearScanCache()
  let s:c['scanned_files'] = {}

  " Don't run rm -fr. Ask user to run it. It cache_dir may have been set to
  " $HOME ! (should nevere be the case but who knows
  echoe "run manually in your shell:  rm -fr ".shellescape(s:c['cache_dir'])."/*"
endf

fun! cached_file_contents#Test()

  " usually you use a global option so that the function can be reused
  let my_interpreting_func  = {'func' : funcref#Function('return len(readfile(ARGS[0]))'), 'version': 2, 'use_file_cache':1}
  let my_interpreting_func2 = {'func' : funcref#Function('return ARGS[0]')               , 'version': 2, 'use_file_cache':1}

  let tmp = tempname()
  call writefile(['some text','2nd line'], tmp)

  let r = [ cached_file_contents#CachedFileContents(tmp, my_interpreting_func)
        \ , cached_file_contents#CachedFileContents(tmp, my_interpreting_func2) ]
   if r != [2, tmp]
    throw "test failed 1, got ".string(r)
  endif
  unlet r

  sleep 3

  " now let's change contents
  call writefile(['some text','2nd line','3rd line'], tmp)

  let r = cached_file_contents#CachedFileContents(tmp, my_interpreting_func)
  if 3 != r
    throw "test failed 2, got ".string(r)
  endif

  echo "test passed"
endf
