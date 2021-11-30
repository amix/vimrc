" Location: autoload/rhubarb.vim
" Author: Tim Pope <http://tpo.pe/>

if exists('g:autoloaded_rhubarb')
  finish
endif
let g:autoloaded_rhubarb = 1

" Section: Utility

function! s:throw(string) abort
  let v:errmsg = 'rhubarb: '.a:string
  throw v:errmsg
endfunction

function! s:shellesc(arg) abort
  if a:arg =~# '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  elseif &shell =~# 'cmd' && a:arg !~# '"'
    return '"'.a:arg.'"'
  else
    return shellescape(a:arg)
  endif
endfunction

function! rhubarb#HomepageForUrl(url) abort
  let dict_or_list = get(g:, 'github_enterprise_urls', get(g:, 'fugitive_github_domains', {}))
  if type(dict_or_list) ==# type({})
    let domains = dict_or_list
  elseif type(dict_or_list) == type([])
    let domains = {}
    for domain in dict_or_list
      let domains[substitute(domain, '^.\{-\}://', '', '')] = domain
    endfor
  else
    let domains = {}
  endif
  " [full_url, scheme, host_with_port, host, path]
  if a:url =~# '://'
    let match = matchlist(a:url, '^\(https\=://\|git://\|ssh://\)\%([^@/]\+@\)\=\(\([^/:]\+\)\%(:\d\+\)\=\)/\(.\{-\}\)\%(\.git\)\=/\=$')
  else
    let match = matchlist(a:url, '^\([^@/]\+@\)\=\(\([^:/]\+\)\):\(.\{-\}\)\%(\.git\)\=/\=$')
    if !empty(match)
      let match[1] = 'ssh://'
    endif
  endif
  if empty(match)
    return ''
  elseif match[3] ==# 'github.com' || match[3] ==# 'ssh.github.com'
    return 'https://github.com/' . match[4]
  elseif has_key(domains, match[1] . match[2])
    let key = match[1] . match[2]
  elseif has_key(domains, match[2])
    let key = match[2]
  elseif has_key(domains, match[3])
    let key = match[3]
  else
    return ''
  endif
  let root = domains[key]
  if type(root) !=# type('') && root
    let root = key
  endif
  if empty(root)
    return ''
  elseif root !~# '://'
    let root = (match[1] =~# '^http://' ? 'http://' : 'https://') . root
  endif
  return substitute(root, '/$', '', '') . '/' . match[4]
endfunction

function! rhubarb#homepage_for_url(url) abort
  return rhubarb#HomepageForUrl(a:url)
endfunction

function! s:repo_homepage() abort
  if exists('b:rhubarb_homepage')
    return b:rhubarb_homepage
  endif
  let remote = FugitiveRemoteUrl()
  let homepage = rhubarb#HomepageForUrl(remote)
  if !empty(homepage)
    let b:rhubarb_homepage = homepage
    return b:rhubarb_homepage
  endif
  call s:throw((len(remote) ? remote : 'origin') . ' is not a GitHub repository')
endfunction

" Section: HTTP

function! s:credentials() abort
  if !exists('g:github_user')
    let g:github_user = $GITHUB_USER
    if g:github_user ==# '' && exists('*FugitiveConfigGet')
      let g:github_user = FugitiveConfigGet('github.user', '')
    endif
    if g:github_user ==# ''
      let g:github_user = $LOGNAME
    endif
  endif
  if !exists('g:github_password')
    let g:github_password = $GITHUB_PASSWORD
    if g:github_password ==# '' && exists('*FugitiveConfigGet')
      let g:github_password = FugitiveConfigGet('github.password', '')
    endif
  endif
  return g:github_user.':'.g:github_password
endfunction

function! rhubarb#JsonDecode(string) abort
  if exists('*json_decode')
    return json_decode(a:string)
  endif
  let [null, false, true] = ['', 0, 1]
  let stripped = substitute(a:string,'\C"\(\\.\|[^"\\]\)*"','','g')
  if stripped !~# "[^,:{}\\[\\]0-9.\\-+Eaeflnr-u \n\r\t]"
    try
      return eval(substitute(a:string,"[\r\n]"," ",'g'))
    catch
    endtry
  endif
  call s:throw("invalid JSON: ".a:string)
endfunction

function! rhubarb#JsonEncode(object) abort
  if exists('*json_encode')
    return json_encode(a:object)
  endif
  if type(a:object) == type('')
    return '"' . substitute(a:object, "[\001-\031\"\\\\]", '\=printf("\\u%04x", char2nr(submatch(0)))', 'g') . '"'
  elseif type(a:object) == type([])
    return '['.join(map(copy(a:object), 'rhubarb#JsonEncode(v:val)'),', ').']'
  elseif type(a:object) == type({})
    let pairs = []
    for key in keys(a:object)
      call add(pairs, rhubarb#JsonEncode(key) . ': ' . rhubarb#JsonEncode(a:object[key]))
    endfor
    return '{' . join(pairs, ', ') . '}'
  else
    return string(a:object)
  endif
endfunction

function! s:curl_arguments(path, ...) abort
  let options = a:0 ? a:1 : {}
  let args = ['curl', '-q', '--silent']
  call extend(args, ['-H', 'Accept: application/json'])
  call extend(args, ['-H', 'Content-Type: application/json'])
  call extend(args, ['-A', 'rhubarb.vim'])
  if get(options, 'auth', '') =~# ':'
    call extend(args, ['-u', options.auth])
  elseif has_key(options, 'auth')
    call extend(args, ['-H', 'Authorization: bearer ' . options.auth])
  elseif exists('g:RHUBARB_TOKEN')
    call extend(args, ['-H', 'Authorization: bearer ' . g:RHUBARB_TOKEN])
  elseif s:credentials() !~# '^[^:]*:$'
    call extend(args, ['-u', s:credentials()])
  elseif has('win32') && filereadable(expand('~/.netrc'))
    call extend(args, ['--netrc-file', expand('~/.netrc')])
  else
    call extend(args, ['--netrc'])
  endif
  if has_key(options, 'method')
    call extend(args, ['-X', toupper(options.method)])
  endif
  for header in get(options, 'headers', [])
    call extend(args, ['-H', header])
  endfor
  if type(get(options, 'data', '')) != type('')
    call extend(args, ['-d', rhubarb#JsonEncode(options.data)])
  elseif has_key(options, 'data')
    call extend(args, ['-d', options.data])
  endif
  call add(args, a:path)
  return args
endfunction

function! rhubarb#Request(path, ...) abort
  if !executable('curl')
    call s:throw('cURL is required')
  endif
  if a:path =~# '://'
    let path = a:path
  elseif a:path =~# '^/'
    let path = 'https://api.github.com' . a:path
  else
    let base = s:repo_homepage()
    let path = substitute(a:path, '%s', matchstr(base, '[^/]\+/[^/]\+$'), '')
    if base =~# '//github\.com/'
      let path = 'https://api.github.com/' . path
    else
      let path = substitute(base, '[^/]\+/[^/]\+$', 'api/v3/', '') . path
    endif
  endif
  let options = a:0 ? a:1 : {}
  let args = s:curl_arguments(path, options)
  if exists('*FugitiveExecute') && v:version >= 800
    try
      if has_key(options, 'callback')
        return FugitiveExecute({'argv': args}, { r -> r.exit_status || r.stdout ==# [''] ? '' : options.callback(json_decode(join(r.stdout, ' '))) })
      endif
      let raw = join(FugitiveExecute({'argv': args}).stdout, ' ')
      return empty(raw) ? raw : json_decode(raw)
    catch /^fugitive:/
    endtry
  endif
  let raw = system(join(map(copy(args), 's:shellesc(v:val)'), ' '))
  if has_key(options, 'callback')
    if !v:shell_error && !empty(raw)
      call options.callback(rhubarb#JsonDecode(raw))
    endif
    return {}
  endif
  if raw ==# ''
    return raw
  else
    return rhubarb#JsonDecode(raw)
  endif
endfunction

function! rhubarb#request(...) abort
  return call('rhubarb#Request', a:000)
endfunction

function! rhubarb#RepoRequest(...) abort
  return rhubarb#Request('repos/%s' . (a:0 && a:1 !=# '' ? '/' . a:1 : ''), a:0 > 1 ? a:2 : {})
endfunction

function! rhubarb#repo_request(...) abort
  return call('rhubarb#RepoRequest', a:000)
endfunction

function! s:url_encode(str) abort
  return substitute(a:str, '[?@=&<>%#/:+[:space:]]', '\=submatch(0)==" "?"+":printf("%%%02X", char2nr(submatch(0)))', 'g')
endfunction

function! rhubarb#RepoSearch(type, q, ...) abort
  return call('rhubarb#Request', ['search/'.a:type.'?per_page=100&q=repo:%s'.s:url_encode(' '.a:q)] + a:000)
endfunction

function! rhubarb#repo_search(...) abort
  return call('rhubarb#RepoSearch', a:000)
endfunction

" Section: Issues

let s:reference = '\<\%(\c\%(clos\|resolv\|referenc\)e[sd]\=\|\cfix\%(e[sd]\)\=\)\>'
function! rhubarb#Complete(findstart, base) abort
  if a:findstart
    let existing = matchstr(getline('.')[0:col('.')-1],s:reference.'\s\+\zs[^#/,.;]*$\|[#@[:alnum:]-]*$')
    return col('.')-1-strlen(existing)
  endif
  try
    if a:base =~# '^@'
      return map(rhubarb#RepoRequest('collaborators'), '"@".v:val.login')
    else
      if a:base =~# '^#'
        let prefix = '#'
        let query = ''
      else
        let prefix = s:repo_homepage().'/issues/'
        let query = a:base
      endif
      let response = rhubarb#RepoSearch('issues', 'state:open '.query)
      if type(response) != type({})
        call s:throw('unknown error')
      elseif has_key(response, 'message')
        call s:throw(response.message)
      else
        let issues = get(response, 'items', [])
      endif
      return map(issues, '{"word": prefix.v:val.number, "abbr": "#".v:val.number, "menu": v:val.title, "info": substitute(empty(v:val.body) ? "\n" : v:val.body,"\\r","","g")}')
    endif
  catch /^rhubarb:.*is not a GitHub repository/
    return []
  catch /^\%(fugitive\|rhubarb\):/
    echoerr v:errmsg
  endtry
endfunction

function! rhubarb#omnifunc(findstart, base) abort
  return rhubarb#Complete(a:findstart, a:base)
endfunction

" Section: Fugitive :GBrowse support

function! rhubarb#FugitiveUrl(...) abort
  if a:0 == 1 || type(a:1) == type({})
    let opts = a:1
    let root = rhubarb#HomepageForUrl(get(opts, 'remote', ''))
  else
    return ''
  endif
  if empty(root)
    return ''
  endif
  let path = substitute(opts.path, '^/', '', '')
  if path =~# '^\.git/refs/heads/'
    return root . '/commits/' . path[16:-1]
  elseif path =~# '^\.git/refs/tags/'
    return root . '/releases/tag/' . path[15:-1]
  elseif path =~# '^\.git/refs/remotes/[^/]\+/.'
    return root . '/commits/' . matchstr(path,'remotes/[^/]\+/\zs.*')
  elseif path =~# '^\.git/\%(config$\|hooks\>\)'
    return root . '/admin'
  elseif path =~# '^\.git\>'
    return root
  endif
  let commit = opts.commit
  if get(opts, 'type', '') ==# 'tree' || opts.path =~# '/$'
    let url = substitute(root . '/tree/' . commit . '/' . path, '/$', '', 'g')
  elseif get(opts, 'type', '') ==# 'blob' || opts.path =~# '[^/]$'
    let escaped_commit = substitute(commit, '#', '%23', 'g')
    let url = root . '/blob/' . escaped_commit . '/' . path
    if get(opts, 'line2') > 0 && get(opts, 'line1') == opts.line2
      let url .= '#L' . opts.line1
    elseif get(opts, 'line1') > 0 && get(opts, 'line2') > 0
      let url .= '#L' . opts.line1 . '-L' . opts.line2
    endif
  else
    let url = root . '/commit/' . commit
  endif
  return url
endfunction

function! rhubarb#fugitive_url(...) abort
  return call('rhubarb#FugitiveUrl', a:000)
endfunction

" Section: End
