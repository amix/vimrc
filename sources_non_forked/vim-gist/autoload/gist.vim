"=============================================================================
" File: gist.vim
" Author: Yasuhiro Matsumoto <mattn.jp@gmail.com>
" Last Change: 10-Oct-2016.
" Version: 7.3
" WebPage: http://github.com/mattn/vim-gist
" License: BSD

let s:save_cpo = &cpoptions
set cpoptions&vim

if exists('g:gist_disabled') && g:gist_disabled == 1
  function! gist#Gist(...) abort
  endfunction
  finish
endif

if !exists('g:github_user') && !executable('git')
  echohl ErrorMsg | echomsg 'Gist: require ''git'' command' | echohl None
  finish
endif

if !executable('curl')
  echohl ErrorMsg | echomsg 'Gist: require ''curl'' command' | echohl None
  finish
endif

if globpath(&rtp, 'autoload/webapi/http.vim') ==# ''
  echohl ErrorMsg | echomsg 'Gist: require ''webapi'', install https://github.com/mattn/webapi-vim' | echohl None
  finish
else
  call webapi#json#true()
endif

let s:gist_token_file = expand(get(g:, 'gist_token_file', '~/.gist-vim'))
let s:system = function(get(g:, 'webapi#system_function', 'system'))

if !exists('g:github_user')
  let g:github_user = substitute(s:system('git config --get github.user'), "\n", '', '')
  if strlen(g:github_user) == 0
    let g:github_user = $GITHUB_USER
  end
endif

if !exists('g:gist_api_url')
  let g:gist_api_url = substitute(s:system('git config --get github.apiurl'), "\n", '', '')
  if strlen(g:gist_api_url) == 0
    let g:gist_api_url = 'https://api.github.com/'
  end
  if exists('g:github_api_url') && !exists('g:gist_shutup_issue154')
    if matchstr(g:gist_api_url, 'https\?://\zs[^/]\+\ze') != matchstr(g:github_api_url, 'https\?://\zs[^/]\+\ze')
      echohl WarningMsg
      echo '--- Warning ---'
      echo 'It seems that you set different URIs for github_api_url/gist_api_url.'
      echo 'If you want to remove this message: let g:gist_shutup_issue154 = 1'
      echohl None
      if confirm('Continue?', '&Yes\n&No') != 1
        let g:gist_disabled = 1
        finish
      endif
      redraw!
    endif
  endif
endif
if g:gist_api_url !~# '/$'
  let g:gist_api_url .= '/'
endif

if !exists('g:gist_update_on_write')
  let g:gist_update_on_write = 1
endif

function! s:get_browser_command() abort
  let gist_browser_command = get(g:, 'gist_browser_command', '')
  if gist_browser_command ==# ''
    if has('win32') || has('win64')
      let gist_browser_command = '!start rundll32 url.dll,FileProtocolHandler %URL%'
    elseif has('mac') || has('macunix') || has('gui_macvim') || system('uname') =~? '^darwin'
      let gist_browser_command = 'open %URL%'
    elseif executable('xdg-open')
      let gist_browser_command = 'xdg-open %URL%'
    elseif executable('firefox')
      let gist_browser_command = 'firefox %URL% &'
    else
      let gist_browser_command = ''
    endif
  endif
  return gist_browser_command
endfunction

function! s:open_browser(url) abort
  let cmd = s:get_browser_command()
  if len(cmd) == 0
    redraw
    echohl WarningMsg
    echo 'It seems that you don''t have general web browser. Open URL below.'
    echohl None
    echo a:url
    return
  endif
  let quote = &shellxquote == '"' ?  "'" : '"'
  if cmd =~# '^!'
    let cmd = substitute(cmd, '%URL%', '\=quote.a:url.quote', 'g')
    silent! exec cmd
  elseif cmd =~# '^:[A-Z]'
    let cmd = substitute(cmd, '%URL%', '\=a:url', 'g')
    exec cmd
  else
    let cmd = substitute(cmd, '%URL%', '\=quote.a:url.quote', 'g')
    call system(cmd)
  endif
endfunction

function! s:shellwords(str) abort
  let words = split(a:str, '\%(\([^ \t\''"]\+\)\|''\([^\'']*\)''\|"\(\%([^\"\\]\|\\.\)*\)"\)\zs\s*\ze')
  let words = map(words, 'substitute(v:val, ''\\\([\\ ]\)'', ''\1'', "g")')
  let words = map(words, 'matchstr(v:val, ''^\%\("\zs\(.*\)\ze"\|''''\zs\(.*\)\ze''''\|.*\)$'')')
  return words
endfunction

function! s:truncate(str, num)
  let mx_first = '^\(.\)\(.*\)$'
  let str = a:str
  let ret = ''
  let width = 0
  while 1
    let char = substitute(str, mx_first, '\1', '')
    let cells = strdisplaywidth(char)
    if cells == 0 || width + cells > a:num
      break
    endif
    let width = width + cells
    let ret .= char
    let str = substitute(str, mx_first, '\2', '')
  endwhile
  while width + 1 <= a:num
    let ret .= ' '
    let width = width + 1
  endwhile
  return ret
endfunction

function! s:format_gist(gist) abort
  let files = sort(keys(a:gist.files))
  if empty(files)
    return ''
  endif
  let file = a:gist.files[files[0]]
  let name = file.filename
  if has_key(file, 'content')
    let code = file.content
    let code = "\n".join(map(split(code, "\n"), '"  ".v:val'), "\n")
  else
    let code = ''
  endif
  let desc = type(a:gist.description)==0 || a:gist.description ==# '' ? '' : a:gist.description
  let name = substitute(name, '[\r\n\t]', ' ', 'g')
  let name = substitute(name, '  ', ' ', 'g')
  let desc = substitute(desc, '[\r\n\t]', ' ', 'g')
  let desc = substitute(desc, '  ', ' ', 'g')
  " Display a nice formatted (and truncated if needed) table of gists on screen
  " Calculate field lengths for gist-listing formatting on screen
  redir =>a |exe 'sil sign place buffer='.bufnr('')|redir end
  let signlist = split(a, '\n')
  let width = winwidth(0) - ((&number||&relativenumber) ? &numberwidth : 0) - &foldcolumn - (len(signlist) > 2 ? 2 : 0)
  let idlen = 33
  let namelen = get(g:, 'gist_namelength', 30)
  let desclen = width - (idlen + namelen + 10)
  return printf('gist: %s %s %s', s:truncate(a:gist.id, idlen), s:truncate(name, namelen), s:truncate(desc, desclen))
endfunction

" Note: A colon in the file name has side effects on Windows due to NTFS Alternate Data Streams; avoid it.
let s:bufprefix = 'gist' . (has('unix') ? ':' : '_')
function! s:GistList(gistls, page) abort
  if a:gistls ==# '-all'
    let url = g:gist_api_url.'gists/public'
  elseif get(g:, 'gist_show_privates', 0) && a:gistls ==# 'starred'
    let url = g:gist_api_url.'gists/starred'
  elseif get(g:, 'gist_show_privates') && a:gistls ==# 'mine'
    let url = g:gist_api_url.'gists'
  else
    let url = g:gist_api_url.'users/'.a:gistls.'/gists'
  endif
  let winnum = bufwinnr(bufnr(s:bufprefix.a:gistls))
  if winnum != -1
    if winnum != bufwinnr('%')
      exe winnum 'wincmd w'
    endif
    setlocal modifiable
  else
    if get(g:, 'gist_list_vsplit', 0)
      exec 'silent noautocmd vsplit +set\ winfixwidth ' s:bufprefix.a:gistls
    elseif get(g:, 'gist_list_rightbelow', 0)
      exec 'silent noautocmd rightbelow 5 split +set\ winfixheight ' s:bufprefix.a:gistls
    else
      exec 'silent noautocmd split' s:bufprefix.a:gistls
    endif
  endif
  if a:page > 1
    let oldlines = getline(0, line('$'))
    let url = url . '?page=' . a:page
  endif

  setlocal modifiable
  let old_undolevels = &undolevels
  let oldlines = []
  silent %d _

  redraw | echon 'Listing gists... '
  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    bw!
    redraw
    echohl ErrorMsg | echomsg v:errmsg | echohl None
    return
  endif
  let res = webapi#http#get(url, '', { 'Authorization': auth })
  if v:shell_error != 0
    bw!
    redraw
    echohl ErrorMsg | echomsg 'Gists not found' | echohl None
    return
  endif
  let content = webapi#json#decode(res.content)
  if type(content) == 4 && has_key(content, 'message') && len(content.message)
    bw!
    redraw
    echohl ErrorMsg | echomsg content.message | echohl None
    if content.message ==# 'Bad credentials'
      call delete(s:gist_token_file)
    endif
    return
  endif

  let lines = map(filter(content, '!empty(v:val.files)'), 's:format_gist(v:val)')
  call setline(1, split(join(lines, "\n"), "\n"))

  $put='more...'

  let b:gistls = a:gistls
  let b:page = a:page
  setlocal buftype=nofile bufhidden=hide noswapfile
  setlocal cursorline
  setlocal nomodified
  setlocal nomodifiable
  syntax match SpecialKey /^gist:/he=e-1
  syntax match Title /^gist: \S\+/hs=s+5 contains=ALL
  nnoremap <silent> <buffer> <cr> :call <SID>GistListAction(0)<cr>
  nnoremap <silent> <buffer> o :call <SID>GistListAction(0)<cr>
  nnoremap <silent> <buffer> b :call <SID>GistListAction(1)<cr>
  nnoremap <silent> <buffer> y :call <SID>GistListAction(2)<cr>
  nnoremap <silent> <buffer> p :call <SID>GistListAction(3)<cr>
  nnoremap <silent> <buffer> <esc> :bw<cr>
  nnoremap <silent> <buffer> <s-cr> :call <SID>GistListAction(1)<cr>

  cal cursor(1+len(oldlines),1)
  nohlsearch
  redraw | echo ''
endfunction

function! gist#list_recursively(user, ...) abort
  let use_cache = get(a:000, 0, 1)
  let limit = get(a:000, 1, -1)
  let verbose = get(a:000, 2, 1)
  if a:user ==# 'mine'
    let url = g:gist_api_url . 'gists'
  elseif a:user ==# 'starred'
    let url = g:gist_api_url . 'gists/starred'
  else
    let url = g:gist_api_url.'users/'.a:user.'/gists'
  endif

  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    " anonymous user cannot get gists to prevent infinite recursive loading
    return []
  endif

  if use_cache && exists('g:gist_list_recursively_cache')
    if has_key(g:gist_list_recursively_cache, a:user)
      return webapi#json#decode(g:gist_list_recursively_cache[a:user])
    endif
  endif

  let page = 1
  let gists = []
  let lastpage = -1

  function! s:get_lastpage(res) abort
    let links = split(a:res.header[match(a:res.header, 'Link')], ',')
    let link = links[match(links, 'rel=[''"]last[''"]')]
    let page = str2nr(matchlist(link, '\%(page=\)\(\d\+\)')[1])
    return page
  endfunction

  if verbose > 0
    redraw | echon 'Loading gists...'
  endif

  while limit == -1 || page <= limit
    let res = webapi#http#get(url.'?page='.page, '', {'Authorization': auth})
    if limit == -1
      " update limit to the last page
      let limit = s:get_lastpage(res)
    endif
    if verbose > 0
      redraw | echon 'Loading gists... ' . page . '/' . limit . ' pages has loaded.'
    endif
    let gists = gists + webapi#json#decode(res.content)
    let page = page + 1
  endwhile
  let g:gist_list_recursively_cache = get(g:, 'gist_list_recursively_cache', {})
  let g:gist_list_recursively_cache[a:user] = webapi#json#encode(gists)
  return gists
endfunction

function! gist#list(user, ...) abort
  let page = get(a:000, 0, 0)
  if a:user ==# '-all'
    let url = g:gist_api_url.'gists/public'
  elseif get(g:, 'gist_show_privates', 0) && a:user ==# 'starred'
    let url = g:gist_api_url.'gists/starred'
  elseif get(g:, 'gist_show_privates') && a:user ==# 'mine'
    let url = g:gist_api_url.'gists'
  else
    let url = g:gist_api_url.'users/'.a:user.'/gists'
  endif

  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    return []
  endif
  let res = webapi#http#get(url, '', { 'Authorization': auth })
  return webapi#json#decode(res.content)
endfunction

function! s:GistGetFileName(gistid) abort
  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    return ''
  endif
  let res = webapi#http#get(g:gist_api_url.'gists/'.a:gistid, '', { 'Authorization': auth })
  let gist = webapi#json#decode(res.content)
  if has_key(gist, 'files')
    return sort(keys(gist.files))[0]
  endif
  return ''
endfunction

function! s:GistDetectFiletype(gistid) abort
  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    return ''
  endif
  let res = webapi#http#get(g:gist_api_url.'gists/'.a:gistid, '', { 'Authorization': auth })
  let gist = webapi#json#decode(res.content)
  let filename = sort(keys(gist.files))[0]
  let ext = fnamemodify(filename, ':e')
  if has_key(s:extmap, ext)
    let type = s:extmap[ext]
  else
    let type = get(gist.files[filename], 'type', 'text')
  endif
  silent! exec 'setlocal ft='.tolower(type)
endfunction

function! s:GistWrite(fname) abort
  if substitute(a:fname, '\\', '/', 'g') == expand("%:p:gs@\\@/@")
    if g:gist_update_on_write != 2 || v:cmdbang
      Gist -e
    else
      echohl ErrorMsg | echomsg 'Please type ":w!" to update a gist.' | echohl None
    endif
  else
    exe 'w'.(v:cmdbang ? '!' : '') fnameescape(v:cmdarg) fnameescape(a:fname)
    silent! exe 'file' fnameescape(a:fname)
    silent! au! BufWriteCmd <buffer>
  endif
endfunction

function! s:GistGet(gistid, clipboard) abort
  redraw | echon 'Getting gist... '
  let res = webapi#http#get(g:gist_api_url.'gists/'.a:gistid, '', { 'Authorization': s:GistGetAuthHeader() })
  if res.status =~# '^2'
    try
      let gist = webapi#json#decode(res.content)
    catch
      redraw
      echohl ErrorMsg | echomsg 'Gist seems to be broken' | echohl None
      return
    endtry
    if get(g:, 'gist_get_multiplefile', 0) != 0
      let num_file = len(keys(gist.files))
    else
      let num_file = 1
    endif
    redraw
    if num_file > len(keys(gist.files))
      echohl ErrorMsg | echomsg 'Gist not found' | echohl None
      return
    endif
    augroup GistWrite
      au!
    augroup END
    for n in range(num_file)
      try
        let old_undolevels = &undolevels
        let filename = sort(keys(gist.files))[n]

        let winnum = bufwinnr(bufnr(s:bufprefix.a:gistid.'/'.filename))
        if winnum != -1
          if winnum != bufwinnr('%')
            exe winnum 'wincmd w'
          endif
          setlocal modifiable
        else
          if num_file == 1
            if get(g:, 'gist_edit_with_buffers', 0)
              let found = -1
              for wnr in range(1, winnr('$'))
                let bnr = winbufnr(wnr)
                if bnr != -1 && !empty(getbufvar(bnr, 'gist'))
                  let found = wnr
                  break
                endif
              endfor
              if found != -1
                exe found 'wincmd w'
                setlocal modifiable
              else
                if get(g:, 'gist_list_vsplit', 0)
                  exec 'silent noautocmd rightbelow vnew'
                else
                  exec 'silent noautocmd rightbelow new'
                endif
              endif
            else
              silent only!
              if get(g:, 'gist_list_vsplit', 0)
                exec 'silent noautocmd rightbelow vnew'
              else
                exec 'silent noautocmd rightbelow new'
              endif
            endif
          else
            if get(g:, 'gist_list_vsplit', 0)
              exec 'silent noautocmd rightbelow vnew'
            else
              exec 'silent noautocmd rightbelow new'
            endif
          endif
          setlocal noswapfile
          silent exec 'noautocmd file' s:bufprefix.a:gistid.'/'.fnameescape(filename)
        endif
        set undolevels=-1
        filetype detect
        silent %d _

        let content = gist.files[filename].content
        call setline(1, split(content, "\n"))
        let b:gist = {
        \ 'filename': filename,
        \ 'id': gist.id,
        \ 'description': gist.description,
        \ 'private': gist.public =~# 'true',
        \}
      catch
        let &undolevels = old_undolevels
        bw!
        redraw
        echohl ErrorMsg | echomsg 'Gist contains binary' | echohl None
        return
      endtry
      let &undolevels = old_undolevels
      setlocal buftype=acwrite bufhidden=hide noswapfile
      setlocal nomodified
      doau StdinReadPost,BufRead,BufReadPost
      let gist_detect_filetype = get(g:, 'gist_detect_filetype', 0)
      if (&ft ==# '' && gist_detect_filetype == 1) || gist_detect_filetype == 2
        call s:GistDetectFiletype(a:gistid)
      endif
      if a:clipboard
        if exists('g:gist_clip_command')
          exec 'silent w !'.g:gist_clip_command
        elseif has('clipboard')
          silent! %yank +
        else
          %yank
        endif
      endif
      1
      augroup GistWrite
        au! BufWriteCmd <buffer> call s:GistWrite(expand("<amatch>"))
      augroup END
    endfor
  else
    bw!
    redraw
    echohl ErrorMsg | echomsg 'Gist not found' | echohl None
    return
  endif
endfunction

function! s:GistListAction(mode) abort
  let line = getline('.')
  let mx = '^gist:\s*\zs\(\w\+\)\ze.*'
  if line =~# mx
    let gistid = matchstr(line, mx)
    if a:mode == 1
      call s:open_browser('https://gist.github.com/' . gistid)
    elseif a:mode == 0
      call s:GistGet(gistid, 0)
      wincmd w
      bw
    elseif a:mode == 2
      call s:GistGet(gistid, 1)
      " TODO close with buffe rname
      bdelete
      bdelete
    elseif a:mode == 3
      call s:GistGet(gistid, 1)
      " TODO close with buffe rname
      bdelete
      bdelete
      normal! "+p
    endif
    return
  endif
  if line =~# '^more\.\.\.$'
    call s:GistList(b:gistls, b:page+1)
    return
  endif
endfunction

function! s:GistUpdate(content, gistid, gistnm, desc) abort
  let gist = { 'id': a:gistid, 'files' : {}, 'description': '','public': function('webapi#json#true') }
  if exists('b:gist')
    if has_key(b:gist, 'filename') && len(a:gistnm) > 0
      let gist.files[b:gist.filename] = { 'content': '', 'filename': b:gist.filename }
      let b:gist.filename = a:gistnm
    endif
    if has_key(b:gist, 'private') && b:gist.private | let gist['public'] = function('webapi#json#false') | endif
    if has_key(b:gist, 'description') | let gist['description'] = b:gist.description | endif
    if has_key(b:gist, 'filename') | let filename = b:gist.filename | endif
  else
    let filename = a:gistnm
    if len(filename) == 0 | let filename = s:GistGetFileName(a:gistid) | endif
    if len(filename) == 0 | let filename = s:get_current_filename(1) | endif
  endif

  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    redraw
    echohl ErrorMsg | echomsg v:errmsg | echohl None
    return
  endif

  " Update description
  " If no new description specified, keep the old description
  if a:desc !=# ' '
    let gist['description'] = a:desc
  else
    let res = webapi#http#get(g:gist_api_url.'gists/'.a:gistid, '', { 'Authorization': auth })
    if res.status =~# '^2'
      let old_gist = webapi#json#decode(res.content)
      let gist['description'] = old_gist.description
    endif
  endif

  let gist.files[filename] = { 'content': a:content, 'filename': filename }

  redraw | echon 'Updating gist... '
  let res = webapi#http#post(g:gist_api_url.'gists/' . a:gistid,
  \ webapi#json#encode(gist), {
  \   'Authorization': auth,
  \   'Content-Type': 'application/json',
  \})
  if res.status =~# '^2'
    let obj = webapi#json#decode(res.content)
    let loc = obj['html_url']
    let b:gist = {'id': a:gistid, 'filename': filename}
    setlocal nomodified
    redraw | echomsg 'Done: '.loc
  else
    let loc = ''
    echohl ErrorMsg | echomsg 'Post failed: ' . res.message | echohl None
  endif
  return loc
endfunction

function! s:GistDelete(gistid) abort
  let auth = s:GistGetAuthHeader()
  if len(auth) == 0
    redraw
    echohl ErrorMsg | echomsg v:errmsg | echohl None
    return
  endif

  redraw | echon 'Deleting gist... '
  let res = webapi#http#post(g:gist_api_url.'gists/'.a:gistid, '', {
  \   'Authorization': auth,
  \   'Content-Type': 'application/json',
  \}, 'DELETE')
  if res.status =~# '^2'
    if exists('b:gist')
      unlet b:gist
    endif
    redraw | echomsg 'Done: '
  else
    echohl ErrorMsg | echomsg 'Delete failed: ' . res.message | echohl None
  endif
endfunction

function! s:get_current_filename(no) abort
  let filename = expand('%:t')
  if len(filename) == 0 && &ft !=# ''
    let pair = filter(items(s:extmap), 'v:val[1] == &ft')
    if len(pair) > 0
      let filename = printf('gistfile%d%s', a:no, pair[0][0])
    endif
  endif
  if filename ==# ''
    let filename = printf('gistfile%d.txt', a:no)
  endif
  return filename
endfunction

function! s:update_GistID(id) abort
  let view = winsaveview()
  normal! gg
  let ret = 0
  if search('\<GistID\>:\s*$')
    let line = getline('.')
    let line = substitute(line, '\s\+$', '', 'g')
    call setline('.', line . ' ' . a:id)
    let ret = 1
  endif
  call winrestview(view)
  return ret
endfunction

" GistPost function:
"   Post new gist to github
"
"   if there is an embedded gist url or gist id in your file,
"   it will just update it.
"                                                   -- by c9s
"
"   embedded gist id format:
"
"       GistID: 123123
"
function! s:GistPost(content, private, desc, anonymous) abort
  let gist = { 'files' : {}, 'description': '','public': function('webapi#json#true') }
  if a:desc !=# ' ' | let gist['description'] = a:desc | endif
  if a:private | let gist['public'] = function('webapi#json#false') | endif
  let filename = s:get_current_filename(1)
  let gist.files[filename] = { 'content': a:content, 'filename': filename }

  let header = {'Content-Type': 'application/json'}
  if !a:anonymous
    let auth = s:GistGetAuthHeader()
    if len(auth) == 0
      redraw
      echohl ErrorMsg | echomsg v:errmsg | echohl None
      return
    endif
    let header['Authorization'] = auth
  endif

  redraw | echon 'Posting it to gist... '
  let res = webapi#http#post(g:gist_api_url.'gists', webapi#json#encode(gist), header)
  if res.status =~# '^2'
    let obj = webapi#json#decode(res.content)
    let loc = obj['html_url']
    let b:gist = {
    \ 'filename': filename,
    \ 'id': matchstr(loc, '[^/]\+$'),
    \ 'description': gist['description'],
    \ 'private': a:private,
    \}
    if s:update_GistID(b:gist['id'])
      Gist -e
    endif
    redraw | echomsg 'Done: '.loc
  else
    let loc = ''
    echohl ErrorMsg | echomsg 'Post failed: '. res.message | echohl None
  endif
  return loc
endfunction

function! s:GistPostBuffers(private, desc, anonymous) abort
  let bufnrs = range(1, bufnr('$'))
  let bn = bufnr('%')
  let query = []

  let gist = { 'files' : {}, 'description': '','public': function('webapi#json#true') }
  if a:desc !=# ' ' | let gist['description'] = a:desc | endif
  if a:private | let gist['public'] = function('webapi#json#false') | endif

  let index = 1
  for bufnr in bufnrs
    if !bufexists(bufnr) || buflisted(bufnr) == 0
      continue
    endif
    echo 'Creating gist content'.index.'... '
    silent! exec 'buffer!' bufnr
    let content = join(getline(1, line('$')), "\n")
    let filename = s:get_current_filename(index)
    let gist.files[filename] = { 'content': content, 'filename': filename }
    let index = index + 1
  endfor
  silent! exec 'buffer!' bn

  let header = {'Content-Type': 'application/json'}
  if !a:anonymous
    let auth = s:GistGetAuthHeader()
    if len(auth) == 0
      redraw
      echohl ErrorMsg | echomsg v:errmsg | echohl None
      return
    endif
    let header['Authorization'] = auth
  endif

  redraw | echon 'Posting it to gist... '
  let res = webapi#http#post(g:gist_api_url.'gists', webapi#json#encode(gist), header)
  if res.status =~# '^2'
    let obj = webapi#json#decode(res.content)
    let loc = obj['html_url']
    let b:gist = {
    \ 'filename': filename,
    \ 'id': matchstr(loc, '[^/]\+$'),
    \ 'description': gist['description'],
    \ 'private': a:private,
    \}
    if s:update_GistID(b:gist['id'])
      Gist -e
    endif
    redraw | echomsg 'Done: '.loc
  else
    let loc = ''
    echohl ErrorMsg | echomsg 'Post failed: ' . res.message | echohl None
  endif
  return loc
endfunction

function! gist#Gist(count, bang, line1, line2, ...) abort
  redraw
  let bufname = bufname('%')
  " find GistID: in content , then we should just update
  let gistid = ''
  let gistls = ''
  let gistnm = ''
  let gistdesc = ' '
  let private = get(g:, 'gist_post_private', 0)
  let multibuffer = 0
  let clipboard = 0
  let deletepost = 0
  let editpost = 0
  let anonymous = get(g:, 'gist_post_anonymous', 0)
  let openbrowser = 0
  let listmx = '^\%(-l\|--list\)\s*\([^\s]\+\)\?$'
  let bufnamemx = '^' . s:bufprefix .'\(\zs[0-9a-f]\+\ze\|\zs[0-9a-f]\+\ze[/\\].*\)$'
  if strlen(g:github_user) == 0 && anonymous == 0
    echohl ErrorMsg | echomsg 'You have not configured a Github account. Read '':help gist-setup''.' | echohl None
    return
  endif
  if a:bang == '!'
    let gistidbuf = ''
  elseif bufname =~# bufnamemx
    let gistidbuf = matchstr(bufname, bufnamemx)
  elseif exists('b:gist') && has_key(b:gist, 'id')
    let gistidbuf = b:gist['id']
  else
    let gistidbuf = matchstr(join(getline(a:line1, a:line2), "\n"), 'GistID:\s*\zs\w\+')
  endif

  let args = (a:0 > 0) ? s:shellwords(a:1) : []
  for arg in args
    if arg =~# '^\(-h\|--help\)$\C'
      help :Gist
      return
    elseif arg =~# '^\(-g\|--git\)$\C' && gistidbuf !=# '' && g:gist_api_url ==# 'https://api.github.com/' && has_key(b:, 'gist') && has_key(b:gist, 'id')
      echo printf('git clone git@github.com:%s', b:gist['id'])
      return
    elseif arg =~# '^\(-G\|--gitclone\)$\C' && gistidbuf !=# '' && g:gist_api_url ==# 'https://api.github.com/' && has_key(b:, 'gist') && has_key(b:gist, 'id')
      exe '!' printf('git clone git@github.com:%s', b:gist['id'])
      return
    elseif arg =~# '^\(-la\|--listall\)$\C'
      let gistls = '-all'
    elseif arg =~# '^\(-ls\|--liststar\)$\C'
      let gistls = 'starred'
    elseif arg =~# '^\(-l\|--list\)$\C'
      if get(g:, 'gist_show_privates')
        let gistls = 'mine'
      else
        let gistls = g:github_user
      endif
    elseif arg =~# '^\(-m\|--multibuffer\)$\C'
      let multibuffer = 1
    elseif arg =~# '^\(-p\|--private\)$\C'
      let private = 1
    elseif arg =~# '^\(-P\|--public\)$\C'
      let private = 0
    elseif arg =~# '^\(-a\|--anonymous\)$\C'
      let anonymous = 1
    elseif arg =~# '^\(-s\|--description\)$\C'
      let gistdesc = ''
    elseif arg =~# '^\(-c\|--clipboard\)$\C'
      let clipboard = 1
    elseif arg =~# '^--rawurl$\C' && gistidbuf !=# '' && g:gist_api_url ==# 'https://api.github.com/'
      let gistid = gistidbuf
      echo 'https://gist.github.com/raw/'.gistid
      return
    elseif arg =~# '^\(-d\|--delete\)$\C' && gistidbuf !=# ''
      let gistid = gistidbuf
      let deletepost = 1
    elseif arg =~# '^\(-e\|--edit\)$\C'
      if gistidbuf !=# ''
        let gistid = gistidbuf
      endif
      let editpost = 1
    elseif arg =~# '^\(+1\|--star\)$\C' && gistidbuf !=# ''
      let auth = s:GistGetAuthHeader()
      if len(auth) == 0
        echohl ErrorMsg | echomsg v:errmsg | echohl None
      else
        let gistid = gistidbuf
        let res = webapi#http#post(g:gist_api_url.'gists/'.gistid.'/star', '', { 'Authorization': auth }, 'PUT')
        if res.status =~# '^2'
          echomsg 'Starred' gistid
        else
          echohl ErrorMsg | echomsg 'Star failed' | echohl None
        endif
      endif
      return
    elseif arg =~# '^\(-1\|--unstar\)$\C' && gistidbuf !=# ''
      let auth = s:GistGetAuthHeader()
      if len(auth) == 0
        echohl ErrorMsg | echomsg v:errmsg | echohl None
      else
        let gistid = gistidbuf
        let res = webapi#http#post(g:gist_api_url.'gists/'.gistid.'/star', '', { 'Authorization': auth }, 'DELETE')
        if res.status =~# '^2'
          echomsg 'Unstarred' gistid
        else
          echohl ErrorMsg | echomsg 'Unstar failed' | echohl None
        endif
      endif
      return
    elseif arg =~# '^\(-f\|--fork\)$\C' && gistidbuf !=# ''
      let auth = s:GistGetAuthHeader()
      if len(auth) == 0
        echohl ErrorMsg | echomsg v:errmsg | echohl None
        return
      else
        let gistid = gistidbuf
        let res = webapi#http#post(g:gist_api_url.'gists/'.gistid.'/fork', '', { 'Authorization': auth })
        if res.status =~# '^2'
          let obj = webapi#json#decode(res.content)
          let gistid = obj['id']
        else
          echohl ErrorMsg | echomsg 'Fork failed' | echohl None
          return
        endif
      endif
    elseif arg =~# '^\(-b\|--browser\)$\C'
      let openbrowser = 1
    elseif arg !~# '^-' && len(gistnm) == 0
      if gistdesc !=# ' '
        let gistdesc = matchstr(arg, '^\s*\zs.*\ze\s*$')
      elseif editpost == 1 || deletepost == 1
        let gistnm = arg
      elseif len(gistls) > 0 && arg !=# '^\w\+$\C'
        let gistls = arg
      elseif arg =~# '^[0-9a-z]\+$\C'
        let gistid = arg
      else
        echohl ErrorMsg | echomsg 'Invalid arguments: '.arg | echohl None
        unlet args
        return 0
      endif
    elseif len(arg) > 0
      echohl ErrorMsg | echomsg 'Invalid arguments: '.arg | echohl None
      unlet args
      return 0
    endif
  endfor
  unlet args
  "echom "gistid=".gistid
  "echom "gistls=".gistls
  "echom "gistnm=".gistnm
  "echom "gistdesc=".gistdesc
  "echom "private=".private
  "echom "clipboard=".clipboard
  "echom "editpost=".editpost
  "echom "deletepost=".deletepost

  if gistidbuf !=# '' && gistid ==# '' && editpost == 0 && deletepost == 0 && anonymous == 0
    let editpost = 1
    let gistid = gistidbuf
  endif

  if len(gistls) > 0
    call s:GistList(gistls, 1)
  elseif len(gistid) > 0 && editpost == 0 && deletepost == 0
    call s:GistGet(gistid, clipboard)
  else
    let url = ''
    if multibuffer == 1
      let url = s:GistPostBuffers(private, gistdesc, anonymous)
    else
      if a:count < 1
        let content = join(getline(a:line1, a:line2), "\n")
      else
        let save_regcont = @"
        let save_regtype = getregtype('"')
        silent! normal! gvy
        let content = @"
        call setreg('"', save_regcont, save_regtype)
      endif
      if editpost == 1
        let url = s:GistUpdate(content, gistid, gistnm, gistdesc)
      elseif deletepost == 1
        call s:GistDelete(gistid)
      else
        let url = s:GistPost(content, private, gistdesc, anonymous)
      endif
      if a:count >= 1 && get(g:, 'gist_keep_selection', 0) == 1
        silent! normal! gv
      endif
    endif
    if type(url) == 1 && len(url) > 0
      if get(g:, 'gist_open_browser_after_post', 0) == 1 || openbrowser
        call s:open_browser(url)
      endif
      let gist_put_url_to_clipboard_after_post = get(g:, 'gist_put_url_to_clipboard_after_post', 1)
      if gist_put_url_to_clipboard_after_post > 0 || clipboard
        if gist_put_url_to_clipboard_after_post == 2
          let url = url . "\n"
        endif
        if exists('g:gist_clip_command')
          call system(g:gist_clip_command, url)
        elseif has('clipboard')
          let @+ = url
        else
          let @" = url
        endif
      endif
    endif
  endif
  return 1
endfunction

function! s:GistGetAuthHeader() abort
  if get(g:, 'gist_use_password_in_gitconfig', 0) != 0
    let password = substitute(system('git config --get github.password'), "\n", '', '')
    if password =~# '^!' | let password = system(password[1:]) | endif
    return printf('basic %s', webapi#base64#b64encode(g:github_user.':'.password))
  endif
  let auth = ''
  if !empty(get(g:, 'gist_token', $GITHUB_TOKEN))
    let auth = 'token ' . get(g:, 'gist_token', $GITHUB_TOKEN)
  elseif filereadable(s:gist_token_file)
    let str = join(readfile(s:gist_token_file), '')
    if type(str) == 1
      let auth = str
    endif
  endif
  if len(auth) > 0
    return auth
  endif

  redraw
  echohl WarningMsg
  echo 'Gist.vim requires authorization to use the GitHub API. These settings are stored in "~/.gist-vim". If you want to revoke, do "rm ~/.gist-vim".'
  echohl None
  let password = inputsecret('GitHub Password for '.g:github_user.':')
  if len(password) == 0
    let v:errmsg = 'Canceled'
    return ''
  endif
  let note = 'Gist.vim on '.hostname().' '.strftime('%Y/%m/%d-%H:%M:%S')
  let note_url = 'http://www.vim.org/scripts/script.php?script_id=2423'
  let insecureSecret = printf('basic %s', webapi#base64#b64encode(g:github_user.':'.password))
  let res = webapi#http#post(g:gist_api_url.'authorizations', webapi#json#encode({
              \  'scopes'   : ['gist'],
              \  'note'     : note,
              \  'note_url' : note_url,
              \}), {
              \  'Content-Type'  : 'application/json',
              \  'Authorization' : insecureSecret,
              \})
  let h = filter(res.header, 'stridx(v:val, "X-GitHub-OTP:") == 0')
  if len(h)
    let otp = inputsecret('OTP:')
    if len(otp) == 0
      let v:errmsg = 'Canceled'
      return ''
    endif
    let res = webapi#http#post(g:gist_api_url.'authorizations', webapi#json#encode({
                \  'scopes'   : ['gist'],
                \  'note'     : note,
                \  'note_url' : note_url,
                \}), {
                \  'Content-Type'  : 'application/json',
                \  'Authorization' : insecureSecret,
                \  'X-GitHub-OTP'  : otp,
                \})
  endif
  let authorization = webapi#json#decode(res.content)
  if has_key(authorization, 'token')
    let secret = printf('token %s', authorization.token)
    call writefile([secret], s:gist_token_file)
    if !(has('win32') || has('win64'))
      call system('chmod go= '.s:gist_token_file)
    endif
  elseif has_key(authorization, 'message')
    let secret = ''
    let v:errmsg = authorization.message
  endif
  return secret
endfunction

let s:extmap = extend({
\'.adb': 'ada',
\'.ahk': 'ahk',
\'.arc': 'arc',
\'.as': 'actionscript',
\'.asm': 'asm',
\'.asp': 'asp',
\'.aw': 'php',
\'.b': 'b',
\'.bat': 'bat',
\'.befunge': 'befunge',
\'.bmx': 'bmx',
\'.boo': 'boo',
\'.c-objdump': 'c-objdump',
\'.c': 'c',
\'.cfg': 'cfg',
\'.cfm': 'cfm',
\'.ck': 'ck',
\'.cl': 'cl',
\'.clj': 'clj',
\'.cmake': 'cmake',
\'.coffee': 'coffee',
\'.cpp': 'cpp',
\'.cppobjdump': 'cppobjdump',
\'.cs': 'csharp',
\'.css': 'css',
\'.cw': 'cw',
\'.d-objdump': 'd-objdump',
\'.d': 'd',
\'.darcspatch': 'darcspatch',
\'.diff': 'diff',
\'.duby': 'duby',
\'.dylan': 'dylan',
\'.e': 'e',
\'.ebuild': 'ebuild',
\'.eclass': 'eclass',
\'.el': 'lisp',
\'.erb': 'erb',
\'.erl': 'erlang',
\'.f90': 'f90',
\'.factor': 'factor',
\'.feature': 'feature',
\'.fs': 'fs',
\'.fy': 'fy',
\'.go': 'go',
\'.groovy': 'groovy',
\'.gs': 'gs',
\'.gsp': 'gsp',
\'.haml': 'haml',
\'.hs': 'haskell',
\'.html': 'html',
\'.hx': 'hx',
\'.ik': 'ik',
\'.ino': 'ino',
\'.io': 'io',
\'.j': 'j',
\'.java': 'java',
\'.js': 'javascript',
\'.json': 'json',
\'.jsp': 'jsp',
\'.kid': 'kid',
\'.lhs': 'lhs',
\'.lisp': 'lisp',
\'.ll': 'll',
\'.lua': 'lua',
\'.ly': 'ly',
\'.m': 'objc',
\'.mak': 'mak',
\'.man': 'man',
\'.mao': 'mao',
\'.matlab': 'matlab',
\'.md': 'markdown',
\'.minid': 'minid',
\'.ml': 'ml',
\'.moo': 'moo',
\'.mu': 'mu',
\'.mustache': 'mustache',
\'.mxt': 'mxt',
\'.myt': 'myt',
\'.n': 'n',
\'.nim': 'nim',
\'.nu': 'nu',
\'.numpy': 'numpy',
\'.objdump': 'objdump',
\'.ooc': 'ooc',
\'.parrot': 'parrot',
\'.pas': 'pas',
\'.pasm': 'pasm',
\'.pd': 'pd',
\'.phtml': 'phtml',
\'.pir': 'pir',
\'.pl': 'perl',
\'.po': 'po',
\'.py': 'python',
\'.pytb': 'pytb',
\'.pyx': 'pyx',
\'.r': 'r',
\'.raw': 'raw',
\'.rb': 'ruby',
\'.rhtml': 'rhtml',
\'.rkt': 'rkt',
\'.rs': 'rs',
\'.rst': 'rst',
\'.s': 's',
\'.sass': 'sass',
\'.sc': 'sc',
\'.scala': 'scala',
\'.scm': 'scheme',
\'.scpt': 'scpt',
\'.scss': 'scss',
\'.self': 'self',
\'.sh': 'sh',
\'.sml': 'sml',
\'.sql': 'sql',
\'.st': 'smalltalk',
\'.swift': 'swift',
\'.tcl': 'tcl',
\'.tcsh': 'tcsh',
\'.tex': 'tex',
\'.textile': 'textile',
\'.tpl': 'smarty',
\'.twig': 'twig',
\'.txt' : 'text',
\'.v': 'verilog',
\'.vala': 'vala',
\'.vb': 'vbnet',
\'.vhd': 'vhdl',
\'.vim': 'vim',
\'.weechatlog': 'weechatlog',
\'.xml': 'xml',
\'.xq': 'xquery',
\'.xs': 'xs',
\'.yml': 'yaml',
\}, get(g:, 'gist_extmap', {}))

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
