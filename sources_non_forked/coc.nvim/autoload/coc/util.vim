scriptencoding utf-8
let s:root = expand('<sfile>:h:h:h')
let s:is_win = has('win32') || has('win64')
let s:is_vim = !has('nvim')
let s:vim_api_version = 30

function! coc#util#remote_fns(name)
  let fns = ['init', 'complete', 'should_complete', 'refresh', 'get_startcol', 'on_complete', 'on_enter']
  let res = []
  for fn in fns
    if exists('*coc#source#'.a:name.'#'.fn)
      call add(res, fn)
    endif
  endfor
  return res
endfunction

function! coc#util#do_complete(name, opt, cb) abort
  let handler = 'coc#source#'.a:name.'#complete'
  let l:Cb = {res -> a:cb(v:null, res)}
  let args = [a:opt, l:Cb]
  call call(handler, args)
endfunction

function! coc#util#suggest_variables(bufnr) abort
  return {
      \ 'coc_suggest_disable': getbufvar(a:bufnr, 'coc_suggest_disable', 0),
      \ 'coc_disabled_sources': getbufvar(a:bufnr, 'coc_disabled_sources', []),
      \ 'coc_suggest_blacklist': getbufvar(a:bufnr, 'coc_suggest_blacklist', []),
      \ }
endfunction

function! coc#util#api_version() abort
  return s:vim_api_version
endfunction

function! coc#util#semantic_hlgroups() abort
  let res = split(execute('hi'), "\n")
  let filtered = filter(res, "v:val =~# '^CocSem'")
  return map(filtered, "matchstr(v:val,'\\v^CocSem\\w+')")
endfunction

" get cursor position
function! coc#util#cursor()
  return [line('.') - 1, strchars(strpart(getline('.'), 0, col('.') - 1))]
endfunction

function! coc#util#change_info() abort
  return {'lnum': line('.'), 'col': col('.'), 'line': getline('.'), 'changedtick': b:changedtick}
endfunction

function! coc#util#jumpTo(line, character) abort
  echohl WarningMsg | echon 'coc#util#jumpTo is deprecated, use coc#cursor#move_to instead.' | echohl None
  call coc#cursor#move_to(a:line, a:character)
endfunction

function! coc#util#root_patterns() abort
  return coc#rpc#request('rootPatterns', [bufnr('%')])
endfunction

function! coc#util#get_config(key) abort
  return coc#rpc#request('getConfig', [a:key])
endfunction

function! coc#util#open_terminal(opts) abort
  return coc#ui#open_terminal(a:opts)
endfunction

function! coc#util#synname() abort
  return synIDattr(synID(line('.'), col('.') - 1, 1), 'name')
endfunction

function! coc#util#setline(lnum, line)
  keepjumps call setline(a:lnum, a:line)
endfunction

function! coc#util#path_replace_patterns() abort
  if has('win32unix') && exists('g:coc_cygqwin_path_prefixes')
    echohl WarningMsg 
    echon 'g:coc_cygqwin_path_prefixes is deprecated, use g:coc_uri_prefix_replace_patterns instead' 
    echohl None
    return g:coc_cygqwin_path_prefixes
  endif
  if exists('g:coc_uri_prefix_replace_patterns')
    return g:coc_uri_prefix_replace_patterns
  endif
  return v:null
endfunction

function! coc#util#version()
  if s:is_vim
    return string(v:versionlong)
  endif
  let c = execute('silent version')
  let lines = split(matchstr(c,  'NVIM v\zs[^\n-]*'))
  return lines[0]
endfunction

function! coc#util#check_refresh(bufnr)
  if !bufloaded(a:bufnr)
    return 0
  endif
  if getbufvar(a:bufnr, 'coc_diagnostic_disable', 0)
    return 0
  endif
  if get(g: , 'EasyMotion_loaded', 0)
    return EasyMotion#is_active() != 1
  endif
  return 1
endfunction

function! coc#util#diagnostic_info(bufnr, checkInsert) abort
  let checked = coc#util#check_refresh(a:bufnr)
  if !checked
    return v:null
  endif
  if a:checkInsert && mode() =~# '^i'
    return v:null
  endif
  let locationlist = ''
  let winid = -1
  for info in getwininfo()
    if info['bufnr'] == a:bufnr
      let winid = info['winid']
      let locationlist = get(getloclist(winid, {'title': 1}), 'title', '')
      break
    endif
  endfor
  return {
      \ 'bufnr': bufnr('%'),
      \ 'winid': winid,
      \ 'lnum': line('.'),
      \ 'locationlist': locationlist
      \ }
endfunction

function! coc#util#open_file(cmd, file)
  execute a:cmd .' '.fnameescape(fnamemodify(a:file, ':~:.'))
  return bufnr('%')
endfunction

function! coc#util#job_command()
  if (has_key(g:, 'coc_node_path'))
    let node = expand(g:coc_node_path)
  else
    let node = $COC_NODE_PATH == '' ? 'node' : $COC_NODE_PATH
  endif
  if !executable(node)
    echohl Error | echom '[coc.nvim] "'.node.'" is not executable, checkout https://nodejs.org/en/download/' | echohl None
    return
  endif
  if !filereadable(s:root.'/build/index.js')
    if isdirectory(s:root.'/src')
      echohl Error | echom '[coc.nvim] build/index.js not found, please install dependencies and compile coc.nvim by: yarn install' | echohl None
    else
      echohl Error | echon '[coc.nvim] your coc.nvim is broken.' | echohl None
    endif
    return
  endif
  return [node] + get(g:, 'coc_node_args', ['--no-warnings']) + [s:root.'/build/index.js']
endfunction

function! coc#util#jump(cmd, filepath, ...) abort
  if a:cmd != 'pedit'
    silent! normal! m'
  endif
  let path = a:filepath
  if (has('win32unix'))
    let path = substitute(a:filepath, '\v\\', '/', 'g')
  endif
  let file = fnamemodify(path, ":~:.")
  if a:cmd == 'pedit'
    let extra = empty(get(a:, 1, [])) ? '' : '+'.(a:1[0] + 1)
    exe 'pedit '.extra.' '.fnameescape(file)
    return
  elseif a:cmd == 'drop' && exists('*bufadd')
    let dstbuf = bufadd(path)
    let binfo = getbufinfo(dstbuf)
    if len(binfo) == 1 && empty(binfo[0].windows)
      exec 'buffer '.dstbuf
      let &buflisted = 1
    else
      exec 'drop '.fnameescape(file)
    endif
  elseif a:cmd == 'edit'
    if bufloaded(file)
      exe 'b '.bufnr(file)
    else
      exe a:cmd.' '.fnameescape(file)
    endif
  else
    exe a:cmd.' '.fnameescape(file)
  endif
  if !empty(get(a:, 1, []))
    let line = getline(a:1[0] + 1)
    " TODO need to use utf16 here
    let col = byteidx(line, a:1[1]) + 1
    if col == 0
      let col = 999
    endif
    call cursor(a:1[0] + 1, col)
  endif
  if &filetype ==# ''
    filetype detect
  endif
  if s:is_vim
    redraw
  endif
endfunction

function! coc#util#variables(bufnr) abort
  let info = getbufinfo(a:bufnr)
  let variables = empty(info) ? {} : copy(info[0]['variables'])
  for key in keys(variables)
    if key !~# '\v^coc'
      unlet variables[key]
    endif
  endfor
  return variables
endfunction

function! coc#util#with_callback(method, args, cb)
  function! s:Cb() closure
    try
      let res = call(a:method, a:args)
      call a:cb(v:null, res)
    catch /.*/
      call a:cb(v:exception)
    endtry
  endfunction
  let timeout = s:is_vim ? 10 : 0
  call timer_start(timeout, {-> s:Cb() })
endfunction

function! coc#util#timer(method, args)
  call timer_start(0, { -> s:Call(a:method, a:args)})
endfunction

function! s:Call(method, args)
  try
    call call(a:method, a:args)
    redraw
  catch /.*/
    return 0
  endtry
endfunction

function! coc#util#vim_info()
  return {
        \ 'apiversion': s:vim_api_version,
        \ 'mode': mode(),
        \ 'config': get(g:, 'coc_user_config', {}),
        \ 'floating': has('nvim') && exists('*nvim_open_win') ? v:true : v:false,
        \ 'extensionRoot': coc#util#extension_root(),
        \ 'globalExtensions': get(g:, 'coc_global_extensions', []),
        \ 'lines': &lines,
        \ 'columns': &columns,
        \ 'cmdheight': &cmdheight,
        \ 'pid': coc#util#getpid(),
        \ 'filetypeMap': get(g:, 'coc_filetype_map', {}),
        \ 'version': coc#util#version(),
        \ 'completeOpt': &completeopt,
        \ 'pumevent': exists('##MenuPopupChanged') || exists('##CompleteChanged'),
        \ 'isVim': has('nvim') ? v:false : v:true,
        \ 'isCygwin': has('win32unix') ? v:true : v:false,
        \ 'isMacvim': has('gui_macvim') ? v:true : v:false,
        \ 'isiTerm': $TERM_PROGRAM ==# "iTerm.app",
        \ 'colorscheme': get(g:, 'colors_name', ''),
        \ 'workspaceFolders': get(g:, 'WorkspaceFolders', v:null),
        \ 'background': &background,
        \ 'runtimepath': join(globpath(&runtimepath, '', 0, 1), ','),
        \ 'locationlist': get(g:,'coc_enable_locationlist', 1),
        \ 'progpath': v:progpath,
        \ 'guicursor': &guicursor,
        \ 'tabCount': tabpagenr('$'),
        \ 'updateHighlight': has('nvim-0.5.0') || has('patch-8.1.1719') ? v:true : v:false,
        \ 'vimCommands': get(g:, 'coc_vim_commands', []),
        \ 'sign': exists('*sign_place') && exists('*sign_unplace'),
        \ 'textprop': has('textprop') && has('patch-8.1.1719') && !has('nvim') ? v:true : v:false,
        \ 'dialog': has('nvim-0.4.0') || has('patch-8.2.0750') ? v:true : v:false,
        \ 'semanticHighlights': coc#util#semantic_hlgroups()
        \}
endfunction

function! coc#util#all_state()
  return {
        \ 'bufnr': bufnr('%'),
        \ 'winid': win_getid(),
        \ 'bufnrs': map(getbufinfo({'bufloaded': 1}),'v:val["bufnr"]'),
        \ 'winids': map(getwininfo(),'v:val["winid"]'),
        \ }
endfunction

function! coc#util#install() abort
  let yarncmd = get(g:, 'coc_install_yarn_cmd', executable('yarnpkg') ? 'yarnpkg' : 'yarn')
  call coc#ui#open_terminal({
        \ 'cwd': s:root,
        \ 'cmd': yarncmd.' install --frozen-lockfile --ignore-engines',
        \ 'autoclose': 0,
        \ })
endfunction

function! coc#util#extension_root() abort
  if get(g:, 'coc_node_env', '') ==# 'test'
    return s:root.'/src/__tests__/extensions'
  endif
  if !empty(get(g:, 'coc_extension_root', ''))
    echohl Error | echon 'g:coc_extension_root not used any more, use g:coc_data_home instead' | echohl None
  endif
  return coc#util#get_data_home().'/extensions'
endfunction

function! coc#util#update_extensions(...) abort
  let async = get(a:, 1, 0)
  if async
    call coc#rpc#notify('updateExtensions', [])
  else
    call coc#rpc#request('updateExtensions', [v:true])
  endif
endfunction

function! coc#util#install_extension(args) abort
  let names = filter(copy(a:args), 'v:val !~# "^-"')
  let isRequest = index(a:args, '-sync') != -1
  if isRequest
    call coc#rpc#request('installExtensions', names)
  else
    call coc#rpc#notify('installExtensions', names)
  endif
endfunction

function! coc#util#do_autocmd(name) abort
  if exists('#User#'.a:name)
    exe 'doautocmd <nomodeline> User '.a:name
  endif
endfunction

function! coc#util#rebuild()
  let dir = coc#util#extension_root()
  if !isdirectory(dir) | return | endif
  call coc#ui#open_terminal({
        \ 'cwd': dir,
        \ 'cmd': 'npm rebuild',
        \ 'keepfocus': 1,
        \})
endfunction

function! coc#util#unmap(bufnr, keys) abort
  if bufnr('%') == a:bufnr
    for key in a:keys
      exe 'silent! nunmap <buffer> '.key
    endfor
  endif
endfunction

function! coc#util#refactor_foldlevel(lnum) abort
  if a:lnum <= 2 | return 0 | endif
  let line = getline(a:lnum)
  if line =~# '^\%u3000\s*$' | return 0 | endif
  return 1
endfunction

function! coc#util#refactor_fold_text(lnum) abort
  let range = ''
  let info = get(b:line_infos, a:lnum, [])
  if !empty(info)
    let range = info[0].':'.info[1]
  endif
  return trim(getline(a:lnum)[3:]).' '.range
endfunction

" get tabsize & expandtab option
function! coc#util#get_format_opts(bufnr) abort
  let bufnr = a:bufnr && bufloaded(a:bufnr) ? a:bufnr : bufnr('%')
  let tabsize = getbufvar(bufnr, '&shiftwidth')
  if tabsize == 0
    let tabsize = getbufvar(bufnr, '&tabstop')
  endif
  return {
      \ 'tabsize': tabsize,
      \ 'expandtab': getbufvar(bufnr, '&expandtab'),
      \ 'insertFinalNewline': getbufvar(bufnr, '&eol'),
      \ 'trimTrailingWhitespace': getbufvar(bufnr, 'coc_trim_trailing_whitespace', 0),
      \ 'trimFinalNewlines': getbufvar(bufnr, 'coc_trim_final_newlines', 0)
      \ }
endfunction

function! coc#util#get_editoroption(winid) abort
  if !coc#compat#win_is_valid(a:winid)
    return v:null
  endif
  if has('nvim') && exists('*nvim_win_get_config')
    " avoid float window
    let config = nvim_win_get_config(a:winid)
    if !empty(get(config, 'relative', ''))
      return v:null
    endif
  endif
  let info = getwininfo(a:winid)[0]
  let bufnr = info['bufnr']
  let buftype = getbufvar(bufnr, '&buftype')
  " avoid window for other purpose.
  if buftype !=# '' && buftype !=# 'acwrite'
    return v:null
  endif
  let tabSize = getbufvar(bufnr, '&shiftwidth')
  if tabSize == 0
    let tabSize = getbufvar(bufnr, '&tabstop')
  endif
  return {
        \ 'bufnr': bufnr,
        \ 'winid': a:winid,
        \ 'winids': map(getwininfo(), 'v:val["winid"]'),
        \ 'tabpagenr': info['tabnr'],
        \ 'winnr': winnr(),
        \ 'visibleRanges': s:visible_ranges(a:winid),
        \ 'tabSize': tabSize,
        \ 'insertSpaces': getbufvar(bufnr, '&expandtab') ? v:true : v:false
        \ }
endfunction

function! coc#util#getpid()
  if !has('win32unix')
    return getpid()
  endif
  let cmd = 'cat /proc/' . getpid() . '/winpid'
  return substitute(system(cmd), '\v\n', '', 'gi')
endfunction

" Get indentkeys for indent on TextChangedP, consider = for word indent only.
function! coc#util#get_indentkeys() abort
  if empty(&indentexpr)
    return ''
  endif
  if &indentkeys !~# '='
    return ''
  endif
  return &indentkeys
endfunction

function! coc#util#get_bufoptions(bufnr) abort
  if !bufloaded(a:bufnr) | return v:null | endif
  let bufname = bufname(a:bufnr)
  let buftype = getbufvar(a:bufnr, '&buftype')
  let winid = bufwinid(a:bufnr)
  let size = -1
  if bufnr('%') == a:bufnr
    let size = line2byte(line("$") + 1)
  elseif !empty(bufname)
    let size = getfsize(bufname)
  endif
  let lines = v:null
  if getbufvar(a:bufnr, 'coc_enabled', 1) && (buftype == '' || buftype == 'acwrite') && size < get(g:, 'coc_max_filesize', 2097152)
    let lines = getbufline(a:bufnr, 1, '$')
  endif
  return {
        \ 'bufnr': a:bufnr,
        \ 'size': size,
        \ 'lines': lines,
        \ 'winid': winid,
        \ 'bufname': bufname,
        \ 'buftype': buftype,
        \ 'previewwindow': v:false,
        \ 'eol': getbufvar(a:bufnr, '&eol'),
        \ 'indentkeys': coc#util#get_indentkeys(),
        \ 'variables': coc#util#variables(a:bufnr),
        \ 'filetype': getbufvar(a:bufnr, '&filetype'),
        \ 'iskeyword': getbufvar(a:bufnr, '&iskeyword'),
        \ 'changedtick': getbufvar(a:bufnr, 'changedtick'),
        \ 'fullpath': empty(bufname) ? '' : fnamemodify(bufname, ':p'),
        \}
endfunction

function! coc#util#get_config_home()
  if !empty(get(g:, 'coc_config_home', ''))
      return resolve(expand(g:coc_config_home))
  endif
  if exists('$VIMCONFIG')
    return resolve($VIMCONFIG)
  endif
  if has('nvim')
    if exists('$XDG_CONFIG_HOME')
      return resolve($XDG_CONFIG_HOME."/nvim")
    endif
    if s:is_win
      return resolve($HOME.'/AppData/Local/nvim')
    endif
    return resolve($HOME.'/.config/nvim')
  else
    if s:is_win
      return resolve($HOME."/vimfiles")
    endif
    return resolve($HOME.'/.vim')
  endif
endfunction

function! coc#util#get_data_home()
  if !empty(get(g:, 'coc_data_home', ''))
    let dir = resolve(expand(g:coc_data_home))
  else
    if exists('$XDG_CONFIG_HOME')
      let dir = resolve($XDG_CONFIG_HOME."/coc")
    else
      if s:is_win
        let dir = resolve(expand('~/AppData/Local/coc'))
      else
        let dir = resolve(expand('~/.config/coc'))
      endif
    endif
  endif
  if !isdirectory(dir)
    call coc#notify#create(['creating data directory: '.dir], {
          \ 'borderhighlight': 'CocInfoSign',
          \ 'timeout': 5000,
          \ 'kind': 'info',
          \ })
    call mkdir(dir, "p", 0755)
  endif
  return dir
endfunction

function! coc#util#get_complete_option()
  if get(b:,"coc_suggest_disable",0)
    return v:null
  endif
  let pos = getcurpos()
  let line = getline(pos[1])
  let input = matchstr(strpart(line, 0, pos[2] - 1), '\k*$')
  let col = pos[2] - strlen(input)
  return {
        \ 'word': matchstr(strpart(line, col - 1), '^\k\+'),
        \ 'input': empty(input) ? '' : input,
        \ 'line': line,
        \ 'filetype': &filetype,
        \ 'filepath': expand('%:p'),
        \ 'bufnr': bufnr('%'),
        \ 'linenr': pos[1],
        \ 'colnr' : pos[2],
        \ 'col': col - 1,
        \ 'changedtick': b:changedtick,
        \ 'blacklist': get(b:, 'coc_suggest_blacklist', []),
        \ 'disabled': get(b:, 'coc_disabled_sources', []),
        \ 'indentkeys': coc#util#get_indentkeys()
        \}
endfunction

" used by vim
function! coc#util#get_buf_lines(bufnr, changedtick)
  if !bufloaded(a:bufnr)
    return v:null
  endif
  let changedtick = getbufvar(a:bufnr, 'changedtick')
  if changedtick == a:changedtick
    return v:null
  endif
  return {
        \ 'lines': getbufline(a:bufnr, 1, '$'),
        \ 'changedtick': getbufvar(a:bufnr, 'changedtick')
        \ }
endfunction

" used for TextChangedI with InsertCharPre
function! coc#util#get_changeinfo()
  return {
        \ 'bufnr': bufnr('%'),
        \ 'lnum': line('.'),
        \ 'line': getline('.'),
        \ 'changedtick': b:changedtick,
        \}
endfunction

function! s:visible_ranges(winid) abort
  let info = getwininfo(a:winid)[0]
  let res = []
  if !has_key(info, 'topline') || !has_key(info, 'botline')
    return res
  endif
  let begin = 0
  let curr = info['topline']
  let max = info['botline']
  if win_getid() != a:winid
    return [[curr, max]]
  endif
  while curr <= max
    let closedend = foldclosedend(curr)
    if closedend == -1
      let begin = begin == 0 ? curr : begin
      if curr == max
        call add(res, [begin, curr])
      endif
      let curr = curr + 1
    else
      if begin != 0
        call add(res, [begin, curr - 1])
        let begin = closedend + 1
      endif
      let curr = closedend + 1
    endif
  endwhile
  return res
endfunction
