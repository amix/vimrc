let s:mkdp_root_dir = expand('<sfile>:h:h:h')
let s:pre_build = s:mkdp_root_dir . '/app/bin/markdown-preview-'
let s:package_file = s:mkdp_root_dir . '/package.json'

" echo message
function! mkdp#util#echo_messages(hl, msgs)
  if empty(a:msgs) | return | endif
  execute 'echohl '.a:hl
  if type(a:msgs) ==# 1
    echomsg a:msgs
  else
    for msg in a:msgs
      echom msg
    endfor
  endif
  echohl None
endfunction

" echo url
function! mkdp#util#echo_url(url)
  let l:url = 'Preview page: ' . a:url
  call mkdp#util#echo_messages('Type', l:url)
endfunction

" try open preview page
function! s:try_open_preview_page(timer_id) abort
  let l:server_status = mkdp#rpc#get_server_status()
  if l:server_status !=# 1
    let s:try_id = ''
    call mkdp#rpc#stop_server()
    call mkdp#rpc#start_server()
  endif
endfunction

" open preview page
function! mkdp#util#open_preview_page() abort
  if get(s:, 'try_id', '') !=# ''
    return
  endif
  let l:server_status = mkdp#rpc#get_server_status()
  if l:server_status ==# -1
    call mkdp#rpc#start_server()
  elseif l:server_status ==# 0
    let s:try_id = timer_start(1000, function('s:try_open_preview_page'))
  else
    call mkdp#util#open_browser()
  endif
endfunction

" open browser
function! mkdp#util#open_browser() abort
  call mkdp#rpc#open_browser()
  call mkdp#autocmd#init()
endfunction

function! mkdp#util#stop_preview() abort
  " TODO: delete autocmd
  call mkdp#rpc#stop_server()
endfunction

function! mkdp#util#get_platform() abort
  if has('win32') || has('win64')
    return 'win'
  elseif has('mac') || has('macvim')
    return 'macos'
  endif
  return 'linux'
endfunction

function! s:on_exit(autoclose, bufnr, Callback, job_id, status, ...)
  let content = join(getbufline(a:bufnr, 1, '$'), "\n")
  if a:status == 0 && a:autoclose == 1
    execute 'silent! bd! '.a:bufnr
  endif
  if !empty(a:Callback)
    call call(a:Callback, [a:status, a:bufnr, content])
  endif
endfunction

function! mkdp#util#open_terminal(opts) abort
  if get(a:opts, 'position', 'bottom') ==# 'bottom'
    let p = '5new'
  else
    let p = 'vnew'
  endif
  execute 'belowright '.p.' +setl\ buftype=nofile '
  setl buftype=nofile
  setl winfixheight
  setl norelativenumber
  setl nonumber
  setl bufhidden=wipe
  let cmd = get(a:opts, 'cmd', '')
  let autoclose = get(a:opts, 'autoclose', 1)
  if empty(cmd)
    throw 'command required!'
  endif
  let cwd = get(a:opts, 'cwd', '')
  if !empty(cwd) | execute 'lcd '.cwd | endif
  let keepfocus = get(a:opts, 'keepfocus', 0)
  let bufnr = bufnr('%')
  let Callback = get(a:opts, 'Callback', v:null)
  if has('nvim')
    call termopen(cmd, {
          \ 'on_exit': function('s:on_exit', [autoclose, bufnr, Callback]),
          \})
  else
    call term_start(cmd, {
          \ 'exit_cb': function('s:on_exit', [autoclose, bufnr, Callback]),
          \ 'curwin': 1,
          \})
  endif
  if keepfocus
    wincmd p
  endif
  return bufnr
endfunction

function! s:markdown_preview_installed(status, ...) abort
  if a:status != 0
    call mkdp#util#echo_messages('Error', '[markdown-preview]: install fail')
    return
  endif
  echo '[markdown-preview.nvim]: install completed'
endfunction

function! s:trim(str) abort
  return substitute(a:str, '\v^(\s|\\n)*|(\s|\\n)*$', '', 'g')
endfunction

function! mkdp#util#install(...)
  let l:version = mkdp#util#pre_build_version()
  let l:info = json_decode(join(readfile(s:mkdp_root_dir . '/package.json'), ''))
  if s:trim(l:version) ==# s:trim(l:info.version)
    return
  endif
  let obj = json_decode(join(readfile(s:package_file)))
  let cmd = (mkdp#util#get_platform() ==# 'win' ? 'install.cmd' : './install.sh') . ' v'.obj['version']
  if get(a:, '1', v:false) ==# v:true
    execute 'lcd ' . s:mkdp_root_dir . '/app'
    execute '!' . cmd
  else
    call mkdp#util#open_terminal({
          \ 'cmd': cmd,
          \ 'cwd': s:mkdp_root_dir . '/app',
          \ 'Callback': function('s:markdown_preview_installed')
          \})
    wincmd p
  endif
endfunction

function! mkdp#util#install_sync(...)
  if get(a:, '1', v:false) ==# v:true
    silent call mkdp#util#install(v:true)
  else
    call mkdp#util#install(v:true)
  endif
endfunction

function! mkdp#util#pre_build_version() abort
  let l:pre_build = s:pre_build . mkdp#util#get_platform()
  if has('win32') || has('win64')
    let l:pre_build .= '.exe'
  endif
  if filereadable(l:pre_build)
    let l:info = system(l:pre_build . ' --version')
    if l:info ==# ''
      call mkdp#util#echo_messages('Type', "[markdown-preview.nvim]: Can not execute pre build binary bundle to get version, will download latest pre build binary bundle")
      return ''
    endif
    let l:info = split(l:info, '\n')
    return l:info[0]
  endif
  return ''
endfunction

function! mkdp#util#toggle_preview() abort
    if !get(b:, 'MarkdownPreviewToggleBool')
        call mkdp#util#open_preview_page()
        let b:MarkdownPreviewToggleBool=1
    else
        call mkdp#util#stop_preview()
        let b:MarkdownPreviewToggleBool=0
    endif
endfunction

