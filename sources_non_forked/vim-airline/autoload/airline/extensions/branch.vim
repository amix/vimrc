" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

let s:has_fugitive = exists('*fugitive#head')
let s:has_lawrencium = exists('*lawrencium#statusline')
let s:has_vcscommand = get(g:, 'airline#extensions#branch#use_vcscommand', 0) && exists('*VCSCommandGetStatusLine')

if !s:has_fugitive && !s:has_lawrencium && !s:has_vcscommand
  finish
endif

let s:git_dirs = {}
let s:untracked_git = {}
let s:untracked_hg = {}

let s:head_format = get(g:, 'airline#extensions#branch#format', 0)
if s:head_format == 1
  function! s:format_name(name)
    return fnamemodify(a:name, ':t')
  endfunction
elseif s:head_format == 2
  function! s:format_name(name)
    return pathshorten(a:name)
  endfunction
elseif type(s:head_format) == type('')
  function! s:format_name(name)
    return call(s:head_format, [a:name])
  endfunction
else
  function! s:format_name(name)
    return a:name
  endfunction
endif

function! s:get_git_branch(path)
  if !s:has_fugitive
    return ''
  endif

  let name = fugitive#head(7)
  if empty(name)
    if has_key(s:git_dirs, a:path)
      return s:git_dirs[a:path]
    endif

    let dir = fugitive#extract_git_dir(a:path)
    if empty(dir)
      let name = ''
    else
      try
        let line = join(readfile(dir . '/HEAD'))
        if strpart(line, 0, 16) == 'ref: refs/heads/'
          let name = strpart(line, 16)
        else
          " raw commit hash
          let name = strpart(line, 0, 7)
        endif
      catch
        let name = ''
      endtry
    endif
  endif

  let s:git_dirs[a:path] = name
  return name
endfunction

function! s:get_git_untracked(file)
  let untracked = ''
  if empty(a:file)
    return untracked
  endif
  if has_key(s:untracked_git, a:file)
    let untracked = s:untracked_git[a:file]
  else
    let output    = system('git status --porcelain -- '. a:file)
    if output[0:1] is# '??' && output[3:-2] is? a:file
      let untracked = get(g:, 'airline#extensions#branch#notexists', g:airline_symbols.notexists)
    endif
    let s:untracked_git[a:file] = untracked
  endif
  return untracked
endfunction

function! s:get_hg_untracked(file)
  if s:has_lawrencium
    " delete cache when unlet b:airline head?
    let untracked = ''
    if empty(a:file)
      return untracked
    endif
    if has_key(s:untracked_hg, a:file)
      let untracked = s:untracked_hg[a:file]
    else
      let untracked = (system('hg status -u -- '. a:file)[0] is# '?'  ?
            \ get(g:, 'airline#extensions#branch#notexists', g:airline_symbols.notexists) : '')
      let s:untracked_hg[a:file] = untracked
    endif
    return untracked
  endif
endfunction

function! s:get_hg_branch()
  if s:has_lawrencium
    return lawrencium#statusline()
  endif
  return ''
endfunction

function! airline#extensions#branch#head()
  if exists('b:airline_head') && !empty(b:airline_head)
    return b:airline_head
  endif

  let b:airline_head = ''
  let l:heads = {}
  let l:vcs_priority = get(g:, "airline#extensions#branch#vcs_priority", ["git", "mercurial"])
  let found_fugitive_head = 0

  let l:git_head = s:get_git_branch(expand("%:p:h"))
  let l:hg_head = s:get_hg_branch()

  if !empty(l:git_head)
    let found_fugitive_head = 1
    let l:heads.git = (!empty(l:hg_head) ? "git:" : '') . s:format_name(l:git_head)
    let l:git_untracked = s:get_git_untracked(expand("%:p"))
    let l:heads.git .= l:git_untracked
  endif

  if !empty(l:hg_head)
    let l:heads.mercurial = (!empty(l:git_head) ? "hg:" : '') . s:format_name(l:hg_head)
    let l:hg_untracked = s:get_hg_untracked(expand("%:p"))
    let l:heads.mercurial.= l:hg_untracked
  endif

  if empty(l:heads)
    if s:has_vcscommand
      call VCSCommandEnableBufferSetup()
      if exists('b:VCSCommandBufferInfo')
        let b:airline_head = s:format_name(get(b:VCSCommandBufferInfo, 0, ''))
      endif
    endif
  else
    for vcs in l:vcs_priority
      if has_key(l:heads, vcs)
        if !empty(b:airline_head)
          let b:airline_head = b:airline_head . " | "
        endif
        let b:airline_head = b:airline_head . l:heads[vcs]
      endif
    endfor
  endif

  if exists("g:airline#extensions#branch#displayed_head_limit")
    let w:displayed_head_limit = g:airline#extensions#branch#displayed_head_limit
    if len(b:airline_head) > w:displayed_head_limit - 1
      let b:airline_head = b:airline_head[0:(w:displayed_head_limit - 1)].(&encoding ==? 'utf-8' ?  '…' : '.')
    endif
  endif

  if empty(b:airline_head) || !found_fugitive_head && !s:check_in_path()
    let b:airline_head = ''
  endif
  return b:airline_head
endfunction

function! airline#extensions#branch#get_head()
  let head = airline#extensions#branch#head()
  let empty_message = get(g:, 'airline#extensions#branch#empty_message', '')
  let symbol = get(g:, 'airline#extensions#branch#symbol', g:airline_symbols.branch)
  return empty(head)
        \ ? empty_message
        \ : printf('%s%s', empty(symbol) ? '' : symbol.(g:airline_symbols.space), head)
endfunction

function! s:check_in_path()
  if !exists('b:airline_branch_path')
    let root = get(b:, 'git_dir', get(b:, 'mercurial_dir', ''))
    let bufferpath = resolve(fnamemodify(expand('%'), ':p'))

    if !filereadable(root) "not a file
      " if .git is a directory, it's the old submodule format
      if match(root, '\.git$') >= 0
        let root = expand(fnamemodify(root, ':h'))
      else
        " else it's the newer format, and we need to guesstimate
        let pattern = '\.git\(\\\|\/\)modules\(\\\|\/\)'
        if match(root, pattern) >= 0
          let root = substitute(root, pattern, '', '')
        endif
      endif
    endif

    let b:airline_file_in_root = stridx(bufferpath, root) > -1
  endif
  return b:airline_file_in_root
endfunction

function! s:reset_untracked_cache()
  if exists("s:untracked_git")
    let s:untracked_git={}
  endif
  if exists("s:untracked_hg")
    let s:untracked_hg={}
  endif
endfunction

function! airline#extensions#branch#init(ext)
  call airline#parts#define_function('branch', 'airline#extensions#branch#get_head')

  autocmd BufReadPost * unlet! b:airline_file_in_root
  autocmd CursorHold,ShellCmdPost,CmdwinLeave * unlet! b:airline_head
  autocmd User AirlineBeforeRefresh unlet! b:airline_head
  autocmd BufWritePost,ShellCmdPost * call s:reset_untracked_cache()
endfunction
