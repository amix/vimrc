" rhubarb.vim - fugitive.vim extension for GitHub
" Maintainer:   Tim Pope <http://tpo.pe/>

if exists("g:loaded_rhubarb") || v:version < 700 || &cp
  finish
endif
let g:loaded_rhubarb = 1

if !exists('g:dispatch_compilers')
  let g:dispatch_compilers = {}
endif
let g:dispatch_compilers['hub'] = 'git'

function! s:SetUpMessage(filename) abort
  if &omnifunc !~# '^\%(syntaxcomplete#Complete\)\=$' ||
        \ a:filename !~# '\.git[\/].*MSG$' ||
        \ !exists('*FugitiveFind')
    return
  endif
  let dir = exists('*FugitiveConfigGetRegexp') ? FugitiveGitDir() : FugitiveExtractGitDir(a:filename)
  if empty(dir)
    return
  endif
  let config_file = FugitiveFind('.git/config', dir)
  let config = filereadable(config_file) ? readfile(config_file) : []
  if !empty(filter(config,
        \ '!empty(rhubarb#HomepageForUrl(matchstr(v:val, ''^\s*url\s*=\s*"\=\zs[^[:space:]"]*'')))'))
    setlocal omnifunc=rhubarb#Complete
  endif
endfunction

augroup rhubarb
  autocmd!
  if exists('+omnifunc')
    autocmd FileType gitcommit call s:SetUpMessage(expand('<afile>:p'))
  endif
  autocmd BufEnter *
        \ if expand('%') ==# '' && &previewwindow && pumvisible() && getbufvar('#', '&omnifunc') ==# 'rhubarb#omnifunc' |
        \    setlocal nolist linebreak filetype=markdown |
        \ endif
  autocmd BufNewFile,BufRead *.git/{PULLREQ_EDIT,ISSUE_EDIT,RELEASE_EDIT}MSG
        \ if &ft ==# '' || &ft ==# 'conf' |
        \   set ft=gitcommit |
        \ endif
augroup END

if !exists('g:fugitive_browse_handlers')
  let g:fugitive_browse_handlers = []
endif

if index(g:fugitive_browse_handlers, function('rhubarb#FugitiveUrl')) < 0
  call insert(g:fugitive_browse_handlers, function('rhubarb#FugitiveUrl'))
endif
