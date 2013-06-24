" vim:tabstop=2:shiftwidth=2:expandtab:foldmethod=marker:textwidth=79
" Vimwiki plugin file
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/
" GetLatestVimScripts: 2226 1 :AutoInstall: vimwiki

if exists("loaded_vimwiki") || &cp
  finish
endif
let loaded_vimwiki = 1

let s:old_cpo = &cpo
set cpo&vim

" HELPER functions {{{
function! s:default(varname, value) "{{{
  if !exists('g:vimwiki_'.a:varname)
    let g:vimwiki_{a:varname} = a:value
  endif
endfunction "}}}

" return longest common path prefix of 2 given paths.
" '~/home/usrname/wiki', '~/home/usrname/wiki/shmiki' => '~/home/usrname/wiki'
function! s:path_common_pfx(path1, path2) "{{{
  let p1 = split(a:path1, '[/\\]', 1)
  let p2 = split(a:path2, '[/\\]', 1)

  let idx = 0
  let minlen = min([len(p1), len(p2)])
  while (idx < minlen) && (p1[idx] ==? p2[idx])
    let idx = idx + 1
  endwhile
  if idx == 0
    return ''
  else
    return join(p1[: idx-1], '/')
  endif
endfunction "}}}

function! s:find_wiki(path) "{{{
  let idx = 0
  while idx < len(g:vimwiki_list)
    let path = vimwiki#base#chomp_slash(expand(VimwikiGet('path', idx)))
    let path = vimwiki#base#path_norm(path)
    if s:path_common_pfx(path, a:path) == path
      return idx
    endif
    let idx += 1
  endwhile
  return -1
endfunction "}}}

function! s:setup_buffer_leave()"{{{
  if &filetype == 'vimwiki' && !exists("b:vimwiki_idx")
    let b:vimwiki_idx = g:vimwiki_current_idx
  endif

  " Set up menu
  if g:vimwiki_menu != ""
    exe 'nmenu disable '.g:vimwiki_menu.'.Table'
  endif
endfunction"}}}

function! s:setup_filetype() "{{{
    " Find what wiki current buffer belongs to.
    let path = expand('%:p:h')
    let ext = '.'.expand('%:e')
    let idx = s:find_wiki(path)

    if idx == -1 && g:vimwiki_global_ext == 0
      return
    endif

  set filetype=vimwiki
endfunction "}}}

function! s:setup_buffer_enter() "{{{
  if exists("b:vimwiki_idx")
    let g:vimwiki_current_idx = b:vimwiki_idx
  else
    " Find what wiki current buffer belongs to.
    " If wiki does not exist in g:vimwiki_list -- add new wiki there with
    " buffer's path and ext.
    " Else set g:vimwiki_current_idx to that wiki index.
    let path = expand('%:p:h')
    let ext = '.'.expand('%:e')
    let idx = s:find_wiki(path)

    " The buffer's file is not in the path and user do not want his wiki
    " extension to be global -- do not add new wiki.
    if idx == -1 && g:vimwiki_global_ext == 0
      return
    endif

    if idx == -1
      call add(g:vimwiki_list, {'path': path, 'ext': ext, 'temp': 1})
      let g:vimwiki_current_idx = len(g:vimwiki_list) - 1
    else
      let g:vimwiki_current_idx = idx
    endif

    let b:vimwiki_idx = g:vimwiki_current_idx
  endif

  " If you have
  "     au GUIEnter * VimwikiIndex
  " Then change it to
  "     au GUIEnter * nested VimwikiIndex
  if &filetype == ''
    set filetype=vimwiki
  endif

  " Update existed/non-existed links highlighting.
  call vimwiki#base#highlight_links()

  " Settings foldmethod, foldexpr and foldtext are local to window. Thus in a
  " new tab with the same buffer folding is reset to vim defaults. So we
  " insist vimwiki folding here.
  if g:vimwiki_folding == 1 && &fdm != 'expr'
    setlocal fdm=expr
    setlocal foldexpr=VimwikiFoldLevel(v:lnum)
    setlocal foldtext=VimwikiFoldText()
  endif

  " And conceal level too.
  if g:vimwiki_conceallevel && exists("+conceallevel")
    let &conceallevel = g:vimwiki_conceallevel
  endif

  " Set up menu
  if g:vimwiki_menu != ""
    exe 'nmenu enable '.g:vimwiki_menu.'.Table'
  endif
endfunction "}}}

" OPTION get/set functions {{{
" return value of option for current wiki or if second parameter exists for
" wiki with a given index.
function! VimwikiGet(option, ...) "{{{
  if a:0 == 0
    let idx = g:vimwiki_current_idx
  else
    let idx = a:1
  endif
  if !has_key(g:vimwiki_list[idx], a:option) &&
        \ has_key(s:vimwiki_defaults, a:option)
    if a:option == 'path_html'
      let g:vimwiki_list[idx][a:option] =
            \VimwikiGet('path', idx)[:-2].'_html/'
    else
      let g:vimwiki_list[idx][a:option] =
            \s:vimwiki_defaults[a:option]
    endif
  endif

  " if path's ending is not a / or \
  " then add it
  if a:option == 'path' || a:option == 'path_html'
    let p = g:vimwiki_list[idx][a:option]
    " resolve doesn't work quite right with symlinks ended with / or \
    let p = substitute(p, '[/\\]\+$', '', '')
    let p = resolve(expand(p))
    let g:vimwiki_list[idx][a:option] = p.'/'
  endif

  return g:vimwiki_list[idx][a:option]
endfunction "}}}

" set option for current wiki or if third parameter exists for
" wiki with a given index.
function! VimwikiSet(option, value, ...) "{{{
  if a:0 == 0
    let idx = g:vimwiki_current_idx
  else
    let idx = a:1
  endif
  let g:vimwiki_list[idx][a:option] = a:value
endfunction "}}}
" }}}

" }}}

" CALLBACK function "{{{
" User can redefine it.
if !exists("*VimwikiWeblinkHandler") "{{{
  function VimwikiWeblinkHandler(weblink)
    for browser in g:vimwiki_browsers
      if executable(browser)
        if has("win32")
          execute '!start "'.browser.'" "' . a:weblink . '"'
        else
          execute '!'.browser.' "' . a:weblink . '"'
        endif
        return
      endif
    endfor
  endfunction
endif "}}}
" CALLBACK }}}

" DEFAULT wiki {{{
let s:vimwiki_defaults = {}
let s:vimwiki_defaults.path = '~/vimwiki/'
let s:vimwiki_defaults.path_html = '~/vimwiki_html/'
let s:vimwiki_defaults.css_name = 'style.css'
let s:vimwiki_defaults.index = 'index'
let s:vimwiki_defaults.ext = '.wiki'
let s:vimwiki_defaults.maxhi = 1
let s:vimwiki_defaults.syntax = 'default'

let s:vimwiki_defaults.template_path = '~/vimwiki/templates/'
let s:vimwiki_defaults.template_default = 'default'
let s:vimwiki_defaults.template_ext = '.html'

let s:vimwiki_defaults.nested_syntaxes = {}
let s:vimwiki_defaults.auto_export = 0
" is wiki temporary -- was added to g:vimwiki_list by opening arbitrary wiki
" file.
let s:vimwiki_defaults.temp = 0

" diary
let s:vimwiki_defaults.diary_rel_path = 'diary/'
let s:vimwiki_defaults.diary_index = 'diary'
let s:vimwiki_defaults.diary_header = 'Diary'

" Do not change this! Will wait till vim become more datetime awareable.
let s:vimwiki_defaults.diary_link_fmt = '%Y-%m-%d'

let s:vimwiki_defaults.diary_link_count = 4

"}}}

" DEFAULT options {{{
call s:default('list', [s:vimwiki_defaults])
if &encoding == 'utf-8'
  call s:default('upper', 'A-Z\u0410-\u042f')
  call s:default('lower', 'a-z\u0430-\u044f')
else
  call s:default('upper', 'A-Z')
  call s:default('lower', 'a-z')
endif
call s:default('other', '0-9')
call s:default('stripsym', '_')
call s:default('badsyms', '')
call s:default('auto_checkbox', 1)
call s:default('use_mouse', 0)
call s:default('folding', 0)
call s:default('fold_trailing_empty_lines', 0)
call s:default('fold_lists', 0)
call s:default('menu', 'Vimwiki')
call s:default('global_ext', 1)
call s:default('hl_headers', 0)
call s:default('hl_cb_checked', 0)
call s:default('camel_case', 1)
call s:default('list_ignore_newline', 1)
call s:default('listsyms', ' .oOX')
if has("win32")
  call s:default('browsers',
        \ [
        \  expand('~').'\Local Settings\Application Data\Google\Chrome\Application\chrome.exe',
        \  'C:\Program Files\Opera\opera.exe',
        \  'C:\Program Files\Mozilla Firefox\firefox.exe',
        \  'C:\Program Files\Internet Explorer\iexplore.exe',
        \ ])
else
  call s:default('browsers',
        \ [
        \  'chromium-browser',
        \  'opera',
        \  'firefox',
        \  'konqueror',
        \ ])
endif

call s:default('use_calendar', 1)
call s:default('table_auto_fmt', 1)
call s:default('w32_dir_enc', '')
call s:default('CJK_length', 0)
call s:default('dir_link', '')
call s:default('file_exts', 'pdf,txt,doc,rtf,xls,php,zip,rar,7z,html,gz')
call s:default('valid_html_tags', 'b,i,s,u,sub,sup,kbd,br,hr,div,center,strong,em')
call s:default('user_htmls', '')

call s:default('html_header_numbering', 0)
call s:default('html_header_numbering_sym', '')
call s:default('conceallevel', 3)

call s:default('current_idx', 0)

let upp = g:vimwiki_upper
let low = g:vimwiki_lower
let oth = g:vimwiki_other
let nup = low.oth
let nlo = upp.oth
let any = upp.nup

let wword = '\C\<['.upp.']['.nlo.']*['.low.']['.nup.']*['.upp.']['.any.']*\>'
let g:vimwiki_rxWikiWord = '!\@<!'.wword
let g:vimwiki_rxNoWikiWord = '!'.wword

let g:vimwiki_rxWikiLink1 = '\[\[[^\]]\+\]\]'
let g:vimwiki_rxWikiLink2 = '\[\[[^\]]\+\]\[[^\]]\+\]\]'
if g:vimwiki_camel_case
  let g:vimwiki_rxWikiLink = g:vimwiki_rxWikiWord.'\|'.
        \ g:vimwiki_rxWikiLink1.'\|'.g:vimwiki_rxWikiLink2
else
  let g:vimwiki_rxWikiLink = g:vimwiki_rxWikiLink1.'\|'.g:vimwiki_rxWikiLink2
endif
let g:vimwiki_rxWeblink = '\%("[^"(]\+\((\([^)]\+\))\)\?":\)\?'.
      \'\%('.
        \'\%('.
          \'\%(https\?\|ftp\|gopher\|telnet\|file\|notes\|ms-help\):'.
          \'\%(\%(//\)\|\%(\\\\\)\)'.
        \'\)'.
        \'\|\%(mailto:\)'.
      \'\)'.
      \'\+\S\+'.
      \'[().,?\]]\@<!'
"}}}

" AUTOCOMMANDS for all known wiki extensions {{{
" Getting all extensions that different wikies could have
let extensions = {}
for wiki in g:vimwiki_list
  if has_key(wiki, 'ext')
    let extensions[wiki.ext] = 1
  else
    let extensions['.wiki'] = 1
  endif
endfor

augroup filetypedetect
  " clear FlexWiki's stuff
  au! * *.wiki
augroup end

augroup vimwiki
  autocmd!
  for ext in keys(extensions)
    exe 'autocmd BufWinEnter *'.ext.' call s:setup_buffer_enter()'
    exe 'autocmd BufLeave,BufHidden *'.ext.' call s:setup_buffer_leave()'
    exe 'autocmd BufNewFile,BufRead, *'.ext.' call s:setup_filetype()'

    " ColorScheme could have or could have not a
    " VimwikiHeader1..VimwikiHeader6 highlight groups. We need to refresh
    " syntax after colorscheme change.
    exe 'autocmd ColorScheme *'.ext.' syntax enable'.
          \ ' | call vimwiki#base#highlight_links()'

    " Format tables when exit from insert mode. Do not use textwidth to
    " autowrap tables.
    if g:vimwiki_table_auto_fmt
      exe 'autocmd InsertLeave *'.ext.' call vimwiki#tbl#format(line("."))'
      exe 'autocmd InsertEnter *'.ext.' call vimwiki#tbl#reset_tw(line("."))'
    endif
  endfor
augroup END
"}}}

" COMMANDS {{{
command! VimwikiUISelect call vimwiki#base#ui_select()
command! -count VimwikiIndex
      \ call vimwiki#base#goto_index(v:count1)
command! -count VimwikiTabIndex tabedit <bar>
      \ call vimwiki#base#goto_index(v:count1)

command! -count VimwikiDiaryIndex
      \ call vimwiki#diary#goto_index(v:count1)
command! -count VimwikiMakeDiaryNote
      \ call vimwiki#diary#make_note(v:count1)
command! -count VimwikiTabMakeDiaryNote tabedit <bar>
      \ call vimwiki#diary#make_note(v:count1)
"}}}

" MAPPINGS {{{
if !hasmapto('<Plug>VimwikiIndex')
  nmap <silent><unique> <Leader>ww <Plug>VimwikiIndex
endif
nnoremap <unique><script> <Plug>VimwikiIndex :VimwikiIndex<CR>

if !hasmapto('<Plug>VimwikiTabIndex')
  nmap <silent><unique> <Leader>wt <Plug>VimwikiTabIndex
endif
nnoremap <unique><script> <Plug>VimwikiTabIndex :VimwikiTabIndex<CR>

if !hasmapto('<Plug>VimwikiUISelect')
  nmap <silent><unique> <Leader>ws <Plug>VimwikiUISelect
endif
nnoremap <unique><script> <Plug>VimwikiUISelect :VimwikiUISelect<CR>

if !hasmapto('<Plug>VimwikiDiaryIndex')
  nmap <silent><unique> <Leader>wi <Plug>VimwikiDiaryIndex
endif
nnoremap <unique><script> <Plug>VimwikiDiaryIndex :VimwikiDiaryIndex<CR>

if !hasmapto('<Plug>VimwikiMakeDiaryNote')
  nmap <silent><unique> <Leader>w<Leader>w <Plug>VimwikiMakeDiaryNote
endif
nnoremap <unique><script> <Plug>VimwikiMakeDiaryNote :VimwikiMakeDiaryNote<CR>

if !hasmapto('<Plug>VimwikiTabMakeDiaryNote')
  nmap <silent><unique> <Leader>w<Leader>t <Plug>VimwikiTabMakeDiaryNote
endif
nnoremap <unique><script> <Plug>VimwikiTabMakeDiaryNote
      \ :VimwikiTabMakeDiaryNote<CR>

"}}}

" MENU {{{
function! s:build_menu(topmenu)
  let idx = 0
  while idx < len(g:vimwiki_list)
    let norm_path = fnamemodify(VimwikiGet('path', idx), ':h:t')
    let norm_path = escape(norm_path, '\ \.')
    execute 'menu '.a:topmenu.'.Open\ index.'.norm_path.
          \ ' :call vimwiki#base#goto_index('.(idx + 1).')<CR>'
    execute 'menu '.a:topmenu.'.Open/Create\ diary\ note.'.norm_path.
          \ ' :call vimwiki#diary#make_note('.(idx + 1).')<CR>'
    let idx += 1
  endwhile
endfunction

function! s:build_table_menu(topmenu)
  exe 'menu '.a:topmenu.'.-Sep- :'
  exe 'menu '.a:topmenu.'.Table.Create\ (enter\ cols\ rows) :VimwikiTable '
  exe 'nmenu '.a:topmenu.'.Table.Format<tab>gqq gqq'
  exe 'nmenu '.a:topmenu.'.Table.Move\ column\ left<tab><A-Left> :VimwikiTableMoveColumnLeft<CR>'
  exe 'nmenu '.a:topmenu.'.Table.Move\ column\ right<tab><A-Right> :VimwikiTableMoveColumnRight<CR>'
  exe 'nmenu disable '.a:topmenu.'.Table'
endfunction

if !empty(g:vimwiki_menu)
  call s:build_menu(g:vimwiki_menu)
  call s:build_table_menu(g:vimwiki_menu)
endif
" }}}

" CALENDAR Hook "{{{
if g:vimwiki_use_calendar
  let g:calendar_action = 'vimwiki#diary#calendar_action'
  let g:calendar_sign = 'vimwiki#diary#calendar_sign'
endif
"}}}

let &cpo = s:old_cpo
