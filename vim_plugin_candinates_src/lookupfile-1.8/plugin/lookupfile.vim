" lookupfile.vim: Lookup filenames by pattern
" Author: Hari Krishna Dara (hari.vim at gmail dot com)
" Last Change: 14-Jun-2007 @ 18:30
" Created:     11-May-2006
" Requires:    Vim-7.1, genutils.vim(2.3)
" Version:     1.8.0
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://www.vim.org//script.php?script_id=1581
" Usage:
"     See :help lookupfile.txt

if exists('loaded_lookupfile')
  finish
endif
if v:version < 701
  echomsg 'lookupfile: You need at least Vim 7.1'
  finish
endif
if !exists('loaded_genutils')
  runtime plugin/genutils.vim
endif
if !exists('loaded_genutils') || loaded_genutils < 203
  echomsg 'lookupfile: You need a newer version of genutils.vim plugin'
  finish
endif

let g:loaded_lookupfile = 108

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim

if !exists('g:LookupFile_TagExpr')
  let g:LookupFile_TagExpr = '&tags'
endif

if !exists('g:LookupFile_LookupFunc')
  let g:LookupFile_LookupFunc = ''
endif

if !exists('g:LookupFile_LookupNotifyFunc')
  let g:LookupFile_LookupNotifyFunc = ''
endif

if !exists('g:LookupFile_LookupAcceptFunc')
  let g:LookupFile_LookupAcceptFunc = ''
endif

if !exists('g:LookupFile_MinPatLength')
  let g:LookupFile_MinPatLength = 4
endif

if !exists('g:LookupFile_PreservePatternHistory')
  let g:LookupFile_PreservePatternHistory = 1
endif

if !exists('g:LookupFile_PreserveLastPattern')
  let g:LookupFile_PreserveLastPattern = 1
endif

if !exists('g:LookupFile_ShowFiller')
  let g:LookupFile_ShowFiller = 1
endif

if !exists('g:LookupFile_AlwaysAcceptFirst')
  let g:LookupFile_AlwaysAcceptFirst = 0
endif

if !exists('g:LookupFile_FileFilter')
  let g:LookupFile_FileFilter = ''
endif

if !exists('g:LookupFile_AllowNewFiles')
  let g:LookupFile_AllowNewFiles = 1
endif

if !exists('g:LookupFile_SortMethod')
  let g:LookupFile_SortMethod = 'alpha'
endif

if !exists('g:LookupFile_Bufs_BufListExpr')
  let g:LookupFile_Bufs_BufListExpr = ''
endif

if !exists('g:LookupFile_Bufs_SkipUnlisted')
  let g:LookupFile_Bufs_SkipUnlisted = 1
endif

if !exists('g:LookupFile_Bufs_LikeBufCmd')
  let g:LookupFile_Bufs_LikeBufCmd = 1
endif

if !exists('g:LookupFile_UsingSpecializedTags')
  let g:LookupFile_UsingSpecializedTags = 0
endif

if !exists('g:LookupFile_DefaultCmd')
  let g:LookupFile_DefaultCmd = ':LUTags'
endif

if !exists('g:LookupFile_EnableRemapCmd')
  let g:LookupFile_EnableRemapCmd = 1
endif

if !exists('g:LookupFile_DisableDefaultMap')
  let g:LookupFile_DisableDefaultMap = 0
endif

if !exists('g:LookupFile_UpdateTime')
  let g:LookupFile_UpdateTime = 300
endif

if !exists('g:LookupFile_OnCursorMovedI')
  let g:LookupFile_OnCursorMovedI = 0
endif

if !exists('g:LookupFile_EscCancelsPopup')
  let g:LookupFile_EscCancelsPopup = 1
endif

if !exists('g:LookupFile_SearchForBufsInTabs')
  let g:LookupFile_SearchForBufsInTabs = 1
endif

if !exists('g:LookupFile_TagsExpandCamelCase')
  let g:LookupFile_TagsExpandCamelCase = 1
endif

if !exists('g:LookupFile_RecentFileListSize')
  let g:LookupFile_RecentFileListSize = 20
endif

if (! exists("no_plugin_maps") || ! no_plugin_maps) &&
      \ (! exists("no_lookupfile_maps") || ! no_lookupfile_maps)
  noremap <script> <silent> <Plug>LookupFile :LookupFile<CR>

  if ! g:LookupFile_DisableDefaultMap
    if !hasmapto('<Plug>LookupFile', 'n')
      nmap <unique> <silent> <F5> <Plug>LookupFile
    endif
    if !hasmapto('<Plug>LookupFile', 'i')
      inoremap <Plug>LookupFileCE <C-E>
      imap <unique> <expr> <silent> <F5> (pumvisible() ? "\<Plug>LookupFileCE" :
            \ "")."\<Esc>\<Plug>LookupFile"
    endif
  endif
endif

command! -nargs=? -bang -complete=file LookupFile :call
      \ <SID>LookupUsing('lookupfile', "<bang>", <q-args>, 0)

command! -nargs=? -bang -complete=tag LUTags :call
      \ <SID>LookupUsing('Tags', "<bang>", <q-args>, 0)
command! -nargs=? -bang -complete=file LUPath :call
      \ <SID>LookupUsing('Path', "<bang>", <q-args>, g:LookupFile_MinPatLength)
command! -nargs=? -bang -complete=file LUArgs :call
      \ <SID>LookupUsing('Args', "<bang>", <q-args>, 0)
command! -nargs=? -bang -complete=file LUBufs :call
      \ <SID>LookupUsing('Bufs', "<bang>", <q-args>, 0)
command! -nargs=? -bang -complete=dir LUWalk :call
      \ <SID>LookupUsing('Walk', "<bang>", <q-args>, 0)

function! s:RemapLookupFile(cmd)
  let cmd = (a:cmd != '') ? a:cmd : ':LUTags'
  " It is not straight-forward to determine the right completion method.
  exec 'command! -nargs=? -bang -complete=file LookupFile' cmd
endfunction
call s:RemapLookupFile(g:LookupFile_DefaultCmd)

let s:mySNR = ''
function! s:SNR()
  if s:mySNR == ''
    let s:mySNR = matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
  endif
  return s:mySNR
endfun

let s:baseBufNr = 0
function! s:LookupUsing(ftr, bang, initPat, minPatLen)
  let cmd = ':LUTags'
  if a:ftr != 'Tags'
    call s:SaveSett('LookupFunc')
    call s:SaveSett('LookupNotifyFunc')
    call s:SaveSett('MinPatLength')
    unlet! g:LookupFile_LookupFunc g:LookupFile_LookupNotifyFunc
    let g:LookupFile_LookupFunc = function(s:SNR().'Lookup'.a:ftr)
    let g:LookupFile_LookupNotifyFunc = function(s:SNR().'LookupReset')
    let g:LookupFile_MinPatLength = a:minPatLen
    let s:baseBufNr = bufnr('%')
    let cmd = ':LU'.a:ftr
  endif
  if g:LookupFile_EnableRemapCmd
    call s:RemapLookupFile(cmd)
  endif
  call lookupfile#OpenWindow(a:bang, a:initPat)

  if exists('*s:Config'.a:ftr)
    call s:Config{a:ftr}()
  endif

  aug LookupReset
    au!
    au BufHidden <buffer> call <SID>LookupReset()
  aug END
endfunction

function! s:LookupReset()
  if exists('s:saved')
    for sett in keys(s:saved)
      unlet! g:LookupFile_{sett}
      let g:LookupFile_{sett} = s:saved[sett]
    endfor
    unlet s:saved
  endif
  if exists('s:cleanup')
    for cmd in s:cleanup
      try
        exec cmd
      catch
        echoerr v:exception . ', while executing cleanup command: ' . cmd
      endtry
    endfor
    unlet s:cleanup
  endif
  aug ConfigIdo
    au!
  aug END
endfunction

function! s:SaveSett(sett)
  if !exists('s:saved')
    let s:saved = {}
  endif
  " Avoid overwriting the original value.
  if !has_key(s:saved, a:sett)
    let s:saved[a:sett] = g:LookupFile_{a:sett}
  endif
endfunction

function! s:AddCleanup(cmd)
  if !exists('s:cleanup')
    let s:cleanup = []
  endif
  if index(s:cleanup, a:cmd) == -1
    call add(s:cleanup, a:cmd)
  endif
endfunction

function! s:LookupPath(pattern)
  let filePat = a:pattern
  let matchingExactCase = s:MatchingExactCase(filePat)
  " Remove leading or trailing '*'s as we add a star anyway. This also removes
  " '**' unless it is followed by a slash.
  let filePat = substitute(filePat, '^\*\+\|\*\+$', '', 'g')
  " On windows, the case is anyway ignored.
  if !genutils#OnMS() && !matchingExactCase
    let filePat = s:FilePatIgnoreCase(filePat)
  endif
  let fl = split(globpath(&path, (filePat != '') ? '*'.filePat.'*' : '*'),
        \ "\n")
  let regexPat = s:TranslateFileRegex(filePat)
  " This is a psuedo case-sensitive match for windows, when 'smartcase' is
  " set.
  if genutils#OnMS() && matchingExactCase
    set verbose=15
    call filter(fl, 'v:val =~# regexPat')
    set verbose=0
  endif
  return map(fl,
        \ '{'.
        \ ' "word": v:val,'.
        \ ' "abbr": fnamemodify(v:val, ":t"), '.
        \ ' "menu": fnamemodify(v:val, ":h"), '.
        \ ' "dup": 1'.
        \ '}')
endfunction

function! s:LookupArgs(pattern)
  return map(filter(argv(), 'v:val =~ a:pattern'),
        \ '{'.
        \ ' "word":fnamemodify(v:val, ":p"), '.
        \ ' "abbr": v:val, '.
        \ ' "menu": substitute(v:val, a:pattern, "[&]", ""), '.
        \ ' "dup": 1'.
        \ '}')
endfunction

let s:bufList = [1]
function! s:LookupBufs(pattern)
  let results = []

  if g:LookupFile_Bufs_BufListExpr != ''
    let buflist = eval(g:LookupFile_Bufs_BufListExpr)
  else
    " Since we need to generate the same range again and again, it is better to
    " cache the list.
    if s:bufList[-1] != bufnr('$')
      call extend(s:bufList, range(s:bufList[-1], bufnr('$')))
    endif
    let buflist = s:bufList
  endif
  let lastBufNr = bufnr('$')
  let i = 1
  if g:LookupFile_Bufs_LikeBufCmd
    let pattern = s:TranslateFileRegex(a:pattern)
  else
    let pattern = a:pattern
  endif
  for bufNr in buflist
    if ! bufexists(bufNr)
      call remove(buflist, i)
      continue
    endif
    try
      if g:LookupFile_Bufs_SkipUnlisted && ! buflisted(bufNr)
        continue
      endif
      let fname = expand('#'.bufNr.':p')
      if g:LookupFile_Bufs_LikeBufCmd
        let bname = bufname(bufNr)
        let dir = ''
      else
        let bname = fnamemodify(bufname(bufNr), ':t')
        let dir = fnamemodify(bufname(bufNr), ':h').'/'
      endif
      if bname =~ pattern
        call add(results, {
              \ 'word': fname,
              \ 'abbr': bname,
              \ 'menu': dir.substitute(bname, pattern, '[&]', ''),
              \ 'dup': 1,
              \ })
      endif
    finally
      let i = i + 1
    endtry
  endfor
  return results
endfunction

function! s:LookupWalk(pattern)
  " We will wait till '/' is typed
  if a:pattern =~ '\*\*$'
    return []
  endif
  let showOnlyDirs = 0
  " Determine the parent dir.
  if a:pattern =~ '//$'
    let parent = strpart(a:pattern, 0, strlen(a:pattern)-1)
    let filePat = ''
    if parent ==# g:lookupfile#lastPattern
      return filter(g:lookupfile#lastResults, 'v:val["kind"] == "/"')
    endif
    let showOnlyDirs = 1
  else
    let parent = matchstr(a:pattern, '^.*/')
    let filePat = strpart(a:pattern, len(parent))
  endif

  let matchingExactCase = s:MatchingExactCase(filePat)

  " Remove leading or trailing '*'s as we add a star anyway. This also makes
  " '**' as '', but we rule this case out already.
  let filePat = substitute(filePat, '^\*\+\|\*\+$', '', 'g')
  " On windows, the case is anyway ignored.
  if !genutils#OnMS() && !matchingExactCase
    let filePat = s:FilePatIgnoreCase(filePat)
  endif
  "exec BPBreak(1)
  let _shellslash = &shellslash
  set shellslash
  try
    let files = glob(parent.((filePat != '') ? '*'.filePat.'*' : '*'))
  catch
    " Ignore errors in patterns.
    let files = ''
  finally
    let &shellslash = _shellslash
  endtry
  let fl = split(files, "\<NL>")
  let regexPat = s:TranslateFileRegex(filePat)
  " This is a psuedo case-sensitive match for windows, when 'smartcase' is
  " set.
  if genutils#OnMS() && matchingExactCase
    call filter(fl, 'fnamemodify(v:val, ":t") =~# regexPat')
  endif
  " Find the start of path component that uses any of the *, [], ? or {
  " wildcard. Path until this is unambiguously common to all, so we can strip
  " it off, for brevity.
  let firstWildIdx = match(a:pattern, '[^/]*\%(\*\|\[\|?\|{\)')
  return s:FormatFileResults(fl, firstWildIdx!=-1 ? firstWildIdx :
        \ strlen(parent), regexPat, matchingExactCase, showOnlyDirs)
endfunction

function! s:FormatFileResults(fl, parentLen, matchPat, matchingCase, dirsOnly)
  let entries = []
  for f in a:fl
    if isdirectory(f)
      let suffx = '/'
    else
      if a:dirsOnly
        continue
      endif
      let suffx = ''
    endif
    let word = f.suffx
    let fname = matchstr(f, '[^/]*$')
    let dir = fnamemodify(f, ':h').'/'
    if dir != '/' && a:parentLen != -1
      let dir = strpart(dir, a:parentLen)
    else
      let dir = ''
    endif
    "let dir = (dir == '/'?'':dir)
    call add(entries, {
          \ 'word': word,
          \ 'abbr': fname.suffx,
          \ 'menu': (a:matchPat!='') ? dir.substitute(fname,
          \   (a:matchingCase?'\C':'\c').a:matchPat, '[&]', '') :
          \    dir.fname,
          \ 'kind': suffx,
          \ 'dup': 1
          \ })
  endfor
  return entries
endfunction

function! s:ConfigBufs()
  " Allow switching to file mode.
  inoremap <expr> <buffer> <C-F> <SID>IdoSwitchTo('file')
  call s:AddCleanup('iunmap <buffer> <C-F>')
  if g:LookupFile_Bufs_BufListExpr != ''
    call s:SaveSett('SortMethod')
    let g:LookupFile_SortMethod = ''
  endif
endfunction

function! s:ConfigWalk()
  call s:SaveSett('LookupAcceptFunc')
  unlet! g:LookupFile_LookupAcceptFunc
  let g:LookupFile_LookupAcceptFunc = function(s:SNR().'IdoAccept')
  " Make sure we have the right slashes, in case user passed in init path
  " with wrong slashes.
  call setline('.', substitute(getline('.'), '\\', '/', 'g'))

  inoremap <buffer> <expr> <BS> <SID>IdoBS()
  inoremap <buffer> <expr> <S-BS> <SID>IdoBS()
  call s:AddCleanup('iunmap <buffer> <BS>')
  imap <buffer> <expr> <Tab> <SID>IdoTab()
  call s:AddCleanup('iunmap <buffer> <Tab>')
  inoremap <expr> <buffer> <C-B> <SID>IdoSwitchTo('buffer')
  call s:AddCleanup('iunmap <buffer> <C-B>')
endfunction

function! s:IdoSwitchTo(mode)
  call s:LookupReset()
  if a:mode == 'buffer'
    let tok = matchstr(getline('.'), '[^/]*$')
    let cmd = 'LUBufs'.(tok == "" ? '!' : ' '.tok)
  else
    let cmd = 'LUWalk '.s:GetDefDir().getline('.')
  endif
  return (pumvisible()?"\<C-E>":'')."\<Esc>:".cmd."\<CR>"
endfunction

function! s:IdoAccept(splitWin, key)
  let refreshCmd = "\<C-O>:call lookupfile#LookupFile(0)\<CR>\<C-O>:\<BS>"
  if getline('.') !=# g:lookupfile#lastPattern && getline('.')[strlen(getline('.'))-1] == '/'
    return refreshCmd
  elseif getline('.') ==# g:lookupfile#lastPattern
        \ && len(g:lookupfile#lastResults) > 0
        \ && g:lookupfile#lastResults[0]['kind'] == '/'
    " When the first entry is a directory, accept it, and trigger a fresh
    " completion on that.
    return "\<C-N>\<C-R>=(getline('.') == lookupfile#lastPattern)?\"\\<C-N>\":''\<CR>".refreshCmd
  endif
  return lookupfile#AcceptFile(a:splitWin, a:key)
endfunction

function! s:IdoBS()
  if lookupfile#IsPopupHidden() == 1
    return "\<BS>"
  endif
  if getline('.') !~ '/$'
    return (pumvisible() ? "\<C-E>" : '')."\<BS>"
  else
    " Determine the number of <BS>'s required to remove the patch component.
    let lastComp = matchstr(getline('.'), '[^/]*/$')
    return (pumvisible() ? (getline('.') ==# g:lookupfile#lastPattern ?
          \ "\<C-E>" : "\<C-Y>") : '') . repeat("\<BS>", strlen(lastComp))
  endif
endfunction

function! s:IdoTab()
  " When no pattern yet, fill up with current directory.
  if !pumvisible() && getline('.') == ''
    return s:GetDefDir()
  else
    return "\<Tab>"
  endif
endfunction

function! s:GetDefDir()
  return substitute(expand('#'.s:baseBufNr.':p:h'), '\\', '/', 'g').'/'
endfunction

" Convert file wildcards ("*", "?" etc. |file-pattern|) to a Vim string
"   regex metachars (see |pattern.txt|). Returns metachars that work in "very
"   nomagic" mode.
let s:fileWild = {}
function! s:TranslateFileWild(fileWild)
  let strRegex = ''
  if a:fileWild ==# '*'
    let strRegex = '\[^/]\*'
  elseif a:fileWild ==# '**'
    let strRegex = '\.\*'
  elseif a:fileWild ==# '?'
    let strRegex = '\.'
  elseif a:fileWild ==# '['
    let strRegex = '\['
  endif
  return strRegex
endfunction

" Convert a |file-pattern| to a Vim string regex (see |pattern.txt|).
"   No error checks for now, for simplicity.
function! s:TranslateFileRegex(filePat)
  let pat = substitute(a:filePat, '\(\*\*\|\*\|\[\)',
        \ '\=s:TranslateFileWild(submatch(1))', 'g')
  let unprotectedMeta = genutils#CrUnProtectedCharsPattern('?,', 1)
  let pat = substitute(pat, unprotectedMeta,
        \ '\=s:TranslateFileWild(submatch(1))', 'g')
  return (pat == '') ? pat : '\V'.pat
endfunction
 
" Translates the file pattern to ignore case on non-case-insensitive systems.
function! s:FilePatIgnoreCase(filePat)
  return substitute(a:filePat, '\(\[.\{-}]\)\|\(\a\)',
        \ '\=s:TranslateAlpha(submatch(0))', 'g')
endfunction

function! s:TranslateAlpha(pat)
  if a:pat =~"^["
    return substitute(substitute(a:pat, '-\@<!\a-\@!', '&\u&', 'g'),
          \ '\(\a\)-\(\a\)', '\1-\2\u\1-\u\2', 'g')
  else
    return substitute(a:pat, '\a', '[\l&\u&]', 'g')
  endif
endfunction

function! s:MatchingExactCase(filePat)
  if &ignorecase
    if &smartcase && a:filePat =~# '\u'
      let matchingExactCase = 1
    else
      let matchingExactCase = 0
    endif
  else
    if genutils#OnMS()
      let matchingExactCase = 0
    else
      let matchingExactCase = 1
    endif
  endif
  return matchingExactCase
endfunction

" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker et sw=2
