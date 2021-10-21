"=============================================================================
" Copyright (c) 2007-2009 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_autoload_acp') || v:version < 702
  finish
endif
let g:loaded_autoload_acp = 1

" }}}1
"=============================================================================
" GLOBAL FUNCTIONS: {{{1

"
function acp#enable()
  call acp#disable()

  augroup AcpGlobalAutoCommand
    autocmd!
    autocmd InsertEnter * unlet! s:posLast s:lastUncompletable
    autocmd InsertLeave * call s:finishPopup(1)
  augroup END

  if g:acp_mappingDriven
    call s:mapForMappingDriven()
  else
    autocmd AcpGlobalAutoCommand CursorMovedI * call s:feedPopup()
  endif

  nnoremap <silent> i i<C-r>=<SID>feedPopup()<CR>
  nnoremap <silent> a a<C-r>=<SID>feedPopup()<CR>
  nnoremap <silent> R R<C-r>=<SID>feedPopup()<CR>
endfunction

"
function acp#disable()
  call s:unmapForMappingDriven()
  augroup AcpGlobalAutoCommand
    autocmd!
  augroup END
  nnoremap i <Nop> | nunmap i
  nnoremap a <Nop> | nunmap a
  nnoremap R <Nop> | nunmap R
endfunction

"
function acp#lock()
  let s:lockCount += 1
endfunction

"
function acp#unlock()
  let s:lockCount -= 1
  if s:lockCount < 0
    let s:lockCount = 0
    throw "AutoComplPop: not locked"
  endif
endfunction

"
function acp#meetsForSnipmate(context)
  if g:acp_behaviorSnipmateLength < 0
    return 0
  endif
  let matches = matchlist(a:context, '\(^\|\s\|\<\)\(\u\{' .
        \                            g:acp_behaviorSnipmateLength . ',}\)$')
  return !empty(matches) && !empty(s:getMatchingSnipItems(matches[2]))
endfunction

"
function acp#meetsForKeyword(context)
  if g:acp_behaviorKeywordLength < 0
    return 0
  endif
  let matches = matchlist(a:context, '\(\k\{' . g:acp_behaviorKeywordLength . ',}\)$')
  if empty(matches)
    return 0
  endif
  for ignore in g:acp_behaviorKeywordIgnores
    if stridx(ignore, matches[1]) == 0
      return 0
    endif
  endfor
  return 1
endfunction

"
function acp#meetsForFile(context)
  if g:acp_behaviorFileLength < 0
    return 0
  endif
  if has('win32') || has('win64')
    let separator = '[/\\]'
  else
    let separator = '\/'
  endif
  if a:context !~ '\f' . separator . '\f\{' . g:acp_behaviorFileLength . ',}$'
    return 0
  endif
  return a:context !~ '[*/\\][/\\]\f*$\|[^[:print:]]\f*$'
endfunction

"
function acp#meetsForRubyOmni(context)
  if !has('ruby')
    return 0
  endif
  if g:acp_behaviorRubyOmniMethodLength >= 0 &&
        \ a:context =~ '[^. \t]\(\.\|::\)\k\{' .
        \              g:acp_behaviorRubyOmniMethodLength . ',}$'
    return 1
  endif
  if g:acp_behaviorRubyOmniSymbolLength >= 0 &&
        \ a:context =~ '\(^\|[^:]\):\k\{' .
        \              g:acp_behaviorRubyOmniSymbolLength . ',}$'
    return 1
  endif
  return 0
endfunction

"
function acp#meetsForPythonOmni(context)
  return has('python') && g:acp_behaviorPythonOmniLength >= 0 &&
        \ a:context =~ '\k\.\k\{' . g:acp_behaviorPythonOmniLength . ',}$'
endfunction

"
function acp#meetsForPerlOmni(context)
  return g:acp_behaviorPerlOmniLength >= 0 &&
        \ a:context =~ '\w->\k\{' . g:acp_behaviorPerlOmniLength . ',}$'
endfunction

"
function acp#meetsForXmlOmni(context)
  return g:acp_behaviorXmlOmniLength >= 0 &&
        \ a:context =~ '\(<\|<\/\|<[^>]\+ \|<[^>]\+=\"\)\k\{' .
        \              g:acp_behaviorXmlOmniLength . ',}$'
endfunction

"
function acp#meetsForHtmlOmni(context)
  return g:acp_behaviorHtmlOmniLength >= 0 &&
        \ a:context =~ '\(<\|<\/\|<[^>]\+ \|<[^>]\+=\"\)\k\{' .
        \              g:acp_behaviorHtmlOmniLength . ',}$'
endfunction

"
function acp#meetsForCssOmni(context)
  if g:acp_behaviorCssOmniPropertyLength >= 0 &&
        \ a:context =~ '\(^\s\|[;{]\)\s*\k\{' .
        \              g:acp_behaviorCssOmniPropertyLength . ',}$'
    return 1
  endif
  if g:acp_behaviorCssOmniValueLength >= 0 &&
        \ a:context =~ '[:@!]\s*\k\{' .
        \              g:acp_behaviorCssOmniValueLength . ',}$'
    return 1
  endif
  return 0
endfunction

"
function acp#completeSnipmate(findstart, base)
  if a:findstart
    let s:posSnipmateCompletion = len(matchstr(s:getCurrentText(), '.*\U'))
    return s:posSnipmateCompletion
  endif
  let lenBase = len(a:base)
  let items = filter(GetSnipsInCurrentScope(),
        \            'strpart(v:key, 0, lenBase) ==? a:base')
  return map(sort(items(items)), 's:makeSnipmateItem(v:val[0], v:val[1])')
endfunction

"
function acp#onPopupCloseSnipmate()
  let word = s:getCurrentText()[s:posSnipmateCompletion :]
  for trigger in keys(GetSnipsInCurrentScope())
    if word ==# trigger
      call feedkeys("\<C-r>=TriggerSnippet()\<CR>", "n")
      return 0
    endif
  endfor
  return 1
endfunction

"
function acp#onPopupPost()
  " to clear <C-r>= expression on command-line
  echo ''
  if pumvisible()
    inoremap <silent> <expr> <C-h> acp#onBs()
    inoremap <silent> <expr> <BS>  acp#onBs()
    " a command to restore to original text and select the first match
    return (s:behavsCurrent[s:iBehavs].command =~# "\<C-p>" ? "\<C-n>\<Up>"
          \                                                 : "\<C-p>\<Down>")
  endif
  let s:iBehavs += 1
  if len(s:behavsCurrent) > s:iBehavs 
    call s:setCompletefunc()
    return printf("\<C-e>%s\<C-r>=acp#onPopupPost()\<CR>",
          \       s:behavsCurrent[s:iBehavs].command)
  else
    let s:lastUncompletable = {
          \   'word': s:getCurrentWord(),
          \   'commands': map(copy(s:behavsCurrent), 'v:val.command')[1:],
          \ }
    call s:finishPopup(0)
    return "\<C-e>"
  endif
endfunction

"
function acp#onBs()
  " using "matchstr" and not "strpart" in order to handle multi-byte
  " characters
  if call(s:behavsCurrent[s:iBehavs].meets,
        \ [matchstr(s:getCurrentText(), '.*\ze.')])
    return "\<BS>"
  endif
  return "\<C-e>\<BS>"
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS: {{{1

"
function s:mapForMappingDriven()
  call s:unmapForMappingDriven()
  let s:keysMappingDriven = [
        \ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        \ 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        \ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        \ 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        \ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        \ '-', '_', '~', '^', '.', ',', ':', '!', '#', '=', '%', '$', '@', '<', '>', '/', '\',
        \ '<Space>', '<C-h>', '<BS>', ]
  for key in s:keysMappingDriven
    execute printf('inoremap <silent> %s %s<C-r>=<SID>feedPopup()<CR>',
          \        key, key)
  endfor
endfunction

"
function s:unmapForMappingDriven()
  if !exists('s:keysMappingDriven')
    return
  endif
  for key in s:keysMappingDriven
    execute 'iunmap ' . key
  endfor
  let s:keysMappingDriven = []
endfunction

"
function s:setTempOption(group, name, value)
  call extend(s:tempOptionSet[a:group], { a:name : eval('&' . a:name) }, 'keep')
  execute printf('let &%s = a:value', a:name)
endfunction

"
function s:restoreTempOptions(group)
  for [name, value] in items(s:tempOptionSet[a:group])
    execute printf('let &%s = value', name)
  endfor
  let s:tempOptionSet[a:group] = {}
endfunction

"
function s:getCurrentWord()
  return matchstr(s:getCurrentText(), '\k*$')
endfunction

"
function s:getCurrentText()
  return strpart(getline('.'), 0, col('.') - 1)
endfunction

"
function s:getPostText()
  return strpart(getline('.'), col('.') - 1)
endfunction

"
function s:isModifiedSinceLastCall()
  if exists('s:posLast')
    let posPrev = s:posLast
    let nLinesPrev = s:nLinesLast
    let textPrev = s:textLast
  endif
  let s:posLast = getpos('.')
  let s:nLinesLast = line('$')
  let s:textLast = getline('.')
  if !exists('posPrev')
    return 1
  elseif posPrev[1] != s:posLast[1] || nLinesPrev != s:nLinesLast
    return (posPrev[1] - s:posLast[1] == nLinesPrev - s:nLinesLast)
  elseif textPrev ==# s:textLast
    return 0
  elseif posPrev[2] > s:posLast[2]
    return 1
  elseif has('gui_running') && has('multi_byte')
    " NOTE: auto-popup causes a strange behavior when IME/XIM is working
    return posPrev[2] + 1 == s:posLast[2]
  endif
  return posPrev[2] != s:posLast[2]
endfunction

"
function s:makeCurrentBehaviorSet()
  let modified = s:isModifiedSinceLastCall()
  if exists('s:behavsCurrent[s:iBehavs].repeat') && s:behavsCurrent[s:iBehavs].repeat
    let behavs = [ s:behavsCurrent[s:iBehavs] ]
  elseif exists('s:behavsCurrent[s:iBehavs]')
    return []
  elseif modified
    let behavs = copy(exists('g:acp_behavior[&filetype]')
          \           ? g:acp_behavior[&filetype]
          \           : g:acp_behavior['*'])
  else
    return []
  endif
  let text = s:getCurrentText()
  call filter(behavs, 'call(v:val.meets, [text])')
  let s:iBehavs = 0
  if exists('s:lastUncompletable') &&
        \ stridx(s:getCurrentWord(), s:lastUncompletable.word) == 0 &&
        \ map(copy(behavs), 'v:val.command') ==# s:lastUncompletable.commands
    let behavs = []
  else
    unlet! s:lastUncompletable
  endif
  return behavs
endfunction

"
function s:feedPopup()
  " NOTE: CursorMovedI is not triggered while the popup menu is visible. And
  "       it will be triggered when popup menu is disappeared.
  if s:lockCount > 0 || pumvisible() || &paste
    return ''
  endif
  if exists('s:behavsCurrent[s:iBehavs].onPopupClose')
    if !call(s:behavsCurrent[s:iBehavs].onPopupClose, [])
      call s:finishPopup(1)
      return ''
    endif
  endif
  let s:behavsCurrent = s:makeCurrentBehaviorSet()
  if empty(s:behavsCurrent)
    call s:finishPopup(1)
    return ''
  endif
  " In case of dividing words by symbols (e.g. "for(int", "ab==cd") while a
  " popup menu is visible, another popup is not available unless input <C-e>
  " or try popup once. So first completion is duplicated.
  call insert(s:behavsCurrent, s:behavsCurrent[s:iBehavs])
  call s:setTempOption(s:GROUP0, 'spell', 0)
  call s:setTempOption(s:GROUP0, 'completeopt', 'menuone' . (g:acp_completeoptPreview ? ',preview' : ''))
  call s:setTempOption(s:GROUP0, 'complete', g:acp_completeOption)
  call s:setTempOption(s:GROUP0, 'ignorecase', g:acp_ignorecaseOption)
  " NOTE: With CursorMovedI driven, Set 'lazyredraw' to avoid flickering.
  "       With Mapping driven, set 'nolazyredraw' to make a popup menu visible.
  call s:setTempOption(s:GROUP0, 'lazyredraw', !g:acp_mappingDriven)
  " NOTE: 'textwidth' must be restored after <C-e>.
  call s:setTempOption(s:GROUP1, 'textwidth', 0)
  call s:setCompletefunc()
  call feedkeys(s:behavsCurrent[s:iBehavs].command . "\<C-r>=acp#onPopupPost()\<CR>", 'n')
  return '' " this function is called by <C-r>=
endfunction

"
function s:finishPopup(fGroup1)
  inoremap <C-h> <Nop> | iunmap <C-h>
  inoremap <BS>  <Nop> | iunmap <BS>
  let s:behavsCurrent = []
  call s:restoreTempOptions(s:GROUP0)
  if a:fGroup1
    call s:restoreTempOptions(s:GROUP1)
  endif
endfunction

"
function s:setCompletefunc()
  if exists('s:behavsCurrent[s:iBehavs].completefunc')
    call s:setTempOption(0, 'completefunc', s:behavsCurrent[s:iBehavs].completefunc)
  endif
endfunction

"
function s:makeSnipmateItem(key, snip)
  if type(a:snip) == type([])
    let descriptions = map(copy(a:snip), 'v:val[0]')
    let snipFormatted = '[MULTI] ' . join(descriptions, ', ')
  else
    let snipFormatted = substitute(a:snip, '\(\n\|\s\)\+', ' ', 'g')
  endif
  return  {
        \   'word': a:key,
        \   'menu': strpart(snipFormatted, 0, 80),
        \ }
endfunction

"
function s:getMatchingSnipItems(base)
  let key = a:base . "\n"
  if !exists('s:snipItems[key]')
    let s:snipItems[key] = items(GetSnipsInCurrentScope())
    call filter(s:snipItems[key], 'strpart(v:val[0], 0, len(a:base)) ==? a:base')
    call map(s:snipItems[key], 's:makeSnipmateItem(v:val[0], v:val[1])')
  endif
  return s:snipItems[key]
endfunction

" }}}1
"=============================================================================
" INITIALIZATION {{{1

let s:GROUP0 = 0
let s:GROUP1 = 1
let s:lockCount = 0
let s:behavsCurrent = []
let s:iBehavs = 0
let s:tempOptionSet = [{}, {}]
let s:snipItems = {}

" }}}1
"=============================================================================
" vim: set fdm=marker:
