"=============================================================================
" Copyright (c) 2007-2009 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_autoload_fuf') || v:version < 702
  finish
endif
let g:loaded_autoload_fuf = 1

" }}}1
"=============================================================================
" GLOBAL FUNCTIONS {{{1



function fuf#getPathSeparator()
  return (!&shellslash && (has('win32') || has('win64')) ? '\' : '/')
endfunction

" Removes duplicates
" this function doesn't change list of argument.
function fuf#unique(items)
  let sorted = sort(a:items)
  if len(sorted) < 2
    return sorted
  endif
  let last = remove(sorted, 0)
  let result = [last]
  for item in sorted
    if item != last
      call add(result, item)
      let last = item
    endif
  endfor
  return result
endfunction

" [ [0], [1,2], [3] ] -> [ 0, 1, 2, 3 ]
" this function doesn't change list of argument.
function fuf#concat(items)
  let result = []
  for l in a:items
    let result += l
  endfor
  return result
endfunction

" filter() with the maximum number of items
" this function doesn't change list of argument.
function fuf#filterWithLimit(items, expr, limit)
  if a:limit <= 0
    return filter(copy(a:items), a:expr)
  endif
  let result = []
  let stride = a:limit * 3 / 2 " x1.5
  for i in range(0, len(a:items) - 1, stride)
    let result += filter(a:items[i : i + stride - 1], a:expr)
    if len(result) >= a:limit
      return remove(result, 0, a:limit - 1)
    endif
  endfor
  return result
endfunction

"
function fuf#countModifiedFiles(files, time)
  return len(filter(copy(a:files), 'getftime(v:val) > a:time'))
endfunction

"
function fuf#getCurrentTagFiles()
  return sort(filter(map(tagfiles(), 'fnamemodify(v:val, '':p'')'), 'filereadable(v:val)'))
endfunction

"
function fuf#mapToSetSerialIndex(in, offset)
  for i in range(len(a:in))
    let a:in[i].index = i + a:offset
  endfor
  return a:in
endfunction

"
function fuf#updateMruList(mrulist, newItem, maxItem, exclude)
  let result = copy(a:mrulist)
  let result = filter(result,'v:val.word != a:newItem.word')
  let result = insert(result, a:newItem)
  let result = filter(result, 'v:val.word !~ a:exclude')
  return result[0 : a:maxItem - 1]
endfunction

" takes suffix number. if no digits, returns -1
function fuf#suffixNumber(str)
  let s = matchstr(a:str, '\d\+$')
  return (len(s) ? str2nr(s) : -1)
endfunction

" "foo/bar/buz/hoge" -> { head: "foo/bar/buz/", tail: "hoge" }
function fuf#splitPath(path)
  let head = matchstr(a:path, '^.*[/\\]')
  return  {
        \   'head' : head,
        \   'tail' : a:path[strlen(head):]
        \ }
endfunction

" "foo/.../bar/...hoge" -> "foo/.../bar/../../hoge"
function fuf#expandTailDotSequenceToParentDir(pattern)
  return substitute(a:pattern, '^\(.*[/\\]\)\?\zs\.\(\.\+\)\ze[^/\\]*$',
        \           '\=repeat(".." . fuf#getPathSeparator(), len(submatch(2)))', '')
endfunction

"
function fuf#hash224(str)
  let a = 0x00000800 " shift 11 bit 
  let b = 0x001fffff " extract 11 bit
  let nHash = 7
  let hashes = repeat([0], nHash)
  for i in range(len(a:str))
    let iHash = i % nHash
    let hashes[iHash] = hashes[iHash] * a + hashes[iHash] / b
    let hashes[iHash] += char2nr(a:str[i])
  endfor
  return join(map(hashes, 'printf("%08x", v:val)'), '')
endfunction

"
function fuf#formatPrompt(prompt, partialMatching)
  let indicator = (a:partialMatching ? '!' : '')
  return substitute(a:prompt, '[]', indicator, 'g')
endfunction

"
function fuf#getFileLines(file)
  let bufnr = (type(a:file) ==# type(0) ? a:file : bufnr('^' . a:file . '$'))
  let lines = getbufline(bufnr, 1, '$')
  if !empty(lines)
    return lines
  endif
  try
    return readfile(expand(a:file))
  catch /.*/ 
  endtry
  return []
endfunction

"
function fuf#makePreviewLinesAround(lines, indices, page, maxHeight)
  let index = ((empty(a:indices) ? 0 : a:indices[0])
        \ + a:page * a:maxHeight) % len(a:lines)
  if empty(a:lines) || a:maxHeight <= 0
    return []
  endif
  let beg = max([0, index - a:maxHeight / 2])
  let end = min([beg + a:maxHeight, len(a:lines)])
  let beg = max([0, end - a:maxHeight])
  let lines = []
  for i in range(beg, end - 1)
    let mark = (count(a:indices, i) ? '>' : ' ')
    call add(lines, printf('%s%4d ', mark, i + 1) . a:lines[i])
  endfor
  return lines
endfunction

" a:file: a path string or a buffer number
function fuf#makePreviewLinesForFile(file, count, maxHeight)
  let lines = fuf#getFileLines(a:file)
  if empty(lines)
    return []
  endif
  let bufnr = (type(a:file) ==# type(0) ? a:file : bufnr('^' . a:file . '$'))
  if exists('s:bufferCursorPosMap[bufnr]')
    let indices = [s:bufferCursorPosMap[bufnr][1] - 1]
  else
    let indices = []
  endif
  return fuf#makePreviewLinesAround(
        \ lines, indices, a:count, a:maxHeight)
endfunction

"
function fuf#echoWithHl(msg, hl)
  execute "echohl " . a:hl
  echo a:msg
  echohl None
endfunction

"
function fuf#inputHl(prompt, text, hl)
  execute "echohl " . a:hl
  let s = input(a:prompt, a:text)
  echohl None
  return s
endfunction

"
function fuf#openBuffer(bufNr, mode, reuse)
  if a:reuse && ((a:mode == s:OPEN_TYPE_SPLIT &&
        \         s:moveToWindowOfBufferInCurrentTabPage(a:bufNr)) ||
        \        (a:mode == s:OPEN_TYPE_VSPLIT &&
        \         s:moveToWindowOfBufferInCurrentTabPage(a:bufNr)) ||
        \        (a:mode == s:OPEN_TYPE_TAB &&
        \         s:moveToWindowOfBufferInOtherTabPage(a:bufNr)))
    return
  endif
  execute printf({
        \   s:OPEN_TYPE_CURRENT : '%sbuffer'          ,
        \   s:OPEN_TYPE_SPLIT   : '%ssbuffer'         ,
        \   s:OPEN_TYPE_VSPLIT  : 'vertical %ssbuffer',
        \   s:OPEN_TYPE_TAB     : 'tab %ssbuffer'     ,
        \ }[a:mode], a:bufNr)
endfunction

"
function fuf#openFile(path, mode, reuse)
  let bufNr = bufnr('^' . a:path . '$')
  if bufNr > -1
    call fuf#openBuffer(bufNr, a:mode, a:reuse)
  else
    execute {
          \   s:OPEN_TYPE_CURRENT : 'edit '   ,
          \   s:OPEN_TYPE_SPLIT   : 'split '  ,
          \   s:OPEN_TYPE_VSPLIT  : 'vsplit ' ,
          \   s:OPEN_TYPE_TAB     : 'tabedit ',
          \ }[a:mode] . fnameescape(fnamemodify(a:path, ':~:.'))
  endif
endfunction

"
function fuf#openTag(tag, mode)
  execute {
        \   s:OPEN_TYPE_CURRENT : 'tjump '          ,
        \   s:OPEN_TYPE_SPLIT   : 'stjump '         ,
        \   s:OPEN_TYPE_VSPLIT  : 'vertical stjump ',
        \   s:OPEN_TYPE_TAB     : 'tab stjump '     ,
        \ }[a:mode] . a:tag
endfunction

"
function fuf#openHelp(tag, mode)
  execute {
        \   s:OPEN_TYPE_CURRENT : 'help '         ,
        \   s:OPEN_TYPE_SPLIT   : 'help '         ,
        \   s:OPEN_TYPE_VSPLIT  : 'vertical help ',
        \   s:OPEN_TYPE_TAB     : 'tab help '   ,
        \ }[a:mode] . a:tag
endfunction

"
function fuf#prejump(mode)
  execute {
        \   s:OPEN_TYPE_CURRENT : ''         ,
        \   s:OPEN_TYPE_SPLIT   : 'split'    ,
        \   s:OPEN_TYPE_VSPLIT  : 'vsplit'   ,
        \   s:OPEN_TYPE_TAB     : 'tab split',
        \ }[a:mode] 
endfunction

"
function fuf#compareRanks(i1, i2)
  if exists('a:i1.ranks') && exists('a:i2.ranks')
    for i in range(min([len(a:i1.ranks), len(a:i2.ranks)]))
      if     a:i1.ranks[i] > a:i2.ranks[i]
        return +1
      elseif a:i1.ranks[i] < a:i2.ranks[i]
        return -1
      endif
    endfor
  endif
  return 0
endfunction

"
function fuf#makePathItem(fname, menu, appendsDirSuffix)
  let pathPair = fuf#splitPath(a:fname)
  let dirSuffix = (a:appendsDirSuffix && isdirectory(a:fname)
        \          ? fuf#getPathSeparator()
        \          : '')
  return {
        \   'word'              : a:fname . dirSuffix,
        \   'wordForPrimaryHead': s:toLowerForIgnoringCase(pathPair.head),
        \   'wordForPrimaryTail': s:toLowerForIgnoringCase(pathPair.tail),
        \   'wordForBoundary'   : s:toLowerForIgnoringCase(s:getWordBoundaries(pathPair.tail)),
        \   'wordForRefining'   : s:toLowerForIgnoringCase(a:fname . dirSuffix),
        \   'wordForRank'       : s:toLowerForIgnoringCase(pathPair.tail),
        \   'menu'              : a:menu,
        \ }
endfunction

"
function fuf#makeNonPathItem(word, menu)
  let wordL = s:toLowerForIgnoringCase(a:word)
  return {
        \   'word'           : a:word,
        \   'wordForPrimary' : wordL,
        \   'wordForBoundary': s:toLowerForIgnoringCase(s:getWordBoundaries(a:word)),
        \   'wordForRefining': wordL,
        \   'wordForRank'    : wordL,
        \   'menu'           : a:menu,
        \ }
endfunction

"
function s:interpretPrimaryPatternForPathTail(pattern)
  let pattern = fuf#expandTailDotSequenceToParentDir(a:pattern)
  let pairL = fuf#splitPath(s:toLowerForIgnoringCase(pattern))
  return {
        \   'primary'       : pattern,
        \   'primaryForRank': pairL.tail,
        \   'matchingPairs' : [['v:val.wordForPrimaryTail', pairL.tail],],
        \ }
endfunction

"
function s:interpretPrimaryPatternForPath(pattern)
  let pattern = fuf#expandTailDotSequenceToParentDir(a:pattern)
  let patternL = s:toLowerForIgnoringCase(pattern)
  let pairL = fuf#splitPath(patternL)
  if g:fuf_splitPathMatching
    let matches = [
        \     ['v:val.wordForPrimaryHead', pairL.head],
        \     ['v:val.wordForPrimaryTail', pairL.tail],
        \   ]
  else
    let matches = [
          \     ['v:val.wordForPrimaryHead . v:val.wordForPrimaryTail', patternL],
          \   ]
  endif
  return {
        \   'primary'       : pattern,
        \   'primaryForRank': pairL.tail,
        \   'matchingPairs' : matches,
        \ }
endfunction

"
function s:interpretPrimaryPatternForNonPath(pattern)
  let patternL = s:toLowerForIgnoringCase(a:pattern)
  return {
        \   'primary'       : a:pattern,
        \   'primaryForRank': patternL,
        \   'matchingPairs' : [['v:val.wordForPrimary', patternL],],
        \ }
endfunction

"
function fuf#makePatternSet(patternBase, interpreter, partialMatching)
  let MakeMatchingExpr = function(a:partialMatching
        \                         ? 's:makePartialMatchingExpr'
        \                         : 's:makeFuzzyMatchingExpr')
  let [primary; refinings] = split(a:patternBase, g:fuf_patternSeparator, 1)
  let elements = call(a:interpreter, [primary])
  let primaryExprs  = map(elements.matchingPairs, 'MakeMatchingExpr(v:val[0], v:val[1])')
  let refiningExprs = map(refinings, 's:makeRefiningExpr(v:val)')
  return  {
        \   'primary'       : elements.primary,
        \   'primaryForRank': elements.primaryForRank,
        \   'filteringExpr' : join(primaryExprs + refiningExprs, ' && '),
        \ }
endfunction

"
function fuf#enumExpandedDirsEntries(dir, exclude)
  " Substitutes "\" because on Windows, "**\" doesn't include ".\",
  " but "**/" include "./". I don't know why.
  let dirNormalized = substitute(a:dir, '\', '/', 'g')
  let entries = split(glob(dirNormalized . "*" ), "\n") +
        \       split(glob(dirNormalized . ".*"), "\n")
  " removes "*/." and "*/.."
  call filter(entries, 'v:val !~ ''\v(^|[/\\])\.\.?$''')
  call map(entries, 'fuf#makePathItem(v:val, "", 1)')
  if len(a:exclude)
    call filter(entries, 'v:val.word !~ a:exclude')
  endif
  return entries
endfunction

"
function fuf#mapToSetAbbrWithSnippedWordAsPath(items)
  let maxLenStats = {}
  call map(a:items, 's:makeFileAbbrInfo(v:val, maxLenStats)')
  let snippedHeads =
        \ map(maxLenStats, 's:getSnippedHead(v:key[: -2], v:val)')
  return map(a:items, 's:setAbbrWithFileAbbrData(v:val, snippedHeads)')
endfunction

"
function fuf#setAbbrWithFormattedWord(item, abbrIndex)
  let lenMenu = (exists('a:item.menu') ? len(a:item.menu) + 2 : 0)
  let abbrPrefix = (exists('a:item.abbrPrefix') ? a:item.abbrPrefix : '')
  let a:item.abbr = abbrPrefix . a:item.word
  if a:abbrIndex
    let a:item.abbr = printf('%4d: ', a:item.index) . a:item.abbr
  endif
  let a:item.abbr = s:snipTail(a:item.abbr, g:fuf_maxMenuWidth - lenMenu, s:ABBR_SNIP_MASK)
  return a:item
endfunction

"
function fuf#defineLaunchCommand(CmdName, modeName, prefixInitialPattern)
  execute printf('command! -bang -narg=? %s call fuf#launch(%s, %s . <q-args>, len(<q-bang>))',
        \        a:CmdName, string(a:modeName), a:prefixInitialPattern)
endfunction

"
function fuf#defineKeyMappingInHandler(key, func)
    " hacks to be able to use feedkeys().
    execute printf(
          \ 'inoremap <buffer> <silent> %s <C-r>=fuf#getRunningHandler().%s ? "" : ""<CR>',
          \ a:key, a:func)
endfunction

"
function fuf#launch(modeName, initialPattern, partialMatching)
  if exists('s:runningHandler')
    call fuf#echoWithHl('FuzzyFinder is running.', 'WarningMsg')
  endif
  if count(g:fuf_modes, a:modeName) == 0
    echoerr 'This mode is not available: ' . a:modeName
    return
  endif
  let s:runningHandler = fuf#{a:modeName}#createHandler(copy(s:handlerBase))
  let s:runningHandler.info = fuf#loadInfoFile(s:runningHandler.getModeName())
  let s:runningHandler.partialMatching = a:partialMatching
  let s:runningHandler.bufNrPrev = bufnr('%')
  let s:runningHandler.lastCol = -1
  call s:runningHandler.onModeEnterPre()
  call s:setTemporaryGlobalOption('completeopt', 'menuone')
  call s:setTemporaryGlobalOption('ignorecase', 0)
  if s:runningHandler.getPreviewHeight() > 0
    call s:setTemporaryGlobalOption(
          \ 'cmdheight', s:runningHandler.getPreviewHeight() + 1)
  endif
  call s:activateFufBuffer()
  augroup FufLocal
    autocmd!
    autocmd CursorMovedI <buffer>        call s:runningHandler.onCursorMovedI()
    autocmd InsertLeave  <buffer> nested call s:runningHandler.onInsertLeave()
  augroup END
  for [key, func] in [
        \   [ g:fuf_keyOpen          , 'onCr(' . s:OPEN_TYPE_CURRENT . ', 0)' ],
        \   [ g:fuf_keyOpenSplit     , 'onCr(' . s:OPEN_TYPE_SPLIT   . ', 0)' ],
        \   [ g:fuf_keyOpenVsplit    , 'onCr(' . s:OPEN_TYPE_VSPLIT  . ', 0)' ],
        \   [ g:fuf_keyOpenTabpage   , 'onCr(' . s:OPEN_TYPE_TAB     . ', 0)' ],
        \   [ '<BS>'                 , 'onBs()'                               ],
        \   [ '<C-h>'                , 'onBs()'                               ],
        \   [ g:fuf_keyPreview       , 'onPreviewBase()'                      ],
        \   [ g:fuf_keyNextMode      , 'onSwitchMode(+1)'                     ],
        \   [ g:fuf_keyPrevMode      , 'onSwitchMode(-1)'                     ],
        \   [ g:fuf_keySwitchMatching, 'onSwitchMatching()'                   ],
        \   [ g:fuf_keyPrevPattern   , 'onRecallPattern(+1)'                  ],
        \   [ g:fuf_keyNextPattern   , 'onRecallPattern(-1)'                  ],
        \ ]
    call fuf#defineKeyMappingInHandler(key, func)
  endfor
  " Starts Insert mode and makes CursorMovedI event now. Command prompt is
  " needed to forces a completion menu to update every typing.
  call setline(1, s:runningHandler.getPrompt() . a:initialPattern)
  call s:runningHandler.onModeEnterPost()
  call feedkeys("A", 'n') " startinsert! does not work in InsertLeave event handler
  redraw
endfunction

"
function fuf#loadInfoFile(modeName)
  try
    let lines = readfile(expand(g:fuf_infoFile))
    " compatibility check
    if count(lines, s:INFO_FILE_VERSION_LINE) == 0
      call s:warnOldInfoFile()
      let g:fuf_infoFile = ''
      throw 1
    endif
  catch /.*/ 
    let lines = []
  endtry
  let s:lastInfoMap = s:deserializeInfoMap(lines)
  if !exists('s:lastInfoMap[a:modeName]')
    let s:lastInfoMap[a:modeName] = {}
  endif
  return extend(s:lastInfoMap[a:modeName], { 'data': [], 'stats': [] }, 'keep')
endfunction

" if a:modeName is empty, a:info is treated as a map of information
function fuf#saveInfoFile(modeName, info)
  if empty(a:modeName)
    let s:lastInfoMap = a:info
  else
    let s:lastInfoMap[a:modeName] = a:info
  endif
  let lines = [ s:INFO_FILE_VERSION_LINE ] + s:serializeInfoMap(s:lastInfoMap)
  try
    call writefile(lines, expand(g:fuf_infoFile))
  catch /.*/ 
  endtry
endfunction

"
function fuf#editInfoFile()
  new
  silent file `='[fuf-info]'`
  let s:bufNrInfo = bufnr('%')
  setlocal filetype=vim
  setlocal bufhidden=delete
  setlocal buftype=acwrite
  setlocal noswapfile
  augroup FufInfo
    autocmd!
    autocmd BufWriteCmd <buffer> call s:onBufWriteCmdInfoFile()
  augroup END
  execute '0read ' . expand(g:fuf_infoFile)
  setlocal nomodified
endfunction

" 
function fuf#getRunningHandler()
  return s:runningHandler
endfunction

" 
function fuf#onComplete(findstart, base)
  return s:runningHandler.onComplete(a:findstart, a:base)
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:INFO_FILE_VERSION_LINE = "VERSION\t300"
let s:ABBR_SNIP_MASK = '...'
let s:OPEN_TYPE_CURRENT = 1
let s:OPEN_TYPE_SPLIT   = 2
let s:OPEN_TYPE_VSPLIT  = 3
let s:OPEN_TYPE_TAB     = 4

" wildcard -> regexp
function s:convertWildcardToRegexp(expr)
  let re = escape(a:expr, '\')
  for [pat, sub] in [ [ '*', '\\.\\*' ], [ '?', '\\.' ], [ '[', '\\[' ], ]
    let re = substitute(re, pat, sub, 'g')
  endfor
  return '\V' . re
endfunction

" a:pattern: 'str' -> '\V\.\*s\.\*t\.\*r\.\*'
function s:makeFuzzyMatchingExpr(target, pattern)
  let wi = ''
  for c in split(a:pattern, '\zs')
    if wi =~# '[^*?]$' && c !~ '[*?]'
      let wi .= '*'
    endif
    let wi .= c
  endfor
  return s:makePartialMatchingExpr(a:target, wi)
endfunction

" a:pattern: 'str' -> '\Vstr'
"            'st*r' -> '\Vst\.\*r'
function s:makePartialMatchingExpr(target, pattern)
  let patternMigemo = s:makeAdditionalMigemoPattern(a:pattern)
  if a:pattern !~ '[*?]' && empty(patternMigemo)
    " NOTE: stridx is faster than regexp matching
    return 'stridx(' . a:target . ', ' . string(a:pattern) . ') >= 0'
  endif
  return a:target . ' =~# ' .
        \ string(s:convertWildcardToRegexp(a:pattern)) . patternMigemo
endfunction

" 
function s:makeRefiningExpr(pattern)
  let expr = s:makePartialMatchingExpr('v:val.wordForRefining', a:pattern)
  if a:pattern =~# '\D'
    return expr
  else
    return '(' . expr . ' || v:val.index == ' . string(a:pattern) . ')'
  endif
endfunction

" 
function s:makeAdditionalMigemoPattern(pattern)
  if !g:fuf_useMigemo || a:pattern =~# '[^\x01-\x7e]'
    return ''
  endif
  return '\|\m' . substitute(migemo(a:pattern), '\\_s\*', '.*', 'g')
endfunction

" Snips a:str and add a:mask if the length of a:str is more than a:len
function s:snipHead(str, len, mask)
  if a:len >= len(a:str)
    return a:str
  elseif a:len <= len(a:mask)
    return a:mask
  endif
  return a:mask . a:str[-a:len + len(a:mask):]
endfunction

" Snips a:str and add a:mask if the length of a:str is more than a:len
function s:snipTail(str, len, mask)
  if a:len >= len(a:str)
    return a:str
  elseif a:len <= len(a:mask)
    return a:mask
  endif
  return a:str[:a:len - 1 - len(a:mask)] . a:mask
endfunction

" Snips a:str and add a:mask if the length of a:str is more than a:len
function s:snipMid(str, len, mask)
  if a:len >= len(a:str)
    return a:str
  elseif a:len <= len(a:mask)
    return a:mask
  endif
  let len_head = (a:len - len(a:mask)) / 2
  let len_tail = a:len - len(a:mask) - len_head
  return  (len_head > 0 ? a:str[: len_head - 1] : '') . a:mask .
        \ (len_tail > 0 ? a:str[-len_tail :] : '')
endfunction

"
function s:getWordBoundaries(word)
  return substitute(a:word, '\a\zs\l\+\|\zs\A', '', 'g')
endfunction

"
function s:toLowerForIgnoringCase(str)
    return (g:fuf_ignoreCase ? tolower(a:str) : a:str)
endfunction

"
function s:setRanks(item, pattern, exprBoundary, stats)
  "let word2 = substitute(a:eval_word, '\a\zs\l\+\|\zs\A', '', 'g')
  let a:item.ranks = [
        \   s:evaluateLearningRank(a:item.word, a:stats),
        \   -s:scoreSequentialMatching(a:item.wordForRank, a:pattern),
        \   -s:scoreBoundaryMatching(a:item.wordForBoundary, 
        \                            a:pattern, a:exprBoundary),
        \   a:item.index,
        \ ]
  return a:item
endfunction

" 
function s:evaluateLearningRank(word, stats)
  for i in range(len(a:stats))
    if a:stats[i].word ==# a:word
      return i
    endif
  endfor
  return len(a:stats)
endfunction

let g:s = ""
" range of return value is [0.0, 1.0]
function s:scoreSequentialMatching(word, pattern)
  if empty(a:pattern)
    return 0.0
  endif
  let pos = stridx(a:word, a:pattern)
  if pos < 0
    return 0.0
  endif
  let lenRest = len(a:word) - len(a:pattern) - pos
  return (pos == 0 ? 0.5 : 0.0) + 0.5 / (lenRest + 1)
endfunction

" range of return value is [0.0, 1.0]
function s:scoreBoundaryMatching(wordForBoundary, pattern, exprBoundary)
  if empty(a:pattern)
    return 0.0
  endif
  if !eval(a:exprBoundary)
    return 0
  endif
  return 0.5 + 0.5 * s:scoreSequentialMatching(a:wordForBoundary, a:pattern)
endfunction

"
function s:highlightPrompt(prompt)
  syntax clear
  execute printf('syntax match %s /^\V%s/', g:fuf_promptHighlight, escape(a:prompt, '\'))
endfunction

"
function s:highlightError()
  syntax clear
  syntax match Error  /^.*$/
endfunction

" returns 0 if the buffer is not found.
function s:moveToWindowOfBufferInCurrentTabPage(bufNr)
  if count(tabpagebuflist(), a:bufNr) == 0
    return 0
  endif
  execute bufwinnr(a:bufNr) . 'wincmd w'
  return 1
endfunction

" returns 0 if the buffer is not found.
function s:moveToOtherTabPageOpeningBuffer(bufNr)
  for tabNr in range(1, tabpagenr('$'))
    if tabNr != tabpagenr() && count(tabpagebuflist(tabNr), a:bufNr) > 0
      execute 'tabnext ' . tabNr
      return 1
    endif
  endfor
  return 0
endfunction

" returns 0 if the buffer is not found.
function s:moveToWindowOfBufferInOtherTabPage(bufNr)
  if !s:moveToOtherTabPageOpeningBuffer(a:bufNr)
    return 0
  endif
  return s:moveToWindowOfBufferInCurrentTabPage(a:bufNr)
endfunction

"
function s:expandAbbrevMap(pattern, abbrevMap)
  let result = [a:pattern]
  for [pattern, subs] in items(a:abbrevMap)
    let exprs = result
    let result = []
    for expr in exprs
      let result += map(copy(subs), 'substitute(expr, pattern, escape(v:val, ''\''), "g")')
    endfor
  endfor
  return fuf#unique(result)
endfunction

"
function s:makeFileAbbrInfo(item, maxLenStats)
  let head = matchstr(a:item.word, '^.*[/\\]\ze.')
  let a:item.abbr = { 'head' : head,
        \             'tail' : a:item.word[strlen(head):],
        \             'key' : head . '.',
        \             'prefix' : printf('%4d: ', a:item.index), }
  if exists('a:item.abbrPrefix')
    let a:item.abbr.prefix .= a:item.abbrPrefix
  endif
  let len = len(a:item.abbr.prefix) + len(a:item.word) +
        \   (exists('a:item.menu') ? len(a:item.menu) + 2 : 0)
  if !exists('a:maxLenStats[a:item.abbr.key]') || len > a:maxLenStats[a:item.abbr.key]
    let a:maxLenStats[a:item.abbr.key] = len
  endif
  return a:item
endfunction

"
function s:getSnippedHead(head, baseLen)
  return s:snipMid(a:head, len(a:head) + g:fuf_maxMenuWidth - a:baseLen, s:ABBR_SNIP_MASK)
endfunction

"
function s:setAbbrWithFileAbbrData(item, snippedHeads)
  let lenMenu = (exists('a:item.menu') ? len(a:item.menu) + 2 : 0)
  let abbr = a:item.abbr.prefix . a:snippedHeads[a:item.abbr.key] . a:item.abbr.tail
  let a:item.abbr = s:snipTail(abbr, g:fuf_maxMenuWidth - lenMenu, s:ABBR_SNIP_MASK)
  return a:item
endfunction

let s:bufNrFuf = -1

"
function s:openFufBuffer()
  if !bufexists(s:bufNrFuf)
    topleft 1new
    silent file `='[fuf]'`
    let s:bufNrFuf = bufnr('%')
  elseif bufwinnr(s:bufNrFuf) == -1
    topleft 1split
    execute 'silent ' . s:bufNrFuf . 'buffer'
    delete _
  elseif bufwinnr(s:bufNrFuf) != bufwinnr('%')
    execute bufwinnr(s:bufNrFuf) . 'wincmd w'
  endif
endfunction

function s:setLocalOptionsForFufBuffer()
  setlocal filetype=fuf
  setlocal bufhidden=delete
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal nobuflisted
  setlocal modifiable
  setlocal nocursorline   " for highlighting
  setlocal nocursorcolumn " for highlighting
  setlocal omnifunc=fuf#onComplete
endfunction

"
function s:activateFufBuffer()
  " lcd . : To avoid the strange behavior that unnamed buffer changes its cwd
  "         if 'autochdir' was set on.
  lcd .
  let cwd = getcwd()
  call s:openFufBuffer()
  " lcd ... : countermeasure against auto-cd script
  lcd `=cwd`
  call s:setLocalOptionsForFufBuffer()
  redraw " for 'lazyredraw'
  if exists(':AcpLock')
    AcpLock
  elseif exists(':AutoComplPopLock')
    AutoComplPopLock
  endif
endfunction

"
function s:deactivateFufBuffer()
  if exists(':AcpUnlock')
    AcpUnlock
  elseif exists(':AutoComplPopUnlock')
    AutoComplPopUnlock
  endif
  " must close after returning to previous window
  wincmd p
  execute s:bufNrFuf . 'bdelete'
endfunction

let s:originalGlobalOptions = {}

" 
function s:setTemporaryGlobalOption(name, value)
  call extend(s:originalGlobalOptions, { a:name : eval('&' . a:name) }, 'keep')
  execute printf('let &%s = a:value', a:name)
endfunction

"
function s:restoreTemporaryGlobalOptions()
  for [name, value] in items(s:originalGlobalOptions)
    execute printf('let &%s = value', name)
  endfor
  let s:originalGlobalOptions = {}
endfunction

"
function s:warnOldInfoFile()
  call fuf#echoWithHl(printf("=================================================================\n" .
        \                    "  Sorry, but your information file for FuzzyFinder is no longer  \n" .
        \                    "  compatible with this version of FuzzyFinder. Please remove     \n" .
        \                    "  %-63s\n" .
        \                    "=================================================================\n" ,
        \                    '"' . expand(g:fuf_infoFile) . '".'),
        \           'WarningMsg')
  echohl Question
  call input('Press Enter')
  echohl None
endfunction

"
function s:serializeInfoMap(infoMap)
  let lines = []
  for [m, info] in items(a:infoMap)
    for [key, value] in items(info)
      let lines += map(copy(value), 'm . "\t" . key . "\t" . string(v:val)')
    endfor
  endfor
  return lines
endfunction

"
function s:deserializeInfoMap(lines)
  let infoMap = {}
  for e in filter(map(a:lines, 'matchlist(v:val, ''^\v(\S+)\s+(\S+)\s+(.+)$'')'), '!empty(v:val)')
    if !exists('infoMap[e[1]]')
      let infoMap[e[1]] = {}
    endif
    if !exists('infoMap[e[1]][e[2]]')
      let infoMap[e[1]][e[2]] = []
    endif
    call add(infoMap[e[1]][e[2]], eval(e[3]))
  endfor
  return infoMap
endfunction

"
function s:onBufWriteCmdInfoFile()
  call fuf#saveInfoFile('', s:deserializeInfoMap(getline(1, '$')))
  setlocal nomodified
  execute printf('%dbdelete! ', s:bufNrInfo)
  echo "Information file updated"
endfunction

" }}}1
"=============================================================================
" s:handlerBase {{{1

let s:handlerBase = {}

"-----------------------------------------------------------------------------
" PURE VIRTUAL FUNCTIONS {{{2
"
" "
" s:handler.getModeName()
" 
" "
" s:handler.getPrompt()
" 
" " returns true if the mode deals with file paths.
" s:handler.targetsPath()
"
" "
" s:handler.getCompleteItems(patternSet)
" 
" "
" s:handler.onOpen(word, mode)
" 
" " Before entering FuzzyFinder buffer. This function should return in a short time.
" s:handler.onModeEnterPre()
"
" " After entering FuzzyFinder buffer.
" s:handler.onModeEnterPost()
"
" " After leaving FuzzyFinder buffer.
" s:handler.onModeLeavePost(opened)
"
" }}}2
"-----------------------------------------------------------------------------

"
function s:handlerBase.concretize(deriv)
  call extend(self, a:deriv, 'error')
  return self
endfunction

"
function s:handlerBase.addStat(pattern, word)
  let stat = { 'pattern' : a:pattern, 'word' : a:word }
  call filter(self.info.stats, 'v:val !=# stat')
  call insert(self.info.stats, stat)
  let self.info.stats = self.info.stats[0 : g:fuf_learningLimit - 1]
endfunction

"
function s:handlerBase.getMatchingCompleteItems(patternBase)
  let MakeMatchingExpr = function(self.partialMatching
        \                         ? 's:makePartialMatchingExpr'
        \                         : 's:makeFuzzyMatchingExpr')
  let patternSet = self.makePatternSet(a:patternBase)
  let exprBoundary = s:makeFuzzyMatchingExpr('a:wordForBoundary', patternSet.primaryForRank)
  let stats = filter(
        \ copy(self.info.stats), 'v:val.pattern ==# patternSet.primaryForRank')
  let items = self.getCompleteItems(patternSet.primary)
  " NOTE: In order to know an excess, plus 1 to limit number
  let items = fuf#filterWithLimit(
        \ items, patternSet.filteringExpr, g:fuf_enumeratingLimit + 1)
  return map(items,
        \ 's:setRanks(v:val, patternSet.primaryForRank, exprBoundary, stats)')
endfunction

"
function s:handlerBase.onComplete(findstart, base)
  if a:findstart
    return 0
  elseif  !self.existsPrompt(a:base)
    return []
  endif
  call s:highlightPrompt(self.getPrompt())
  let items = []
  for patternBase in s:expandAbbrevMap(self.removePrompt(a:base), g:fuf_abbrevMap)
    let items += self.getMatchingCompleteItems(patternBase)
    if len(items) > g:fuf_enumeratingLimit
      let items = items[ : g:fuf_enumeratingLimit - 1]
      call s:highlightError()
      break
    endif
  endfor
  if empty(items)
    call s:highlightError()
  else
    call sort(items, 'fuf#compareRanks')
    call feedkeys("\<C-p>\<Down>", 'n')
    let self.lastFirstWord = items[0].word
  endif
  return items
endfunction

"
function s:handlerBase.existsPrompt(line)
  return  strlen(a:line) >= strlen(self.getPrompt()) &&
        \ a:line[:strlen(self.getPrompt()) -1] ==# self.getPrompt()
endfunction

"
function s:handlerBase.removePrompt(line)
  return a:line[(self.existsPrompt(a:line) ? strlen(self.getPrompt()) : 0):]
endfunction

"
function s:handlerBase.restorePrompt(line)
  let i = 0
  while i < len(self.getPrompt()) && i < len(a:line) && self.getPrompt()[i] ==# a:line[i]
    let i += 1
  endwhile
  return self.getPrompt() . a:line[i : ]
endfunction

"
function s:handlerBase.onCursorMovedI()
  if !self.existsPrompt(getline('.'))
    call setline('.', self.restorePrompt(getline('.')))
    call feedkeys("\<End>", 'n')
  elseif col('.') <= len(self.getPrompt())
    " if the cursor is moved before command prompt
    call feedkeys(repeat("\<Right>", len(self.getPrompt()) - col('.') + 1), 'n')
  elseif col('.') > strlen(getline('.')) && col('.') != self.lastCol
    " if the cursor is placed on the end of the line and has been actually moved.
    let self.lastCol = col('.')
    let self.lastPattern = self.removePrompt(getline('.'))
    call feedkeys("\<C-x>\<C-o>", 'n')
  endif
endfunction

"
function s:handlerBase.onInsertLeave()
  unlet s:runningHandler
  let lastPattern = self.removePrompt(getline('.'))
  call s:restoreTemporaryGlobalOptions()
  call s:deactivateFufBuffer()
  call fuf#saveInfoFile(self.getModeName(), self.info)
  let fOpen = exists('s:reservedCommand')
  if fOpen
    call self.onOpen(s:reservedCommand[0], s:reservedCommand[1])
    unlet s:reservedCommand
  endif
  call self.onModeLeavePost(fOpen)
  if exists('s:reservedMode')
    call fuf#launch(s:reservedMode, lastPattern, self.partialMatching)
    unlet s:reservedMode
  endif
endfunction

"
function s:handlerBase.onCr(openType, fCheckDir)
  if pumvisible()
    call feedkeys(printf("\<C-y>\<C-r>=fuf#getRunningHandler().onCr(%d, %d) ? '' : ''\<CR>",
          \              a:openType, self.targetsPath()), 'n')
    return
  endif
  if !empty(self.lastPattern)
    call self.addStat(self.lastPattern, self.removePrompt(getline('.')))
  endif
  if a:fCheckDir && getline('.') =~# '[/\\]$'
    " To clear i_<C-r> expression (fuf#getRunningHandler().onCr...)
    echo ''
    return
  endif
  let s:reservedCommand = [self.removePrompt(getline('.')), a:openType]
  call feedkeys("\<Esc>", 'n') " stopinsert behavior is strange...
endfunction

"
function s:handlerBase.onBs()
  let pattern = self.removePrompt(getline('.')[ : col('.') - 2])
  if empty(pattern)
    let numBs = 0
  elseif !g:fuf_smartBs
    let numBs = 1
  elseif pattern[-len(g:fuf_patternSeparator) : ] ==# g:fuf_patternSeparator
    let numBs = len(split(pattern, g:fuf_patternSeparator, 1)[-2])
          \   + len(g:fuf_patternSeparator)
  elseif self.targetsPath() && pattern[-1 : ] =~# '[/\\]'
    let numBs = len(matchstr(pattern, '[^/\\]*.$'))
  else
    let numBs = 1
  endif
  call feedkeys((pumvisible() ? "\<C-e>" : "") . repeat("\<BS>", numBs), 'n')
endfunction

"
function s:handlerBase.onPreviewBase()
  if self.getPreviewHeight() <= 0
    return
  elseif !pumvisible()
    return
  elseif !self.existsPrompt(getline('.'))
    let word = self.removePrompt(getline('.'))
  elseif !exists('self.lastFirstWord')
    return
  else
    let word = self.lastFirstWord
  endif
  redraw
  if exists('self.lastPreviewInfo') && self.lastPreviewInfo.word ==# word
    let self.lastPreviewInfo.count += 1
  else
    let self.lastPreviewInfo = {'word': word, 'count': 0}
  endif
  let lines = self.makePreviewLines(word, self.lastPreviewInfo.count)
  let lines = lines[: self.getPreviewHeight() - 1]
  call map(lines, 'substitute(v:val, "\t", repeat(" ", &tabstop), "g")')
  call map(lines, 's:snipTail(v:val, &columns - 1, s:ABBR_SNIP_MASK)')
  echo join(lines, "\n")
endfunction

"
function s:handlerBase.onSwitchMode(shift)
  let modes = copy(g:fuf_modes)
  call map(modes, '{ "ranks": [ fuf#{v:val}#getSwitchOrder(), v:val ] }')
  call filter(modes, 'v:val.ranks[0] >= 0')
  call sort(modes, 'fuf#compareRanks')
  let s:reservedMode = self.getModeName()
  for i in range(len(modes))
    if modes[i].ranks[1] ==# self.getModeName()
      let s:reservedMode = modes[(i + a:shift) % len(modes)].ranks[1]
      break
    endif
  endfor
  call feedkeys("\<Esc>", 'n') " stopinsert doesn't work.
endfunction

"
function s:handlerBase.onSwitchMatching()
  let self.partialMatching = !self.partialMatching
  let self.lastCol = -1
  call setline('.', self.restorePrompt(self.lastPattern))
  call feedkeys("\<End>", 'n')
  "call self.onCursorMovedI()
endfunction

"
function s:handlerBase.onRecallPattern(shift)
  let patterns = map(copy(self.info.stats), 'v:val.pattern')
  if !exists('self.indexRecall')
    let self.indexRecall = -1
  endif
  let self.indexRecall += a:shift
  if self.indexRecall < 0
    let self.indexRecall = -1
  elseif self.indexRecall >= len(patterns)
    let self.indexRecall = len(patterns) - 1
  else
    call setline('.', self.getPrompt() . patterns[self.indexRecall])
    call feedkeys("\<End>", 'n')
  endif
endfunction

" }}}1
"=============================================================================
" INITIALIZATION {{{1

augroup FufGlobal
  autocmd!
  autocmd BufLeave * let s:bufferCursorPosMap[bufnr('')] = getpos('.')
augroup END

let s:bufferCursorPosMap = {}

" }}}1
"=============================================================================
" vim: set fdm=marker:

