scriptencoding utf-8
if exists('g:did_coc_loaded') || v:version < 800
  finish
endif

function! s:checkVersion() abort
  let l:unsupported = 0
  if get(g:, 'coc_disable_startup_warning', 0) != 1
    if has('nvim')
      let l:unsupported = !has('nvim-0.3.2')
    else
      let l:unsupported = !has('patch-8.0.1453')
    endif

    if l:unsupported == 1
      echohl Error
      echom "coc.nvim requires at least Vim 8.0.1453 or Neovim 0.3.2, but you're using an older version."
      echom "Please upgrade your (neo)vim."
      echom "You can add this to your vimrc to avoid this message:"
      echom "    let g:coc_disable_startup_warning = 1"
      echom "Note that some features may error out or behave incorrectly."
      echom "Please do not report bugs unless you're using at least Vim 8.0.1453 or Neovim 0.3.2."
      echohl None
      sleep 2
    else
      if !has('nvim-0.4.0') && !has('patch-8.1.1719')
        echohl WarningMsg
        echom "coc.nvim works best on vim >= 8.1.1719 and neovim >= 0.4.0, consider upgrade your vim."
        echom "You can add this to your vimrc to avoid this message:"
        echom "    let g:coc_disable_startup_warning = 1"
        echom "Note that some features may behave incorrectly."
        echohl None
        sleep 2
      endif
    endif
  endif
endfunction

call s:checkVersion()

let g:did_coc_loaded = 1
let g:coc_service_initialized = 0
let s:is_win = has('win32') || has('win64')
let s:root = expand('<sfile>:h:h')
let s:is_vim = !has('nvim')
let s:is_gvim = s:is_vim && has("gui_running")

if get(g:, 'coc_start_at_startup', 1) && !s:is_gvim
  call coc#rpc#start_server()
endif

function! CocTagFunc(pattern, flags, info) abort
  if a:flags !=# 'c'
    " use standard tag search
    return v:null
  endif
  return coc#rpc#request('getTagList', [])
endfunction

function! CocPopupCallback(bufnr, arglist) abort
  if len(a:arglist) == 2
    if a:arglist[0] == 'confirm'
      call coc#rpc#notify('PromptInsert', [a:arglist[1], a:bufnr])
    elseif a:arglist[0] == 'exit'
      execute 'silent! bd! '.a:bufnr
      "call coc#rpc#notify('PromptUpdate', [a:arglist[1]])
    elseif a:arglist[0] == 'change'
      let text = a:arglist[1]
      let current = getbufvar(a:bufnr, 'current', '')
      if text !=# current
        call setbufvar(a:bufnr, 'current', text)
        let cursor = term_getcursor(a:bufnr)
        let info = {
              \ 'lnum': cursor[0],
              \ 'col': cursor[1],
              \ 'line': text,
              \ 'changedtick': 0
              \ }
        call coc#rpc#notify('CocAutocmd', ['TextChangedI', a:bufnr, info])
      endif
    elseif a:arglist[0] == 'send'
      let key = a:arglist[1]
      let escaped = strcharpart(key, 1, strchars(key) - 2)
      call coc#rpc#notify('PromptKeyPress', [a:bufnr, escaped])
    endif
  endif
endfunction

function! CocAction(name, ...) abort
  if !get(g:, 'coc_service_initialized', 0)
    throw 'coc.nvim not ready when invoke CocAction "'.a:name.'"'
  endif
  return coc#rpc#request(a:name, a:000)
endfunction

function! CocHasProvider(name) abort
  return coc#rpc#request('hasProvider', [a:name])
endfunction

function! CocActionAsync(name, ...) abort
  return s:AsyncRequest(a:name, a:000)
endfunction

function! CocRequest(...) abort
  return coc#rpc#request('sendRequest', a:000)
endfunction

function! CocNotify(...) abort
  return coc#rpc#request('sendNotification', a:000)
endfunction

function! CocRegistNotification(id, method, cb) abort
  call coc#on_notify(a:id, a:method, a:cb)
endfunction

function! CocLocations(id, method, ...) abort
  let args = [a:id, a:method] + copy(a:000)
  return coc#rpc#request('findLocations', args)
endfunction

function! CocLocationsAsync(id, method, ...) abort
  let args = [a:id, a:method] + copy(a:000)
  return s:AsyncRequest('findLocations', args)
endfunction

function! CocRequestAsync(...)
  return s:AsyncRequest('sendRequest', a:000)
endfunction

function! s:AsyncRequest(name, args) abort
  let Cb = empty(a:args)? v:null : a:args[len(a:args) - 1]
  if type(Cb) == 2
    if !coc#rpc#ready()
      call Cb('service not started', v:null)
    else
      call coc#rpc#request_async(a:name, a:args[0:-2], Cb)
    endif
    return ''
  endif
  call coc#rpc#notify(a:name, a:args)
  return ''
endfunction

function! s:CommandList(...) abort
  let list = coc#rpc#request('commandList', a:000)
  return join(list, "\n")
endfunction

function! s:ExtensionList(...) abort
  let stats = CocAction('extensionStats')
  call filter(stats, 'v:val["isLocal"] == v:false')
  let list = map(stats, 'v:val["id"]')
  return join(list, "\n")
endfunction

function! s:SearchOptions(...) abort
  let list = ['-e', '--regexp', '-F', '--fixed-strings', '-L', '--follow',
        \ '-g', '--glob', '--hidden', '--no-hidden', '--no-ignore-vcs',
        \ '--word-regexp', '-w', '--smart-case', '-S', '--no-config',
        \ '--line-regexp', '--no-ignore', '-x']
  return join(list, "\n")
endfunction

function! s:LoadedExtensions(...) abort
  let list = CocAction('loadedExtensions')
  return join(list, "\n")
endfunction

function! s:InstallOptions(...)abort
  let list = ['-terminal', '-sync']
  return join(list, "\n")
endfunction

function! s:OpenConfig()
  let home = coc#util#get_config_home()
  if !isdirectory(home)
    echohl MoreMsg
    echom 'Config directory "'.home.'" does not exist, create? (y/n)'
    echohl None
    let confirm = nr2char(getchar())
    redraw!
    if !(confirm ==? "y" || confirm ==? "\r")
      return
    else
      call mkdir(home, 'p')
    end
  endif
  execute 'edit '.home.'/coc-settings.json'
  call coc#rpc#notify('checkJsonExtension', [])
endfunction

function! s:get_color(item, fallback) abort
  let t = type(a:item)
  if t == 1
    return a:item
  endif
  if t == 4
    let item = get(a:item, 'gui', {})
    let color = get(item, &background, a:fallback)
    return type(color) == 1 ? color : a:fallback
  endif
  return a:fallback
endfunction

function! s:AddAnsiGroups() abort
  let color_map = {}
  let colors = ['#282828', '#cc241d', '#98971a', '#d79921', '#458588', '#b16286', '#689d6a', '#a89984', '#928374']
  let names = ['black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white', 'grey']
  for i in range(0, len(names) - 1)
    let name = names[i]
    if exists('g:terminal_ansi_colors')
      let color_map[name] = s:get_color(get(g:terminal_ansi_colors, i, colors[i]), colors[i])
    else
      let color_map[name] = get(g:, 'terminal_color_'.i, colors[i])
    endif
  endfor
  try
    for name in keys(color_map)
      let foreground = toupper(name[0]).name[1:]
      let foregroundColor = color_map[name]
      for key in keys(color_map)
        let background = toupper(key[0]).key[1:]
        let backgroundColor = color_map[key]
        exe 'hi default CocList'.foreground.background.' guifg='.foregroundColor.' guibg='.backgroundColor
      endfor
      exe 'hi default CocListFg'.foreground. ' guifg='.foregroundColor. ' ctermfg='.foreground
      exe 'hi default CocListBg'.foreground. ' guibg='.foregroundColor. ' ctermbg='.foreground
    endfor
  catch /.*/
    " ignore invalid color
  endtry
endfunction

function! s:CursorRangeFromSelected(type, ...) abort
  " add range by operator
  call coc#rpc#request('cursorsSelect', [bufnr('%'), 'operator', a:type])
endfunction

function! s:OpenDiagnostics(...) abort
  let height = get(a:, 1, 0)
  call coc#rpc#request('fillDiagnostics', [bufnr('%')])
  if height
    execute ':lopen '.height
   else
    lopen
  endif
endfunction

function! s:Disable() abort
  if get(g:, 'coc_enabled', 0) == 0
    return
  endif
  augroup coc_nvim
    autocmd!
  augroup end
  call coc#rpc#request('detach', [])
  echohl MoreMsg
    echom '[coc.nvim] Event disabled'
  echohl None
  let g:coc_enabled = 0
endfunction

function! s:Autocmd(...) abort
  if !get(g:, 'coc_workspace_initialized', 0)
    return
  endif
  call coc#rpc#notify('CocAutocmd', a:000)
endfunction

function! s:HandleCharInsert(char, bufnr) abort
  if get(g:, 'coc_disable_space_report', 0)
    let g:coc_disable_space_report = 0
    if a:char ==# ' '
      return
    endif
  endif
  call s:Autocmd('InsertCharPre', a:char, a:bufnr)
endfunction

function! s:HandleCompleteDone(complete_item) abort
  let item = copy(a:complete_item)
  if get(g:, 'coc_hide_pum', 0)
    let item['close'] = v:true
    let g:coc_hide_pum = 0
  endif
  if get(g:, 'coc_disable_complete_done', 0)
    let g:coc_disable_complete_done = 0
    let item['closed'] = v:true
  endif
  call s:Autocmd('CompleteDone', item)
endfunction

function! s:HandleWinScrolled(winid) abort
  if getwinvar(a:winid, 'float', 0)
    call coc#float#nvim_scrollbar(a:winid)
  endif
  call s:Autocmd('WinScrolled', a:winid)
endfunction

function! s:SyncAutocmd(...)
  if !get(g:, 'coc_workspace_initialized', 0)
    return
  endif
  call coc#rpc#request('CocAutocmd', a:000)
endfunction

function! s:Enable(initialize)
  if get(g:, 'coc_enabled', 0) == 1
    return
  endif
  let g:coc_enabled = 1

  augroup coc_nvim
    autocmd!

    if exists('##MenuPopupChanged') && exists('*nvim_open_win')
      autocmd MenuPopupChanged *   call s:Autocmd('MenuPopupChanged', get(v:, 'event', {}), win_screenpos(winnr())[0] + winline() - 2)
    endif
    if exists('##CompleteChanged')
      autocmd CompleteChanged *   call s:Autocmd('MenuPopupChanged', get(v:, 'event', {}), win_screenpos(winnr())[0] + winline() - 2)
    endif

    if coc#rpc#started()
      autocmd VimEnter            * call coc#rpc#notify('VimEnter', [])
    elseif get(g:, 'coc_start_at_startup', 1)
      autocmd VimEnter            * call coc#rpc#start_server()
    endif
    if s:is_vim
      if exists('##DirChanged')
        autocmd DirChanged        * call s:Autocmd('DirChanged', getcwd())
      endif
      if exists('##TerminalOpen')
        autocmd TerminalOpen      * call s:Autocmd('TermOpen', +expand('<abuf>'))
      endif
    else
      autocmd DirChanged        * call s:Autocmd('DirChanged', get(v:event, 'cwd', ''))
      autocmd TermOpen          * call s:Autocmd('TermOpen', +expand('<abuf>'))
      autocmd WinEnter          * call coc#float#nvim_win_enter(win_getid())
    endif
    autocmd CursorMoved         list:///* call coc#list#select(bufnr('%'), line('.'))
    if exists('##WinClosed')
      autocmd WinClosed         * call coc#float#on_close(+expand('<amatch>'))
      autocmd WinClosed         * call coc#notify#on_close(+expand('<amatch>'))
    elseif exists('##TabEnter')
      autocmd TabEnter          * call coc#notify#reflow()
    endif
    if has('nvim-0.4.0') || has('patch-8.1.1719')
      autocmd CursorHold        * call coc#float#check_related()
    endif
    if exists('##WinScrolled')
      autocmd WinScrolled       * call s:HandleWinScrolled(+expand('<amatch>'))
    endif
    autocmd TabNew              * call s:Autocmd('TabNew', tabpagenr())
    autocmd TabClosed           * call s:Autocmd('TabClosed', +expand('<afile>'))
    autocmd WinLeave            * call s:Autocmd('WinLeave', win_getid())
    autocmd WinEnter            * call s:Autocmd('WinEnter', win_getid())
    autocmd BufWinLeave         * call s:Autocmd('BufWinLeave', +expand('<abuf>'), bufwinid(+expand('<abuf>')))
    autocmd BufWinEnter         * call s:Autocmd('BufWinEnter', +expand('<abuf>'), win_getid())
    autocmd FileType            * call s:Autocmd('FileType', expand('<amatch>'), +expand('<abuf>'))
    autocmd CompleteDone        * call s:HandleCompleteDone(get(v:, 'completed_item', {}))
    autocmd InsertCharPre       * call s:HandleCharInsert(v:char, bufnr('%'))
    if exists('##TextChangedP')
      autocmd TextChangedP      * call s:Autocmd('TextChangedP', +expand('<abuf>'), coc#util#change_info())
    endif
    autocmd TextChangedI        * call s:Autocmd('TextChangedI', +expand('<abuf>'), coc#util#change_info())
    autocmd InsertLeave         * call s:Autocmd('InsertLeave', +expand('<abuf>'))
    autocmd InsertEnter         * call s:Autocmd('InsertEnter', +expand('<abuf>'))
    autocmd BufHidden           * call s:Autocmd('BufHidden', +expand('<abuf>'))
    autocmd BufEnter            * call s:Autocmd('BufEnter', +expand('<abuf>'))
    autocmd TextChanged         * call s:Autocmd('TextChanged', +expand('<abuf>'), getbufvar(+expand('<abuf>'), 'changedtick'))
    autocmd BufWritePost        * call s:Autocmd('BufWritePost', +expand('<abuf>'), getbufvar(+expand('<abuf>'), 'changedtick'))
    autocmd CursorMoved         * call s:Autocmd('CursorMoved', +expand('<abuf>'), [line('.'), col('.')])
    autocmd CursorMovedI        * call s:Autocmd('CursorMovedI', +expand('<abuf>'), [line('.'), col('.')])
    autocmd CursorHold          * call s:Autocmd('CursorHold', +expand('<abuf>'), [line('.'), col('.')], coc#util#suggest_variables(bufnr('%')))
    autocmd CursorHoldI         * call s:Autocmd('CursorHoldI', +expand('<abuf>'), [line('.'), col('.')])
    autocmd BufNewFile,BufReadPost * call s:Autocmd('BufCreate', +expand('<abuf>'))
    autocmd BufUnload           * call s:Autocmd('BufUnload', +expand('<abuf>'))
    autocmd BufWritePre         * call s:SyncAutocmd('BufWritePre', +expand('<abuf>'), bufname(+expand('<abuf>')), getbufvar(+expand('<abuf>'), 'changedtick'))
    autocmd FocusGained         * if mode() !~# '^c' | call s:Autocmd('FocusGained') | endif
    autocmd FocusLost           * call s:Autocmd('FocusLost')
    autocmd VimResized          * call s:Autocmd('VimResized', &columns, &lines)
    autocmd VimLeavePre         * let g:coc_vim_leaving = 1
    autocmd VimLeavePre         * call s:Autocmd('VimLeavePre')
    autocmd BufReadCmd,FileReadCmd,SourceCmd list://* call coc#list#setup(expand('<amatch>'))
    autocmd BufWriteCmd __coc_refactor__* :call coc#rpc#notify('saveRefactor', [+expand('<abuf>')])
    autocmd ColorScheme * call s:Hi()
  augroup end
  if a:initialize == 0
     call coc#rpc#request('attach', [])
     echohl MoreMsg
     echom '[coc.nvim] Event enabled'
     echohl None
  endif
endfunction

function! s:Hi() abort
  hi default CocErrorSign     ctermfg=Red     guifg=#ff0000 guibg=NONE
  hi default CocWarningSign   ctermfg=Brown   guifg=#ff922b guibg=NONE
  hi default CocInfoSign      ctermfg=Yellow  guifg=#fab005 guibg=NONE
  hi default CocHintSign      ctermfg=Blue    guifg=#15aabf guibg=NONE
  hi default CocSelectedText  ctermfg=Red     guifg=#fb4934 guibg=NONE
  hi default CocCodeLens      ctermfg=Gray    guifg=#999999 guibg=NONE
  hi default CocUnderline     term=underline cterm=underline gui=underline
  hi default CocBold          term=bold cterm=bold gui=bold
  hi default CocItalic        term=italic cterm=italic gui=italic
  if s:is_vim || has('nvim-0.4.0')
    hi default CocStrikeThrough term=strikethrough cterm=strikethrough gui=strikethrough
  else
    hi default CocStrikeThrough guifg=#989898 ctermfg=gray
  endif
  hi default CocMarkdownLink  ctermfg=Blue    guifg=#15aabf guibg=NONE
  hi default CocDisabled      guifg=#999999   ctermfg=gray
  hi default CocSearch        ctermfg=Blue    guifg=#15aabf guibg=NONE
  hi default link CocFadeOut             Conceal
  hi default link CocMarkdownCode        markdownCode
  hi default link CocMarkdownHeader      markdownH1
  hi default link CocMenuSel             PmenuSel
  hi default link CocErrorFloat          CocErrorSign
  hi default link CocWarningFloat        CocWarningSign
  hi default link CocInfoFloat           CocInfoSign
  hi default link CocHintFloat           CocHintSign
  hi default link CocErrorHighlight      CocUnderline
  hi default link CocWarningHighlight    CocUnderline
  hi default link CocInfoHighlight       CocUnderline
  hi default link CocHintHighlight       CocUnderline
  hi default link CocDeprecatedHighlight CocStrikeThrough
  hi default link CocUnusedHighlight     CocFadeOut
  hi default link CocListLine            CursorLine
  hi default link CocListSearch          CocSearch
  hi default link CocListMode            ModeMsg
  hi default link CocListPath            Comment
  hi default link CocHighlightText       CursorColumn
  hi default link CocHoverRange          Search
  hi default link CocCursorRange         Search
  hi default link CocLinkedEditing       CocCursorRange
  hi default link CocHighlightRead       CocHighlightText
  hi default link CocHighlightWrite      CocHighlightText
  hi default link CocInlayHint           CocHintSign
  " Notification
  hi default CocNotificationProgress  ctermfg=Blue    guifg=#15aabf guibg=NONE
  hi default link CocNotificationButton  CocUnderline
  hi default link CocNotificationError   CocErrorFloat
  hi default link CocNotificationWarning CocWarningFloat
  hi default link CocNotificationInfo    CocInfoFloat
  " Snippet
  hi default link CocSnippetVisual       Visual
  " Tree view highlights
  hi default link CocTreeTitle       Title
  hi default link CocTreeDescription Comment
  hi default link CocTreeOpenClose   CocBold
  hi default link CocTreeSelected    CursorLine
  hi default link CocSelectedRange   CocHighlightText
  " Symbol highlights
  hi default link CocSymbolDefault       MoreMsg
  hi default link CocSymbolFile          Statement
  hi default link CocSymbolModule        Statement
  hi default link CocSymbolNamespace     Statement
  hi default link CocSymbolPackage       Statement
  hi default link CocSymbolClass         Statement
  hi default link CocSymbolMethod        Function
  hi default link CocSymbolProperty      Keyword
  hi default link CocSymbolField         CocSymbolDefault
  hi default link CocSymbolConstructor   Function
  hi default link CocSymbolEnum          CocSymbolDefault
  hi default link CocSymbolInterface     CocSymbolDefault
  hi default link CocSymbolFunction      Function
  hi default link CocSymbolVariable      CocSymbolDefault
  hi default link CocSymbolConstant      Constant
  hi default link CocSymbolString        String
  hi default link CocSymbolNumber        Number
  hi default link CocSymbolBoolean       Boolean
  hi default link CocSymbolArray         CocSymbolDefault
  hi default link CocSymbolObject        CocSymbolDefault
  hi default link CocSymbolKey           Keyword
  hi default link CocSymbolNull          Type
  hi default link CocSymbolEnumMember    CocSymbolDefault
  hi default link CocSymbolStruct        Keyword
  hi default link CocSymbolEvent         Keyword
  hi default link CocSymbolOperator      Operator
  hi default link CocSymbolTypeParameter Operator

  if has('nvim')
    hi default link CocFloating NormalFloat
  else
    hi default link CocFloating Pmenu
  endif
  if !exists('*sign_getdefined') || empty(sign_getdefined('CocCurrentLine'))
    sign define CocCurrentLine linehl=CocMenuSel
  endif
  if !exists('*sign_getdefined') || empty(sign_getdefined('CocListCurrent'))
    sign define CocListCurrent linehl=CocListLine
  endif
  if !exists('*sign_getdefined') || empty(sign_getdefined('CocTreeSelected'))
    sign define CocTreeSelected linehl=CocTreeSelected
  endif
  if has('nvim-0.5.0')
    hi default CocCursorTransparent gui=strikethrough blend=100
  endif

  if has('nvim')
    let names = ['Error', 'Warning', 'Info', 'Hint']
    for name in names
      if !hlexists('Coc'.name.'VirtualText')
        exe 'hi default link Coc'.name.'VirtualText Coc'.name.'Sign'
      endif
    endfor
  endif
  call s:AddAnsiGroups()

  if get(g:, 'coc_default_semantic_highlight_groups', 1)
    let hlMap = {
        \ 'Namespace': ['TSNamespace', 'Include'],
        \ 'Type': ['TSType', 'Type'],
        \ 'Class': ['TSConstructor', 'Special'],
        \ 'Enum': ['TSEnum', 'Type'],
        \ 'Interface': ['TSInterface', 'Type'],
        \ 'Struct': ['TSStruct', 'Identifier'],
        \ 'TypeParameter': ['TSParameter', 'Identifier'],
        \ 'Parameter': ['TSParameter', 'Identifier'],
        \ 'Variable': ['TSSymbol', 'Identifier'],
        \ 'Property': ['TSProperty', 'Identifier'],
        \ 'EnumMember': ['TSEnumMember', 'Constant'],
        \ 'Event': ['TSEvent', 'Keyword'],
        \ 'Function': ['TSFunction', 'Function'],
        \ 'Method': ['TSMethod', 'Function'],
        \ 'Macro': ['TSConstMacro', 'Define'],
        \ 'Keyword': ['TSKeyword', 'Keyword'],
        \ 'Modifier': ['TSModifier', 'StorageClass'],
        \ 'Comment': ['TSComment', 'Comment'],
        \ 'String': ['TSString', 'String'],
        \ 'Number': ['TSNumber', 'Number'],
        \ 'Boolean': ['TSBoolean', 'Boolean'],
        \ 'Regexp': ['TSStringRegex', 'String'],
        \ 'Operator': ['TSOperator', 'Operator'],
        \ 'Decorator': ['TSSymbol', 'Identifier'],
        \ 'Deprecated': ['TSStrike', 'CocDeprecatedHighlight']
        \ }
    for [key, value] in items(hlMap)
      let ts = get(value, 0, '')
      let fallback = get(value, 1, '')
      execute 'hi default link CocSem'.key.' '.(hlexists(ts) ? ts : fallback)
    endfor
  endif
endfunction

function! s:FormatFromSelected(type)
  call CocActionAsync('formatSelected', a:type)
endfunction

function! s:CodeActionFromSelected(type)
  call CocActionAsync('codeAction', a:type)
endfunction

function! s:ShowInfo()
  if coc#rpc#ready()
    call coc#rpc#notify('showInfo', [])
  else
    let lines = []
    echomsg 'coc.nvim service not started, checking environment...'
    let node = get(g:, 'coc_node_path', $COC_NODE_PATH == '' ? 'node' : $COC_NODE_PATH)
    if !executable(node)
      call add(lines, 'Error: '.node.' is not executable!')
    else
      let output = trim(system(node . ' --version'))
      let ms = matchlist(output, 'v\(\d\+\).\(\d\+\).\(\d\+\)')
      if empty(ms) || str2nr(ms[1]) < 12 || (str2nr(ms[1]) == 12 && str2nr(ms[2]) < 12)
        call add(lines, 'Error: Node version '.output.' < 12.12.0, please upgrade node.js')
      endif
    endif
    " check bundle
    let file = s:root.'/build/index.js'
    if !filereadable(file)
      call add(lines, 'Error: javascript bundle not found, please compile code of coc.nvim by esbuild.')
    endif
    if !empty(lines)
      botright vnew
      setl filetype=nofile
      call setline(1, lines)
    else
      if get(g:, 'coc_start_at_startup',1)
        echohl MoreMsg | echon 'Service stopped for some unknown reason, try :CocStart' | echohl None
      else
        echohl MoreMsg | echon 'Start on startup is disabled, try :CocStart' | echohl None
      endif
    endif
  endif
endfunction

command! -nargs=0 CocOutline      :call coc#rpc#notify('showOutline', [])
command! -nargs=? CocDiagnostics  :call s:OpenDiagnostics(<f-args>)
command! -nargs=0 CocInfo         :call s:ShowInfo()
command! -nargs=0 CocOpenLog      :call coc#rpc#notify('openLog',  [])
command! -nargs=0 CocDisable      :call s:Disable()
command! -nargs=0 CocEnable       :call s:Enable(0)
command! -nargs=0 CocConfig       :call s:OpenConfig()
command! -nargs=0 CocLocalConfig  :call coc#rpc#notify('openLocalConfig', [])
command! -nargs=0 CocRestart      :call coc#rpc#restart()
command! -nargs=0 CocStart        :call coc#rpc#start_server()
command! -nargs=0 CocRebuild      :call coc#util#rebuild()
command! -nargs=1 -complete=custom,s:LoadedExtensions  CocWatch    :call coc#rpc#notify('watchExtension', [<f-args>])
command! -nargs=+ -complete=custom,s:SearchOptions  CocSearch    :call coc#rpc#notify('search', [<f-args>])
command! -nargs=+ -complete=custom,s:ExtensionList  CocUninstall :call CocActionAsync('uninstallExtension', <f-args>)
command! -nargs=* -complete=custom,s:CommandList -range CocCommand :call coc#rpc#notify('runCommand', [<f-args>])
command! -nargs=* -complete=custom,coc#list#options CocList      :call coc#rpc#notify('openList',  [<f-args>])
command! -nargs=? -complete=custom,coc#list#names CocListResume   :call coc#rpc#notify('listResume', [<f-args>])
command! -nargs=? -complete=custom,coc#list#names CocListCancel   :call coc#rpc#notify('listCancel', [])
command! -nargs=? -complete=custom,coc#list#names CocPrev         :call coc#rpc#notify('listPrev', [<f-args>])
command! -nargs=? -complete=custom,coc#list#names CocNext         :call coc#rpc#notify('listNext', [<f-args>])
command! -nargs=? -complete=custom,coc#list#names CocFirst        :call coc#rpc#notify('listFirst', [<f-args>])
command! -nargs=? -complete=custom,coc#list#names CocLast         :call coc#rpc#notify('listLast', [<f-args>])
command! -nargs=0 CocUpdate       :call coc#util#update_extensions(1)
command! -nargs=0 -bar CocUpdateSync   :call coc#util#update_extensions()
command! -nargs=* -bar -complete=custom,s:InstallOptions CocInstall   :call coc#util#install_extension([<f-args>])

call s:Enable(1)
call s:Hi()

vnoremap <silent> <Plug>(coc-range-select)          :<C-u>call       CocActionAsync('rangeSelect',     visualmode(), v:true)<CR>
vnoremap <silent> <Plug>(coc-range-select-backward) :<C-u>call       CocActionAsync('rangeSelect',     visualmode(), v:false)<CR>
nnoremap <Plug>(coc-range-select)          :<C-u>call       CocActionAsync('rangeSelect',     '', v:true)<CR>
nnoremap <Plug>(coc-codelens-action)       :<C-u>call       CocActionAsync('codeLensAction')<CR>
vnoremap <silent> <Plug>(coc-format-selected)       :<C-u>call       CocActionAsync('formatSelected',     visualmode())<CR>
vnoremap <silent> <Plug>(coc-codeaction-selected)   :<C-u>call       CocActionAsync('codeAction',         visualmode())<CR>
nnoremap <Plug>(coc-codeaction-selected)   :<C-u>set        operatorfunc=<SID>CodeActionFromSelected<CR>g@
nnoremap <Plug>(coc-codeaction)            :<C-u>call       CocActionAsync('codeAction',         '')<CR>
nnoremap <Plug>(coc-codeaction-line)       :<C-u>call       CocActionAsync('codeAction',         'line')<CR>
nnoremap <Plug>(coc-codeaction-cursor)     :<C-u>call       CocActionAsync('codeAction',         'cursor')<CR>
nnoremap <silent> <Plug>(coc-rename)                :<C-u>call       CocActionAsync('rename')<CR>
nnoremap <silent> <Plug>(coc-format-selected)       :<C-u>set        operatorfunc=<SID>FormatFromSelected<CR>g@
nnoremap <silent> <Plug>(coc-format)                :<C-u>call       CocActionAsync('format')<CR>
nnoremap <silent> <Plug>(coc-diagnostic-info)       :<C-u>call       CocActionAsync('diagnosticInfo')<CR>
nnoremap <silent> <Plug>(coc-diagnostic-next)       :<C-u>call       CocActionAsync('diagnosticNext')<CR>
nnoremap <silent> <Plug>(coc-diagnostic-prev)       :<C-u>call       CocActionAsync('diagnosticPrevious')<CR>
nnoremap <silent> <Plug>(coc-diagnostic-next-error) :<C-u>call       CocActionAsync('diagnosticNext',     'error')<CR>
nnoremap <silent> <Plug>(coc-diagnostic-prev-error) :<C-u>call       CocActionAsync('diagnosticPrevious', 'error')<CR>
nnoremap <silent> <Plug>(coc-definition)            :<C-u>call       CocActionAsync('jumpDefinition')<CR>
nnoremap <silent> <Plug>(coc-declaration)           :<C-u>call       CocActionAsync('jumpDeclaration')<CR>
nnoremap <silent> <Plug>(coc-implementation)        :<C-u>call       CocActionAsync('jumpImplementation')<CR>
nnoremap <silent> <Plug>(coc-type-definition)       :<C-u>call       CocActionAsync('jumpTypeDefinition')<CR>
nnoremap <silent> <Plug>(coc-references)            :<C-u>call       CocActionAsync('jumpReferences')<CR>
nnoremap <silent> <Plug>(coc-references-used)       :<C-u>call       CocActionAsync('jumpUsed')<CR>
nnoremap <silent> <Plug>(coc-openlink)              :<C-u>call       CocActionAsync('openLink')<CR>
nnoremap <silent> <Plug>(coc-fix-current)           :<C-u>call       CocActionAsync('doQuickfix')<CR>
nnoremap <silent> <Plug>(coc-float-hide)            :<C-u>call       coc#float#close_all()<CR>
nnoremap <silent> <Plug>(coc-float-jump)            :<c-u>call       coc#float#jump()<cr>
nnoremap <silent> <Plug>(coc-command-repeat)        :<C-u>call       CocAction('repeatCommand')<CR>
nnoremap <silent> <Plug>(coc-refactor)              :<C-u>call       CocActionAsync('refactor')<CR>
inoremap <silent>                          <Plug>CocRefresh <C-r>=coc#_complete()<CR>

nnoremap <silent> <Plug>(coc-cursors-operator) :<C-u>set operatorfunc=<SID>CursorRangeFromSelected<CR>g@
vnoremap <silent> <Plug>(coc-cursors-range)    :<C-u>call CocAction('cursorsSelect', bufnr('%'), 'range', visualmode())<CR>
nnoremap <silent> <Plug>(coc-cursors-word)     :<C-u>call CocAction('cursorsSelect', bufnr('%'), 'word', 'n')<CR>
nnoremap <silent> <Plug>(coc-cursors-position) :<C-u>call CocAction('cursorsSelect', bufnr('%'), 'position', 'n')<CR>

vnoremap <silent> <Plug>(coc-funcobj-i)        :<C-U>call CocAction('selectSymbolRange', v:true, visualmode(), ['Method', 'Function'])<CR>
vnoremap <silent> <Plug>(coc-funcobj-a)        :<C-U>call CocAction('selectSymbolRange', v:false, visualmode(), ['Method', 'Function'])<CR>
onoremap <silent> <Plug>(coc-funcobj-i)        :<C-U>call CocAction('selectSymbolRange', v:true, '', ['Method', 'Function'])<CR>
onoremap <silent> <Plug>(coc-funcobj-a)        :<C-U>call CocAction('selectSymbolRange', v:false, '', ['Method', 'Function'])<CR>

vnoremap <silent> <Plug>(coc-classobj-i)       :<C-U>call CocAction('selectSymbolRange', v:true, visualmode(), ['Interface', 'Struct', 'Class'])<CR>
vnoremap <silent> <Plug>(coc-classobj-a)       :<C-U>call CocAction('selectSymbolRange', v:false, visualmode(), ['Interface', 'Struct', 'Class'])<CR>
onoremap <silent> <Plug>(coc-classobj-i)       :<C-U>call CocAction('selectSymbolRange', v:true, '', ['Interface', 'Struct', 'Class'])<CR>
onoremap <silent> <Plug>(coc-classobj-a)       :<C-U>call CocAction('selectSymbolRange', v:false, '', ['Interface', 'Struct', 'Class'])<CR>
