" ============================================================================
" File:        NERD_tree.vim
" Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" ============================================================================
"
" SECTION: Script init stuff {{{1
"============================================================
scriptencoding utf-8

if exists('loaded_nerd_tree')
    finish
endif
if v:version < 703
    echoerr "NERDTree: this plugin requires vim >= 7.3. DOWNLOAD IT! You'll thank me later!"
    finish
endif
let loaded_nerd_tree = 1

"for line continuation - i.e dont want C in &cpoptions
let s:old_cpo = &cpoptions
set cpoptions&vim

"SECTION: Initialize variable calls and other random constants {{{2
let g:NERDTreeAutoCenter            = get(g:, 'NERDTreeAutoCenter',            1)
let g:NERDTreeAutoCenterThreshold   = get(g:, 'NERDTreeAutoCenterThreshold',   3)
let g:NERDTreeCaseSensitiveFS       = get(g:, 'NERDTreeCaseSensitiveFS',       2)
let g:NERDTreeCaseSensitiveSort     = get(g:, 'NERDTreeCaseSensitiveSort',     0)
let g:NERDTreeNaturalSort           = get(g:, 'NERDTreeNaturalSort',           0)
let g:NERDTreeSortHiddenFirst       = get(g:, 'NERDTreeSortHiddenFirst',       1)
let g:NERDTreeUseTCD                = get(g:, 'NERDTreeUseTCD',                0)
let g:NERDTreeChDirMode             = get(g:, 'NERDTreeChDirMode',             0)
let g:NERDTreeCreatePrefix          = get(g:, 'NERDTreeCreatePrefix',          'silent')
let g:NERDTreeMinimalUI             = get(g:, 'NERDTreeMinimalUI',             0)
let g:NERDTreeMinimalMenu           = get(g:, 'NERDTreeMinimalMenu',           0)
let g:NERDTreeIgnore                = get(g:, 'NERDTreeIgnore',                ['\~$'])
let g:NERDTreeBookmarksFile         = get(g:, 'NERDTreeBookmarksFile',         expand('$HOME') . '/.NERDTreeBookmarks')
let g:NERDTreeBookmarksSort         = get(g:, 'NERDTreeBookmarksSort',         1)
let g:NERDTreeHighlightCursorline   = get(g:, 'NERDTreeHighlightCursorline',   1)
let g:NERDTreeHijackNetrw           = get(g:, 'NERDTreeHijackNetrw',           1)
let g:NERDTreeMarkBookmarks         = get(g:, 'NERDTreeMarkBookmarks',         1)
let g:NERDTreeMouseMode             = get(g:, 'NERDTreeMouseMode',             1)
let g:NERDTreeNotificationThreshold = get(g:, 'NERDTreeNotificationThreshold', 100)
let g:NERDTreeQuitOnOpen            = get(g:, 'NERDTreeQuitOnOpen',            0)
let g:NERDTreeRespectWildIgnore     = get(g:, 'NERDTreeRespectWildIgnore',     0)
let g:NERDTreeShowBookmarks         = get(g:, 'NERDTreeShowBookmarks',         0)
let g:NERDTreeShowFiles             = get(g:, 'NERDTreeShowFiles',             1)
let g:NERDTreeShowHidden            = get(g:, 'NERDTreeShowHidden',            0)
let g:NERDTreeShowLineNumbers       = get(g:, 'NERDTreeShowLineNumbers',       0)
let g:NERDTreeSortDirs              = get(g:, 'NERDTreeSortDirs',              1)
let g:NERDTreeFileLines             = get(g:, 'NERDTreeFileLines',             0)


if !nerdtree#runningWindows() && !nerdtree#runningCygwin()
    let g:NERDTreeDirArrowExpandable  = get(g:, 'NERDTreeDirArrowExpandable',  '▸')
    let g:NERDTreeDirArrowCollapsible = get(g:, 'NERDTreeDirArrowCollapsible', '▾')
else
    let g:NERDTreeDirArrowExpandable  = get(g:, 'NERDTreeDirArrowExpandable',  '+')
    let g:NERDTreeDirArrowCollapsible = get(g:, 'NERDTreeDirArrowCollapsible', '~')
endif

let g:NERDTreeCascadeOpenSingleChildDir = get(g:, 'NERDTreeCascadeOpenSingleChildDir', 1)
let g:NERDTreeCascadeSingleChildDir     = get(g:, 'NERDTreeCascadeSingleChildDir',     1)

let g:NERDTreeSortOrder    = get(g:, 'NERDTreeSortOrder', ['\/$', '*', '\.swp$', '\.bak$', '\~$'])
let g:NERDTreeOldSortOrder = []

let g:NERDTreeGlyphReadOnly = get(g:, 'NERDTreeGlyphReadOnly', 'RO')

if has('conceal')
    let g:NERDTreeNodeDelimiter = get(g:, 'NERDTreeNodeDelimiter', "\x07")
elseif (g:NERDTreeDirArrowExpandable ==# "\u00a0" || g:NERDTreeDirArrowCollapsible ==# "\u00a0")
    let g:NERDTreeNodeDelimiter = get(g:, 'NERDTreeNodeDelimiter', "\u00b7")
else
    let g:NERDTreeNodeDelimiter = get(g:, 'NERDTreeNodeDelimiter', "\u00a0")
endif

"the exists() crap here is a hack to stop vim spazzing out when
"loading a session that was created with an open nerd tree. It spazzes
"because it doesnt store b:NERDTree(its a b: var, and its a hash)
let g:NERDTreeStatusline = get(g:, 'NERDTreeStatusline', "%{exists('b:NERDTree')?b:NERDTree.root.path.str():''}")

let g:NERDTreeWinPos  = get(g:, 'NERDTreeWinPos', 'left')
let g:NERDTreeWinSize = get(g:, 'NERDTreeWinSize', 31)

"init the shell commands that will be used to copy nodes, and remove dir trees
"Note: the space after the command is important
if nerdtree#runningWindows()
    let g:NERDTreeRemoveDirCmd = get(g:, 'NERDTreeRemoveDirCmd', 'rmdir /s /q ')
    let g:NERDTreeCopyDirCmd   = get(g:, 'NERDTreeCopyDirCmd',   'xcopy /s /e /i /y /q ')
    let g:NERDTreeCopyFileCmd  = get(g:, 'NERDTreeCopyFileCmd',  'copy /y ')
else
    let g:NERDTreeRemoveDirCmd = get(g:, 'NERDTreeRemoveDirCmd', 'rm -rf ')
    let g:NERDTreeCopyCmd      = get(g:, 'NERDTreeCopyCmd',      'cp -r ')
endif

"SECTION: Init variable calls for key mappings {{{2
let g:NERDTreeMapCustomOpen      = get(g:, 'NERDTreeMapCustomOpen',      '<CR>')
let g:NERDTreeMapJumpBookmarks   = get(g:, 'NERDTreeMapJumpBookmarks',   'gb')
let g:NERDTreeMapActivateNode    = get(g:, 'NERDTreeMapActivateNode',    'o')
let g:NERDTreeMapChangeRoot      = get(g:, 'NERDTreeMapChangeRoot',      'C')
let g:NERDTreeMapChdir           = get(g:, 'NERDTreeMapChdir',           'cd')
let g:NERDTreeMapCloseChildren   = get(g:, 'NERDTreeMapCloseChildren',   'X')
let g:NERDTreeMapCloseDir        = get(g:, 'NERDTreeMapCloseDir',        'x')
let g:NERDTreeMapDeleteBookmark  = get(g:, 'NERDTreeMapDeleteBookmark',  'D')
let g:NERDTreeMapMenu            = get(g:, 'NERDTreeMapMenu',            'm')
let g:NERDTreeMapHelp            = get(g:, 'NERDTreeMapHelp',            '?')
let g:NERDTreeMapJumpFirstChild  = get(g:, 'NERDTreeMapJumpFirstChild',  'K')
let g:NERDTreeMapJumpLastChild   = get(g:, 'NERDTreeMapJumpLastChild',   'J')
let g:NERDTreeMapJumpNextSibling = get(g:, 'NERDTreeMapJumpNextSibling', '<C-j>')
let g:NERDTreeMapJumpParent      = get(g:, 'NERDTreeMapJumpParent',      'p')
let g:NERDTreeMapJumpPrevSibling = get(g:, 'NERDTreeMapJumpPrevSibling', '<C-k>')
let g:NERDTreeMapJumpRoot        = get(g:, 'NERDTreeMapJumpRoot',        'P')
let g:NERDTreeMapOpenExpl        = get(g:, 'NERDTreeMapOpenExpl',        'e')
let g:NERDTreeMapOpenInTab       = get(g:, 'NERDTreeMapOpenInTab',       't')
let g:NERDTreeMapOpenInTabSilent = get(g:, 'NERDTreeMapOpenInTabSilent', 'T')
let g:NERDTreeMapOpenRecursively = get(g:, 'NERDTreeMapOpenRecursively', 'O')
let g:NERDTreeMapOpenSplit       = get(g:, 'NERDTreeMapOpenSplit',       'i')
let g:NERDTreeMapOpenVSplit      = get(g:, 'NERDTreeMapOpenVSplit',      's')
let g:NERDTreeMapPreview         = get(g:, 'NERDTreeMapPreview',         'g'.NERDTreeMapActivateNode)
let g:NERDTreeMapPreviewSplit    = get(g:, 'NERDTreeMapPreviewSplit',    'g'.NERDTreeMapOpenSplit)
let g:NERDTreeMapPreviewVSplit   = get(g:, 'NERDTreeMapPreviewVSplit',   'g'.NERDTreeMapOpenVSplit)
let g:NERDTreeMapQuit            = get(g:, 'NERDTreeMapQuit',            'q')
let g:NERDTreeMapRefresh         = get(g:, 'NERDTreeMapRefresh',         'r')
let g:NERDTreeMapRefreshRoot     = get(g:, 'NERDTreeMapRefreshRoot',     'R')
let g:NERDTreeMapToggleBookmarks = get(g:, 'NERDTreeMapToggleBookmarks', 'B')
let g:NERDTreeMapToggleFiles     = get(g:, 'NERDTreeMapToggleFiles',     'F')
let g:NERDTreeMapToggleFilters   = get(g:, 'NERDTreeMapToggleFilters',   'f')
let g:NERDTreeMapToggleHidden    = get(g:, 'NERDTreeMapToggleHidden',    'I')
let g:NERDTreeMapToggleFileLines = get(g:, 'NERDTreeMapToggleFileLines', 'FL')
let g:NERDTreeMapToggleZoom      = get(g:, 'NERDTreeMapToggleZoom',      'A')
let g:NERDTreeMapUpdir           = get(g:, 'NERDTreeMapUpdir',           'u')
let g:NERDTreeMapUpdirKeepOpen   = get(g:, 'NERDTreeMapUpdirKeepOpen',   'U')
let g:NERDTreeMapCWD             = get(g:, 'NERDTreeMapCWD',             'CD')
let g:NERDTreeMenuDown           = get(g:, 'NERDTreeMenuDown',           'j')
let g:NERDTreeMenuUp             = get(g:, 'NERDTreeMenuUp',             'k')

"SECTION: Load class files{{{2
call nerdtree#loadClassFiles()

" SECTION: Commands {{{1
"============================================================
call nerdtree#ui_glue#setupCommands()


" SECTION: Auto commands {{{1
"============================================================
augroup NERDTree
    "Save the cursor position whenever we close the nerd tree
    exec 'autocmd BufLeave,WinLeave '. g:NERDTreeCreator.BufNamePrefix() .'* call nerdtree#onBufLeave()'

    "disallow insert mode in the NERDTree
    exec 'autocmd BufEnter,WinEnter '. g:NERDTreeCreator.BufNamePrefix() .'* stopinsert'
augroup END

if g:NERDTreeHijackNetrw
    augroup NERDTreeHijackNetrw
        autocmd VimEnter * silent! autocmd! FileExplorer
        au BufEnter,VimEnter * call nerdtree#checkForBrowse(expand('<amatch>'))
    augroup END
endif

if g:NERDTreeChDirMode ==# 3
    augroup NERDTreeChDirOnTabSwitch
        autocmd TabEnter * if g:NERDTree.ExistsForTab()|call g:NERDTree.ForCurrentTab().getRoot().path.changeToDir()|endif
    augroup END
endif

" SECTION: Public API {{{1
"============================================================
function! NERDTreeAddMenuItem(options)
    call g:NERDTreeMenuItem.Create(a:options)
endfunction

function! NERDTreeAddMenuSeparator(...)
    let opts = a:0 ? a:1 : {}
    call g:NERDTreeMenuItem.CreateSeparator(opts)
endfunction

function! NERDTreeAddSubmenu(options)
    return g:NERDTreeMenuItem.Create(a:options)
endfunction

function! NERDTreeAddKeyMap(options)
    call g:NERDTreeKeyMap.Create(a:options)
endfunction

function! NERDTreeRender()
    call nerdtree#renderView()
endfunction

function! NERDTreeFocus()
    if g:NERDTree.IsOpen()
        call g:NERDTree.CursorToTreeWin(0)
    else
        call g:NERDTreeCreator.ToggleTabTree('')
    endif
endfunction

function! NERDTreeCWD()

    if empty(getcwd())
        call nerdtree#echoWarning('current directory does not exist')
        return
    endif

    try
        let l:cwdPath = g:NERDTreePath.New(getcwd())
    catch /^NERDTree.InvalidArgumentsError/
        call nerdtree#echoWarning('current directory does not exist')
        return
    endtry

    call NERDTreeFocus()

    if b:NERDTree.root.path.equals(l:cwdPath)
        return
    endif

    let l:newRoot = g:NERDTreeFileNode.New(l:cwdPath, b:NERDTree)
    call b:NERDTree.changeRoot(l:newRoot)
    normal! ^
endfunction

function! NERDTreeAddPathFilter(callback)
    call g:NERDTree.AddPathFilter(a:callback)
endfunction

" SECTION: Post Source Actions {{{1
call nerdtree#postSourceActions()

"reset &cpoptions back to users setting
let &cpoptions = s:old_cpo

" vim: set sw=4 sts=4 et fdm=marker:
