" ============================================================================
" File:        exec_menuitem.vim
" Description: plugin for NERD Tree that provides an execute file menu item
" Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" ============================================================================
if exists('g:loaded_nerdtree_exec_menuitem')
    finish
endif
let g:loaded_nerdtree_exec_menuitem = 1

call NERDTreeAddMenuItem({
            \ 'text': '(!)Execute file',
            \ 'shortcut': '!',
            \ 'callback': 'NERDTreeExecFile',
            \ 'isActiveCallback': 'NERDTreeExecFileActive' })

function! NERDTreeExecFileActive()
    let node = g:NERDTreeFileNode.GetSelected()
    return !node.path.isDirectory && node.path.isExecutable
endfunction

function! NERDTreeExecFile()
    let treenode = g:NERDTreeFileNode.GetSelected()
    echo "==========================================================\n"
    echo "Complete the command to execute (add arguments etc):\n"
    let cmd = treenode.path.str({'escape': 1})
    let cmd = input(':!', cmd . ' ')

    if cmd !=# ''
        exec ':!' . cmd
    else
        echo 'Aborted'
    endif
endfunction
