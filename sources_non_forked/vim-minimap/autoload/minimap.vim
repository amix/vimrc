" vim-minimap is free software: you can redistribute it and/or modify
" it under the terms of the GNU Affero General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" vim-minimap is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU Affero General Public License for more details.
"
" You should have received a copy of the GNU Affero General Public License
" along with vim-minimap. If not, see < http://www.gnu.org/licenses/ >.
"
" (C) 2014- by Séverin Lemaignan for the VIM integration, <severin@guakamole.org>
" (C) 2014- by Adam Tauber for the Drawille part, <asciimoo@gmail.com>

if has('python') || has('python3')
    " By default Highlight the current screen as a visual selection.
    if !exists('g:minimap_highlight')
        let g:minimap_highlight = 'Visual'
    endif

    let python_module = fnameescape(globpath(&runtimepath, 'autoload/minimap.py'))
    if has('python')
        exe 'pyfile ' . python_module
    elseif has('python3')
        exe 'py3file ' . python_module
    endif
end

function! minimap#ShowMinimap()
    if has('python')
        python showminimap()
    elseif has('python3')
        python3 showminimap()
    endif
endfunction

function! minimap#UpdateMinimap()
    if has('python')
        python updateminimap()
    elseif has('python3')
        python3 updateminimap()
    endif
endfunction

function! minimap#CloseMinimap()
    if has('python')
        python closeminimap()
    elseif has('python3')
        python3 closeminimap()
    endif
endfunction

function! minimap#ToggleMinimap()
    if has('python')
        python toggleminimap()
    elseif has('python3')
        python3 toggleminimap()
    endif
endfunction

