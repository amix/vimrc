"
" Python filetype plugin for running flake8
" Language:     Python (ft=python)
" Maintainer:   Vincent Driessen <vincent@3rdcloud.com>
" Version:      Vim 7 (may work with lower Vim versions, but not tested)
" URL:          http://github.com/nvie/vim-flake8
"
" Only do this when not done yet for this buffer
if exists("b:loaded_flake8_ftplugin")
    finish
endif
let b:loaded_flake8_ftplugin=1

let s:save_cpo = &cpo
set cpo&vim

"" Highlight groups for errors
" pep8 errors
highlight default Flake8_Error
            \ ctermbg=DarkRed ctermfg=Red cterm=bold
            \ guibg=DarkRed   guifg=Red   gui=bold
" pep8 warnings
highlight default Flake8_Warning
            \ ctermbg=Yellow ctermfg=DarkYellow cterm=bold
            \ guibg=Yellow   guifg=DarkYellow   gui=bold
" PyFlakes codes
highlight default Flake8_PyFlake
            \ ctermbg=DarkBlue ctermfg=Blue cterm=bold
            \ guibg=DarkBlue   guifg=Blue   gui=bold
" McCabe complexity warnings
highlight default Flake8_Complexity
            \ ctermbg=DarkBlue ctermfg=Blue cterm=bold
            \ guibg=DarkBlue   guifg=Blue   gui=bold
" naming conventions
highlight default Flake8_Naming
            \ ctermbg=DarkBlue ctermfg=Blue cterm=bold
            \ guibg=DarkBlue   guifg=Blue   gui=bold

" to not break with old versions
function! Flake8()
    call flake8#Flake8()
endfunction

" Add mappings, unless the user didn't want this.
" The default mapping is registered under to <F7> by default, unless the user
" remapped it already (or a mapping exists already for <F7>)
if !exists("no_plugin_maps") && !exists("no_flake8_maps")
    if !hasmapto('Flake8(') && !hasmapto('flake8#Flake8(')
        noremap <buffer> <F7> :call flake8#Flake8()<CR>
    endif
endif

let &cpo = s:save_cpo
unlet s:save_cpo

