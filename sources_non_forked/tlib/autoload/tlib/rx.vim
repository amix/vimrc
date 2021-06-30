" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    113


" :def: function! tlib#rx#Escape(text, ?magic='m')
" magic can be one of: m, M, v, V
" See :help 'magic'
function! tlib#rx#Escape(text, ...) "{{{3
    TVarArg 'magic'
    if empty(magic)
        let magic = 'm'
    endif
    if magic =~# '^\\\?m$'
        return escape(a:text, '^$.*\[]~')
    elseif magic =~# '^\\\?M$'
        return escape(a:text, '^$\')
    elseif magic =~# '^\\\?V$'
        return escape(a:text, '\')
    elseif magic =~# '^\\\?v$'
        return substitute(a:text, '[^0-9a-zA-Z_]', '\\&', 'g')
    else
        echoerr 'tlib: Unsupported magic type'
        return a:text
    endif
endf

" :def: function! tlib#rx#EscapeReplace(text, ?magic='m')
" Escape return |sub-replace-special|.
function! tlib#rx#EscapeReplace(text, ...) "{{{3
    TVarArg ['magic', 'm']
    if magic ==# 'm' || magic ==# 'v'
        return escape(a:text, '\&~')
    elseif magic ==# 'M' || magic ==# 'V'
        return escape(a:text, '\')
    else
        echoerr 'magic must be one of: m, v, M, V'
    endif
endf


function! tlib#rx#Suffixes(...) "{{{3
    TVarArg ['magic', 'm']
    let sfx = split(&suffixes, ',')
    call map(sfx, 'tlib#rx#Escape(v:val, magic)')
    if magic ==# 'v'
        return '('. join(sfx, '|') .')$'
    elseif magic ==# 'V'
        return '\('. join(sfx, '\|') .'\)\$'
    else
        return '\('. join(sfx, '\|') .'\)$'
    endif
endf


function! tlib#rx#LooksLikeRegexp(text) abort "{{{3
    return a:text =~ '[.?*+{}\[\]]'
endf

