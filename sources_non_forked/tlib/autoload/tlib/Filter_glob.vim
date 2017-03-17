" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-11-25.
" @Last Change: 2014-11-18.
" @Revision:    0.0.82

let s:prototype = tlib#Filter_cnf#New({'_class': ['Filter_glob'], 'name': 'glob'}) "{{{2
let s:prototype.highlight = g:tlib#input#higroup


" A character that should be expanded to '\.\{-}'.
TLet g:tlib#Filter_glob#seq = '*'


" A character that should be expanded to '\.\?'.
TLet g:tlib#Filter_glob#char = '?'


" The same as |tlib#Filter_cnf#New()| but a a customizable character 
" |see tlib#Filter_glob#seq| is expanded to '\.\{-}' and 
" |g:tlib#Filter_glob#char| is expanded to '\.'.
" The pattern is a '/\V' very no-'/magic' regexp pattern.
function! tlib#Filter_glob#New(...) "{{{3
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    return object
endf


let s:Help = s:prototype.Help

" :nodoc:
function! s:prototype.Help(world) dict "{{{3
    call call(s:Help, [a:world], self)
    call a:world.PushHelp(g:tlib#Filter_glob#seq, 'Any characters')
    call a:world.PushHelp(g:tlib#Filter_glob#char, 'Single characters')
endf


" :nodoc:
function! s:prototype.SetFrontFilter(world, pattern) dict "{{{3
    let pattern = substitute(a:pattern, tlib#rx#Escape(g:tlib#Filter_glob#seq, 'V'), '\\.\\{-}', 'g')
    let pattern = substitute(a:pattern, tlib#rx#Escape(g:tlib#Filter_glob#char, 'V'), '\\.', 'g')
    let a:world.filter[0] = reverse(split(pattern, '\s*|\s*')) + a:world.filter[0][1 : -1]
endf


" :nodoc:
function! s:prototype.PushFrontFilter(world, char) dict "{{{3
    " TLogVAR a:char, nr2char(a:char)
    if a:char == char2nr(g:tlib#Filter_glob#seq)
        let char = '\.\{-}'
    elseif a:char == char2nr(g:tlib#Filter_glob#char)
        let char = '\.'
    else
        let char = nr2char(a:char)
    endif
    let a:world.filter[0][0] .= char
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict "{{{3
    let filter = substitute(a:filter, '\\\.\\{-}', g:tlib#Filter_glob#seq, 'g')
    let filter = substitute(filter, '\\\.', g:tlib#Filter_glob#char, 'g')
    return filter
endf

