" Filter_cnfd.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-11-25.
" @Last Change: 2014-01-23.
" @Revision:    0.0.57

let s:prototype = tlib#Filter_cnf#New({'_class': ['Filter_cnfd'], 'name': 'cnfd'}) "{{{2
let s:prototype.highlight = g:tlib#input#higroup


" The same as |tlib#Filter_cnf#New()| but a dot is expanded to '\.\{-}'. 
" As a consequence, patterns cannot match dots.
" The pattern is a '/\V' very no-'/magic' regexp pattern.
function! tlib#Filter_cnfd#New(...) "{{{3
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    return object
endf


" :nodoc:
function! s:prototype.Init(world) dict "{{{3
endf


let s:Help = s:prototype.Help

" :nodoc:
function! s:prototype.Help(world) dict "{{{3
    call call(s:Help, [a:world], self)
    call a:world.PushHelp('.', 'Any characters')
endf


" :nodoc:
function! s:prototype.SetFrontFilter(world, pattern) dict "{{{3
    let pattern = substitute(a:pattern, '\.', '\\.\\{-}', 'g')
    let a:world.filter[0] = reverse(split(pattern, '\s*|\s*')) + a:world.filter[0][1 : -1]
endf


" :nodoc:
function! s:prototype.PushFrontFilter(world, char) dict "{{{3
    let a:world.filter[0][0] .= a:char == 46 ? '\.\{-}' : nr2char(a:char)
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict "{{{3
    return substitute(a:filter, '\\.\\{-}', '.', 'g')
endf

