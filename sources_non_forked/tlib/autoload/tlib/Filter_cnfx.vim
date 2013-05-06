" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-11-25.
" @Last Change: 2012-09-20.
" @Revision:    0.0.61

let s:prototype = tlib#Filter_cnfd#New({'_class': ['Filter_cnfx'], 'name': 'cnfx'}) "{{{2
let s:prototype.highlight = g:tlib_inputlist_higroup


" A character that should be expanded to '\.\{-}'.
TLet g:tlib#Filter_cnfx#expander = '+'


" The same as |tlib#Filter_cnfd#New()| but a a customizable character 
" |see tlib#Filter_cnfx#expander| is expanded to '\.\{-}'. 
" The pattern is a '/\V' very no-'/magic' regexp pattern.
function! tlib#Filter_cnfx#New(...) "{{{3
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    return object
endf


" :nodoc:
function! s:prototype.SetFrontFilter(world, pattern) dict "{{{3
    let pattern = substitute(a:pattern, tlib#rx#Escape(g:tlib#Filter_cnfx#expander, 'V'), '\\.\\{-}', 'g')
    let a:world.filter[0] = reverse(split(pattern, '\s*|\s*')) + a:world.filter[0][1 : -1]
endf


" :nodoc:
function! s:prototype.PushFrontFilter(world, char) dict "{{{3
    let a:world.filter[0][0] .= a:char == char2nr(g:tlib#Filter_cnfx#expander) ? '\.\{-}' : nr2char(a:char)
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict "{{{3
    return substitute(a:filter, '\\.\\{-}', g:tlib#Filter_cnfx#expander, 'g')
endf

