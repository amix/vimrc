" Filter_fuzzy.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-11-25.
" @Last Change: 2013-09-25.
" @Revision:    0.0.47

let s:prototype = tlib#Filter_cnf#New({'_class': ['Filter_fuzzy'], 'name': 'fuzzy'}) "{{{2
let s:prototype.highlight = g:tlib#input#higroup


" Support for "fuzzy" pattern matching in |tlib#input#List()|. 
" Patterns are interpreted as if characters were connected with '.\{-}'.
"
" In "fuzzy" mode, the pretty printing of filenames is disabled.
function! tlib#Filter_fuzzy#New(...) "{{{3
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    return object
endf


" :nodoc:
function! s:prototype.Init(world) dict "{{{3
    " TLogVAR a:world.display_format
    " :nodoc:
    function! a:world.Set_display_format(value) dict
        if a:value == 'filename'
            let self.display_format = ''
        else
            let self.display_format = a:value
        endif
    endf
endf


let s:Help = s:prototype.Help

" :nodoc:
function! s:prototype.Help(world) dict "{{{3
    call call(s:Help, [a:world], self)
    call a:world.PushHelp('Patterns are interpreted as if characters were connected with .\{-}')
endf


" :nodoc:
function! s:prototype.DisplayFilter(filter) dict "{{{3
    " TLogVAR a:filter
    let filter1 = deepcopy(a:filter)
    call map(filter1, '"{". join(reverse(v:val), " OR ") ."}"')
    return join(reverse(filter1), ' AND ')
endf


" :nodoc:
function! s:prototype.SetFrontFilter(world, pattern) dict "{{{3
    let a:world.filter[0] = map(reverse(split(a:pattern, '\s*|\s*')), 'join(map(split(v:val, ''\zs''), ''tlib#rx#Escape(v:val, "V")''), ''\.\{-}'')') + a:world.filter[0][1 : -1]
    endif
endf


" :nodoc:
function! s:prototype.PushFrontFilter(world, char) dict "{{{3
    let ch = tlib#rx#Escape(nr2char(a:char), 'V')
    if empty(a:world.filter[0][0])
        let a:world.filter[0][0] .= ch
    else
        let a:world.filter[0][0] .= '\.\{-}'. ch
    endif
endf


" :nodoc:
function! s:prototype.ReduceFrontFilter(world) dict "{{{3
    let a:world.filter[0][0] = substitute(a:world.filter[0][0], '\(\\\.\\{-}\)\?.$', '', '')
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict "{{{3
    return substitute(a:filter, '\\\.\\{-}', '', 'g')
endf

