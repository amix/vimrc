" list.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2015-10-21.
" @Revision:    61


""" List related functions {{{1
" For the following functions please see ../../test/tlib.vim for examples.

" :def: function! tlib#list#Inject(list, initial_value, funcref)
" EXAMPLES: >
"   echo tlib#list#Inject([1,2,3], 0, function('Add')
"   => 6
function! tlib#list#Inject(list, value, Function) "{{{3
    if empty(a:list)
        return a:value
    else
        let item  = a:list[0]
        let rest  = a:list[1:-1]
        let value = call(a:Function, [a:value, item])
        return tlib#list#Inject(rest, value, a:Function)
    endif
endf


" EXAMPLES: >
"   tlib#list#Compact([0,1,2,3,[], {}, ""])
"   => [1,2,3]
function! tlib#list#Compact(list) "{{{3
    return filter(copy(a:list), '!empty(v:val)')
endf


" EXAMPLES: >
"   tlib#list#Flatten([0,[1,2,[3,""]]])
"   => [0,1,2,3,""]
function! tlib#list#Flatten(list) "{{{3
    let acc = []
    for e in a:list
        if type(e) == 3
            let acc += tlib#list#Flatten(e)
        else
            call add(acc, e)
        endif
        unlet e
    endfor
    return acc
endf


" :def: function! tlib#list#FindAll(list, filter, ?process_expr="")
" Basically the same as filter()
"
" EXAMPLES: >
"   tlib#list#FindAll([1,2,3], 'v:val >= 2')
"   => [2, 3]
function! tlib#list#FindAll(list, filter, ...) "{{{3
    let rv   = filter(copy(a:list), a:filter)
    if a:0 >= 1 && a:1 != ''
        let rv = map(rv, a:1)
    endif
    return rv
endf


" :def: function! tlib#list#Find(list, filter, ?default="", ?process_expr="")
"
" EXAMPLES: >
"   tlib#list#Find([1,2,3], 'v:val >= 2')
"   => 2
function! tlib#list#Find(list, filter, ...) "{{{3
    let default = a:0 >= 1 ? a:1 : ''
    let expr    = a:0 >= 2 ? a:2 : ''
    return get(tlib#list#FindAll(a:list, a:filter, expr), 0, default)
endf


" EXAMPLES: >
"   tlib#list#Any([1,2,3], 'v:val >= 2')
"   => 1
function! tlib#list#Any(list, expr) "{{{3
    return !empty(tlib#list#FindAll(a:list, a:expr))
endf


" EXAMPLES: >
"   tlib#list#All([1,2,3], 'v:val >= 2')
"   => 0
function! tlib#list#All(list, expr) "{{{3
    return len(tlib#list#FindAll(a:list, a:expr)) == len(a:list)
endf


" EXAMPLES: >
"   tlib#list#Remove([1,2,1,2], 2)
"   => [1,1,2]
function! tlib#list#Remove(list, element) "{{{3
    let idx = index(a:list, a:element)
    if idx != -1
        call remove(a:list, idx)
    endif
    return a:list
endf


" EXAMPLES: >
"   tlib#list#RemoveAll([1,2,1,2], 2)
"   => [1,1]
function! tlib#list#RemoveAll(list, element) "{{{3
    call filter(a:list, 'v:val != a:element')
    return a:list
endf


" :def: function! tlib#list#Zip(lists, ?default='')
" EXAMPLES: >
"   tlib#list#Zip([[1,2,3], [4,5,6]])
"   => [[1,4], [2,5], [3,6]]
function! tlib#list#Zip(lists, ...) "{{{3
    TVarArg 'default'
    let lists = copy(a:lists)
    let max   = 0
    for l in lists
        let ll = len(l)
        if ll > max
            let max = ll
        endif
    endfor
    " TLogVAR default, max
    return map(range(0, max - 1), 's:GetNthElement(v:val, lists, default)')
endf

function! s:GetNthElement(n, lists, default) "{{{3
    " TLogVAR a:n, a:lists, a:default
    return map(copy(a:lists), 'get(v:val, a:n, a:default)')
endf


function! tlib#list#Uniq(list, ...) "{{{3
    " TLogVAR a:list
    TVarArg ['get_value', ''], ['remove_empty', 0]
    if remove_empty
        call filter(a:list, 'type(v:val) == 0 || !empty(v:val)')
    endif
    " CREDITS: Based on syntastic#util#unique(list) by scrooloose
    let seen = {}
    let uniques = []
    if empty(get_value)
        for e in a:list
            if !has_key(seen, e)
                let seen[e] = 1
                call add(uniques, e)
            endif
            unlet e
        endfor
    else
        for e in a:list
            let v = eval(printf(get_value, string(e)))
            if !has_key(seen, v)
                let seen[v] = 1
                call add(uniques, e)
            endif
            unlet e
        endfor
    endif
    return uniques
endf


function! tlib#list#ToDictionary(list, default, ...) "{{{3
    TVarArg ['generator', '']
    let dict = {}
    for item in a:list
        if !empty(item)
            let dict[item] = empty(generator) ? a:default : call(generator, [item, a:default])
        endif
    endfor
    return dict
endf

