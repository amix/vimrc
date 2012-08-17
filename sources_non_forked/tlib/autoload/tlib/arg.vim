" arg.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2009-02-15.
" @Revision:    0.0.50

if &cp || exists("loaded_tlib_arg_autoload")
    finish
endif
let loaded_tlib_arg_autoload = 1


" :def: function! tlib#arg#Get(n, var, ?default="", ?test='')
" Set a positional argument from a variable argument list.
" See tlib#string#RemoveBackslashes() for an example.
function! tlib#arg#Get(n, var, ...) "{{{3
    let default = a:0 >= 1 ? a:1 : ''
    let atest   = a:0 >= 2 ? a:2 : ''
    " TLogVAR default, atest
    if !empty(atest)
        let atest = ' && (a:'. a:n .' '. atest .')'
    endif
    let test = printf('a:0 >= %d', a:n) . atest
    return printf('let %s = %s ? a:%d : %s', a:var, test, a:n, string(default))
endf


" :def: function! tlib#arg#Let(list, ?default='')
" Set a positional arguments from a variable argument list.
" See tlib#input#List() for an example.
function! tlib#arg#Let(list, ...) "{{{3
    let default = a:0 >= 1 ? a:1 : ''
    let list = map(copy(a:list), 'type(v:val) == 3 ? v:val : [v:val, default]')
    let args = map(range(1, len(list)), 'call("tlib#arg#Get", [v:val] + list[v:val - 1])')
    return join(args, ' | ')
endf


" :def: function! tlib#arg#Key(dict, list, ?default='')
" See |:TKeyArg|.
function! tlib#arg#Key(list, ...) "{{{3
    let default = a:0 >= 1 ? a:1 : ''
    let dict = a:list[0]
    let list = map(copy(a:list[1:-1]), 'type(v:val) == 3 ? v:val : [v:val, default]')
    let args = map(list, '"let ". v:val[0] ." = ". string(get(dict, v:val[0], v:val[1]))')
    " TLogVAR dict, list, args
    return join(args, ' | ')
endf


" :def: function! tlib#arg#StringAsKeyArgs(string, ?keys=[], ?evaluate=0)
function! tlib#arg#StringAsKeyArgs(string, ...) "{{{1
    TVarArg ['keys', {}], ['evaluate', 0]
    let keyargs = {}
    let args = split(a:string, '\\\@<! ')
    let arglist = map(args, 'matchlist(v:val, ''^\(\w\+\):\(.*\)$'')')
    " TLogVAR a:string, args, arglist
    for matchlist in arglist
        if len(matchlist) < 3
            throw 'Malformed key arguments: '. string(matchlist) .' in '. a:string
        endif
        let [match, key, val; rest] = matchlist
        if empty(keys) || has_key(keys, key)
            let val = substitute(val, '\\\\', '\\', 'g')
            if evaluate
                let val = eval(val)
            endif
            let keyargs[key] = val
        else
            echom 'Unknown key: '. key .'='. val
        endif
    endfor
    return keyargs
endf



""" Command line {{{1

" :def: function! tlib#arg#Ex(arg, ?chars='%#! ')
" Escape some characters in a string.
"
" Use |fnamescape()| if available.
"
" EXAMPLES: >
"   exec 'edit '. tlib#arg#Ex('foo%#bar.txt')
function! tlib#arg#Ex(arg, ...) "{{{3
    if exists('*fnameescape') && a:0 == 0
        return fnameescape(a:arg)
    else
        " let chars = '%# \'
        let chars = '%#! '
        if a:0 >= 1
            let chars .= a:1
        endif
        return escape(a:arg, chars)
    endif
endf


