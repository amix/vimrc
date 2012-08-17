" var.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2009-02-15.
" @Revision:    0.0.23

if &cp || exists("loaded_tlib_var_autoload")
    finish
endif
let loaded_tlib_var_autoload = 1


" Define a variable called NAME if yet undefined.
" You can also use the :TLLet command.
"
" EXAMPLES: >
"   exec tlib#var#Let('g:foo', 1)
"   TLet g:foo = 1
function! tlib#var#Let(name, val) "{{{3
    return printf('if !exists(%s) | let %s = %s | endif', string(a:name), a:name, string(a:val))
    " return printf('if !exists(%s) | let %s = %s | endif', string(a:name), a:name, a:val)
endf


" :def: function! tlib#var#EGet(var, namespace, ?default='')
" Retrieve a variable by searching several namespaces.
"
" EXAMPLES: >
"   let g:foo = 1
"   let b:foo = 2
"   let w:foo = 3
"   echo eval(tlib#var#EGet('foo', 'vg'))  => 1
"   echo eval(tlib#var#EGet('foo', 'bg'))  => 2
"   echo eval(tlib#var#EGet('foo', 'wbg')) => 3
function! tlib#var#EGet(var, namespace, ...) "{{{3
    let pre  = []
    let post = []
    for namespace in split(a:namespace, '\zs')
        let var = namespace .':'. a:var
        call add(pre,  printf('exists("%s") ? %s : (', var, var))
        call add(post, ')')
    endfor
    let default = a:0 >= 1 ? a:1 : ''
    return join(pre) . string(default) . join(post)
endf


" :def: function! tlib#var#Get(var, namespace, ?default='')
" Retrieve a variable by searching several namespaces.
"
" EXAMPLES: >
"   let g:foo = 1
"   let b:foo = 2
"   let w:foo = 3
"   echo tlib#var#Get('foo', 'bg')  => 1
"   echo tlib#var#Get('foo', 'bg')  => 2
"   echo tlib#var#Get('foo', 'wbg') => 3
function! tlib#var#Get(var, namespace, ...) "{{{3
    for namespace in split(a:namespace, '\zs')
        let var = namespace .':'. a:var
        if exists(var)
            return eval(var)
        endif
    endfor
    return a:0 >= 1 ? a:1 : ''
endf


" :def: function! tlib#var#List(rx, ?prefix='')
" Get a list of variables matching rx.
" EXAMPLE:
"   echo tlib#var#List('tlib_', 'g:')
function! tlib#var#List(rx, ...) "{{{3
    TVarArg ['prefix', '']
    redir => vars
    silent! exec 'let '. prefix
    redir END
    let varlist = split(vars, '\n')
    call map(varlist, 'matchstr(v:val, ''^\S\+'')')
    call filter(varlist, 'v:val =~ a:rx')
    return varlist
endf

