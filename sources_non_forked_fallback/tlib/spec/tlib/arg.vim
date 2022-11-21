" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-03.
" @Last Change: 2010-04-03.
" @Revision:    1

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib#arg'

function! TestGetArg(...) "{{{3
    exec tlib#arg#Get(1, 'foo', 1)
    return foo
endf

function! TestGetArg1(...) "{{{3
    exec tlib#arg#Get(1, 'foo', 1, '!= ""')
    return foo
endf

Should be equal TestGetArg(), 1
Should be equal TestGetArg(''), ''
Should be equal TestGetArg(2), 2
Should be equal TestGetArg1(), 1
Should be equal TestGetArg1(''), 1
Should be equal TestGetArg1(2), 2

function! TestArgs(...) "{{{3
    exec tlib#arg#Let([['foo', "o"], ['bar', 2]])
    return repeat(foo, bar)
endf
Should be equal TestArgs(), 'oo'
Should be equal TestArgs('a'), 'aa'
Should be equal TestArgs('a', 3), 'aaa'

function! TestArgs1(...) "{{{3
    exec tlib#arg#Let(['foo', ['bar', 2]])
    return repeat(foo, bar)
endf
Should be equal TestArgs1(), ''
Should be equal TestArgs1('a'), 'aa'
Should be equal TestArgs1('a', 3), 'aaa'

function! TestArgs2(...) "{{{3
    exec tlib#arg#Let(['foo', 'bar'], 1)
    return repeat(foo, bar)
endf
Should be equal TestArgs2(), '1'
Should be equal TestArgs2('a'), 'a'
Should be equal TestArgs2('a', 3), 'aaa'

function! TestArgs3(...)
    TVarArg ['a', 1], 'b'
    return a . b
endf
Should be equal TestArgs3(), '1'
Should be equal TestArgs3('a'), 'a'
Should be equal TestArgs3('a', 3), 'a3'


let &cpo = s:save_cpo
unlet s:save_cpo
