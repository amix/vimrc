" Test.vim -- A test class
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-05-01.
" @Last Change: 2010-09-05.
" @Revision:    0.1.10

" :enddoc:

if &cp || exists("loaded_tlib_Test_autoload")
    finish
endif
let loaded_tlib_Test_autoload = 1


let s:prototype = tlib#Object#New({'_class': ['Test']}) "{{{2
function! tlib#Test#New(...) "{{{3
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    return object
endf


function! s:prototype.Dummy() dict "{{{3
    return 'Test.vim'
endf

