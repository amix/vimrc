" hook.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-08-21.
" @Last Change: 2009-02-15.
" @Revision:    0.0.10

if &cp || exists("loaded_tlib_hook_autoload")
    finish
endif
let loaded_tlib_hook_autoload = 1


" :def: function! tlib#hook#Run(hook, ?dict={})
" Execute dict[hook], w:{hook}, b:{hook}, or g:{hook} if existent.
function! tlib#hook#Run(hook, ...) "{{{3
    TVarArg ['dict', {}]
    if has_key(dict, a:hook)
        let hook = dict[a:hook]
    else
        let hook = tlib#var#Get(a:hook, 'wbg')
    endif
    if empty(hook)
        return 0
    else
        let world = dict
        exec hook
        return 1
    endif
endf


