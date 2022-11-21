" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @GIT:         http://github.com/tomtom/tlib_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2015-11-09.
" @Revision:    10

if &cp || exists("loaded_tlib_macros")
    finish
endif
let loaded_tlib_macros = 1

let s:save_cpo = &cpo
set cpo&vim


" :display: :TRequire NAME [VERSION [FILE]]
" Make a certain vim file is loaded.
"
" Conventions: If FILE isn't defined, plugin/NAME.vim is loaded. The 
" file must provide a variable loaded_{NAME} that represents the version 
" number.
command! -nargs=+ TRequire let s:require = [<f-args>]
            \ | if !exists('loaded_'. get(s:require, 0))
                \ | exec 'runtime '. get(s:require, 2, 'plugin/'. get(s:require, 0) .'.vim')
                \ | if !exists('loaded_'. get(s:require, 0)) || loaded_{get(s:require, 0)} < get(s:require, 1, loaded_{get(s:require, 0)})
                    \ | echoerr 'Require '.  get(s:require, 0) .' >= '. get(s:require, 1, 'any version will do')
                    \ | finish
                    \ | endif
                \ | endif | unlet s:require


" :display: :Ttimecommand CMD
" Time the execution time of CMD.
command! -nargs=1 -complete=command Ttimecommand call tlib#cmd#Time(<q-args>)


let &cpo = s:save_cpo
unlet s:save_cpo
