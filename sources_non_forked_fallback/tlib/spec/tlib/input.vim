" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-28.
" @Last Change: 2009-03-14.
" @Revision:    73

let s:save_cpo = &cpo
set cpo&vim


SpecBegin 'title': 'tlib: Input', 'scratch': '%',
            \ 'after': ':unlet! g:spec_lib_rv',
            \ 'options': [
			\ 'vim',
			\ ]


let g:spec_tlib_list = [10, 20, 30, 40, 'a50', 'aa60', 'b70', 'ba80', 90]



It should return empty values when the user presses <escape>.
Replay :let g:spec_lib_rv = tlib#input#List('s', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<esc>
Should be#Equal g:spec_lib_rv, ''

Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<esc>
Should be#Equal g:spec_lib_rv, []

Replay :let g:spec_lib_rv = tlib#input#List('si', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<esc>
Should be#Equal g:spec_lib_rv, 0



It should pick an item from s-type list.
Replay :let g:spec_lib_rv = tlib#input#List('s', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<cr>
Should be#Equal g:spec_lib_rv, 30



It should return an index from si-type list.
Replay :let g:spec_lib_rv = tlib#input#List('si', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<cr>
Should be#Equal g:spec_lib_rv, 3



It should return a list from a m-type list.
Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
            \ \<Down>#\<Down>\<Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), [20, 40]

Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
           \ \<Down>\<S-Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), [20, 30]

Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<S-up>\<cr>
Should be#Equal sort(g:spec_lib_rv), [20, 30]



It should return a list of indices from a mi-type list.
Replay :let g:spec_lib_rv = tlib#input#List('mi', '', g:spec_tlib_list)\<cr>
            \ \<Down>#\<Down>\<Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), [2, 4]

Replay :let g:spec_lib_rv = tlib#input#List('mi', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<S-Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), [2, 3]

Replay :let g:spec_lib_rv = tlib#input#List('mi', '', g:spec_tlib_list)\<cr>
            \ \<Down>\<Down>\<S-up>\<cr>
Should be#Equal sort(g:spec_lib_rv), [2, 3]



It should filter items from a s-type list.
Replay :let g:spec_lib_rv = tlib#input#List('s', '', g:spec_tlib_list)\<cr>
            \ \<Down>a\<Down>\<cr>
Should be#Equal g:spec_lib_rv, 'aa60'



It should filter items from a si-type list.
Replay :let g:spec_lib_rv = tlib#input#List('si', '', g:spec_tlib_list)\<cr>
            \ \<Down>a\<Down>\<cr>
Should be#Equal g:spec_lib_rv, 6



It should filter items from a m-type list.
Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
            \ a\<Down>#\<Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), ['aa60', 'ba80']

Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
            \ a\<Down>\<S-Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), ['aa60', 'ba80']

Replay :let g:spec_lib_rv = tlib#input#List('m', '', g:spec_tlib_list)\<cr>
            \ a\<Down>\<Down>\<S-up>\<cr>
Should be#Equal sort(g:spec_lib_rv), ['aa60', 'ba80']



It should filter items from a mi-type list.
Replay :let g:spec_lib_rv = tlib#input#List('mi', '', g:spec_tlib_list)\<cr>
            \ a\<Down>#\<Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), [6, 8]

Replay :let g:spec_lib_rv = tlib#input#List('mi', '', g:spec_tlib_list)\<cr>
            \ a\<Down>\<S-Down>\<cr>
Should be#Equal sort(g:spec_lib_rv), [6, 8]

Replay :let g:spec_lib_rv = tlib#input#List('mi', '', g:spec_tlib_list)\<cr>
            \ a\<Down>\<Down>\<S-up>\<cr>
Should be#Equal sort(g:spec_lib_rv), [6, 8]



let &cpo = s:save_cpo
