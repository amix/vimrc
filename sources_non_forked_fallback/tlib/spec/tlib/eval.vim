" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2015-10-26.
" @Last Change: 2015-10-26.

let s:save_cpo = &cpo
set cpo&vim


SpecBegin 'title': 'tlib#eval'


let g:eval_a = {'foo': range(0, 5), 'd': {'a': range(0, 5)}}
let g:eval_b = {'foo': range(6, 10), 'd': {'a': range(6, 10), 'b': 2}, 'bar': range(5)}
let g:eval_a0 = deepcopy(g:eval_a)
let g:eval_b0 = deepcopy(g:eval_b)
let g:eval_c = {'foo': range(0, 10), 'd': {'a': range(0, 10), 'b': 2}, 'bar': range(5)}


Should be equal tlib#eval#Extend(copy(g:eval_a), g:eval_b), g:eval_c
Should be equal g:eval_a, g:eval_a0
Should be equal g:eval_b, g:eval_b0


let &cpo = s:save_cpo
unlet s:save_cpo
