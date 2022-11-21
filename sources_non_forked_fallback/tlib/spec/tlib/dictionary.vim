" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2016-04-03.
" @Last Change: 2016-04-03.

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib#dictionary'


It should handle basic test cases for tlib#dictionary#Rev properly.

Should be equal tlib#dictionary#Rev({}), {}
Should be equal tlib#dictionary#Rev({1: 2, 3: 4}), {'2': '1', '4': '3'}
Should be equal tlib#dictionary#Rev({1: '', 3: 4}, {'empty': '*'}), {'*': '1', '4': '3'}
Should be equal tlib#dictionary#Rev({1: '', 3: 4}, {'use_string': 1}), {'''''': '1', '4': '3'}
Should be equal tlib#dictionary#Rev({1: '', 3: 4}, {'use_string': 1, 'use_eval': 1}), {'''''': 1, '4': 3}
Should be equal tlib#dictionary#Rev(tlib#dictionary#Rev({1: '', 3: 4}, {'use_string': 1}), {'use_eval': 1}), {1: '', 3: 4}
Should be equal tlib#dictionary#Rev({1: 4, 2: 4}, {'values_as_list': 1}), {'4': ['1', '2']}
Should be equal tlib#dictionary#Rev({1: 4, 2: 4}, {'values_as_list': 1, 'use_eval': 1}), {'4': [1, 2]}


let &cpo = s:save_cpo
unlet s:save_cpo
