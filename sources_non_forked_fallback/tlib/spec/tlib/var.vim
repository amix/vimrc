" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-03.
" @Last Change: 2010-04-03.
" @Revision:    2

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib#var'


let g:foo = 1
let g:bar = 2
let b:bar = 3
let s:bar = 4

Should be equal tlib#var#Get('bar', 'bg'), 3
Should be equal tlib#var#Get('bar', 'g'), 2
Should be equal tlib#var#Get('foo', 'bg'), 1
Should be equal tlib#var#Get('foo', 'g'), 1
Should be equal tlib#var#Get('none', 'l'), ''

Should be equal eval(tlib#var#EGet('bar', 'bg')), 3
Should be equal eval(tlib#var#EGet('bar', 'g')), 2
" Should be equal eval(tlib#var#EGet('bar', 'sg')), 4
Should be equal eval(tlib#var#EGet('foo', 'bg')), 1
Should be equal eval(tlib#var#EGet('foo', 'g')), 1
Should be equal eval(tlib#var#EGet('none', 'l')), ''



let &cpo = s:save_cpo
unlet s:save_cpo
