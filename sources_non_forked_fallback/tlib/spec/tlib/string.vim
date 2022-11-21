" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-03.
" @Last Change: 2010-04-03.
" @Revision:    4

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib#string'

Should be equal tlib#string#RemoveBackslashes('foo bar'), 'foo bar'
Should be equal tlib#string#RemoveBackslashes('foo\ bar'), 'foo bar'
Should be equal tlib#string#RemoveBackslashes('foo\ \\bar'), 'foo \\bar'
Should be equal tlib#string#RemoveBackslashes('foo\ \\bar', '\ '), 'foo \bar'


Should be equal tlib#string#Count("fooo", "o"), 3
Should be equal tlib#string#Count("***", "\\*"), 3
Should be equal tlib#string#Count("***foo", "\\*"), 3
Should be equal tlib#string#Count("foo***", "\\*"), 3



let &cpo = s:save_cpo
unlet s:save_cpo
