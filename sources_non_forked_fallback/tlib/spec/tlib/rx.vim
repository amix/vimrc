" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-03.
" @Last Change: 2010-04-03.
" @Revision:    2

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib#rx'


for c in split('^$.*+\()|{}[]~', '\zs')
    let s = printf('%sfoo%sbar%s', c, c, c)
    Should be like s, '\m^'. tlib#rx#Escape(s, 'm') .'$'
    Should be like s, '\M^'. tlib#rx#Escape(s, 'M') .'$'
    Should be like s, '\v^'. tlib#rx#Escape(s, 'v') .'$'
    Should be like s, '\V\^'. tlib#rx#Escape(s, 'V') .'\$'
endfor



let &cpo = s:save_cpo
unlet s:save_cpo
