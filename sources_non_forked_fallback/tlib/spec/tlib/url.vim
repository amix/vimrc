" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-03.
" @Last Change: 2010-04-03.
" @Revision:    2

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib#url'

Should be equal tlib#url#Decode('http://example.com/foo+bar%25bar'), 'http://example.com/foo bar%bar'
Should be equal tlib#url#Decode('Hello%20World.%20%20Good%2c%20bye.'), 'Hello World.  Good, bye.'

Should be equal tlib#url#Encode('foo bar%bar'), 'foo+bar%%bar'
Should be equal tlib#url#Encode('Hello World. Good, bye.'), 'Hello+World.+Good%2c+bye.'


let &cpo = s:save_cpo
unlet s:save_cpo
