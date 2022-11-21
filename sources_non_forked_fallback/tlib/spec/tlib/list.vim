" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-03.
" @Last Change: 2010-04-03.
" @Revision:    4

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'tlib: List'
            " \, 'options': [vim, <+SET+>]
            " \, 'sfile': '<+SCRIPT CONTEXT+>'
            " \, 'scratch': '<+SCRATCH FILE+>'
            " \, 'before': '<+BEFORE EX COMMANDS+>'
            " \, 'after': '<+AFTER EX COMMANDS+>'
            " \, 'cleanup': ['<+FUNCTION+>()']


" List {{{2
fun! Add(a,b)
    return a:a + a:b
endf

Should be equal tlib#list#Inject([], 0, function('Add')), 0
Should be equal tlib#list#Inject([1,2,3], 0, function('Add')), 6

Should be equal tlib#list#Compact([]), []
Should be equal tlib#list#Compact([0,1,2,3,[], {}, ""]), [1,2,3]

Should be equal tlib#list#Flatten([]), []
Should be equal tlib#list#Flatten([1,2,3]), [1,2,3]
Should be equal tlib#list#Flatten([1,2, [1,2,3], 3]), [1,2,1,2,3,3]
Should be equal tlib#list#Flatten([0,[1,2,[3,""]]]), [0,1,2,3,""]

Should be equal tlib#list#FindAll([1,2,3], 'v:val >= 2'), [2,3]
Should be equal tlib#list#FindAll([1,2,3], 'v:val >= 2', 'v:val * 10'), [20,30]

Should be equal tlib#list#Find([1,2,3], 'v:val >= 2'), 2
Should be equal tlib#list#Find([1,2,3], 'v:val >= 2', 0, 'v:val * 10'), 20
Should be equal tlib#list#Find([1,2,3], 'v:val >= 5', 10), 10

Should be equal tlib#list#Any([1,2,3], 'v:val >= 2'), 1
Should be equal tlib#list#Any([1,2,3], 'v:val >= 5'), 0

Should be equal tlib#list#All([1,2,3], 'v:val < 5'), 1
Should be equal tlib#list#All([1,2,3], 'v:val >= 2'), 0

Should be equal tlib#list#Remove([1,2,1,2], 2), [1,1,2]
Should be equal tlib#list#RemoveAll([1,2,1,2], 2), [1,1]

Should be equal tlib#list#Zip([[1,2,3], [4,5,6]]), [[1,4], [2,5], [3,6]]
Should be equal tlib#list#Zip([[1,2,3], [4,5,6,7]]), [[1,4], [2,5], [3,6], ['', 7]]
Should be equal tlib#list#Zip([[1,2,3], [4,5,6,7]], -1), [[1,4], [2,5], [3,6], [-1,7]]
Should be equal tlib#list#Zip([[1,2,3,7], [4,5,6]], -1), [[1,4], [2,5], [3,6], [7,-1]]


Should be equal tlib#list#Uniq([]), []
Should be equal tlib#list#Uniq([1,1]), [1]
Should be equal tlib#list#Uniq([1,2,2,3,2,3,4,2,1,7,2,3,2,3,7]), [1,2,3,4,7]



let &cpo = s:save_cpo
unlet s:save_cpo
