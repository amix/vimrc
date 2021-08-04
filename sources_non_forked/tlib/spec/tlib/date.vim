" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-09-17.
" @Last Change: 2016-03-16.
" @Revision:    21

SpecBegin 'title': 'tlib#date'

Should be equal tlib#date#Parse('2000-1-0', 1), [2000, 1, 0]
Should be equal tlib#date#Parse('2000-1-2'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000-01-02'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000-10-20'), [2000, 10, 20]

Should be equal tlib#date#Parse('00-1-0', 1), [2000, 1, 0]
Should be equal tlib#date#Parse('00-1-2'), [2000, 1, 2]
Should be equal tlib#date#Parse('00-01-02'), [2000, 1, 2]
Should be equal tlib#date#Parse('00-10-20'), [2000, 10, 20]

Should be equal tlib#date#Parse('2000/2/1'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000/02/01'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000/20/10'), [2000, 10, 20]

Should be equal tlib#date#Parse('00/2/1'), [2000, 1, 2]
Should be equal tlib#date#Parse('00/02/01'), [2000, 1, 2]
Should be equal tlib#date#Parse('00/20/10'), [2000, 10, 20]

Should be equal tlib#date#Parse('2.1.2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('2. 1. 2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('02.01.2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('02. 01. 2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('20.10.2000'), [2000, 10, 20]
Should be equal tlib#date#Parse('20. 10. 2000'), [2000, 10, 20]

Should throw exception "tlib#date#Parse('2000-14-2')", 'TLib: Invalid date'
Should throw exception "tlib#date#Parse('2000-011-02')", 'TLib: Invalid date'
Should throw exception "tlib#date#Parse('2000-10-40')", 'TLib: Invalid date'
Should throw exception "tlib#date#Parse('2000-10-0')", 'TLib: Invalid date'

Should be equal tlib#date#Shift('2015-10-29', '1m'), '2015-11-29'
Should be equal tlib#date#Shift('2015-11-29', '1m'), '2015-12-29'
Should be equal tlib#date#Shift('2015-12-29', '1m'), '2016-01-29'
Should be equal tlib#date#Shift('2016-01-29', '1m'), '2016-02-29'
Should be equal tlib#date#Shift('2015-10-29', '2m'), '2015-12-29'
Should be equal tlib#date#Shift('2015-10-29', '3m'), '2016-01-29'
Should be equal tlib#date#Shift('2015-10-29', '4m'), '2016-02-29'
Should be equal tlib#date#Shift('2015-12-30', '1d'), '2015-12-31'
Should be equal tlib#date#Shift('2015-12-31', '1d'), '2016-01-01'
Should be equal tlib#date#Shift('2015-12-30', '2d'), '2016-01-01'
Should be equal tlib#date#Shift('2015-12-30', '3d'), '2016-01-02'

Should be equal tlib#date#Shift('2016-03-16', '1b'), '2016-03-17'
Should be equal tlib#date#Shift('2016-03-16', '2b'), '2016-03-18'
Should be equal tlib#date#Shift('2016-03-16', '3b'), '2016-03-21'
Should be equal tlib#date#Shift('2016-03-16', '4b'), '2016-03-22'
Should be equal tlib#date#Shift('2016-03-16', '5b'), '2016-03-23'
Should be equal tlib#date#Shift('2016-03-16', '6b'), '2016-03-24'
Should be equal tlib#date#Shift('2016-03-16', '7b'), '2016-03-25'
Should be equal tlib#date#Shift('2016-03-16', '8b'), '2016-03-28'

