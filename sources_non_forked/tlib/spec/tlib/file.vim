" @Author:      Thomas Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-25.
" @Last Change: 2010-04-03.
" @Revision:    13

let s:save_cpo = &cpo
set cpo&vim


SpecBegin 'title': 'tlib/file',
            \ 'sfile': 'autoload/tlib/file.vim'



It should split filenames.
Should be equal tlib#file#Split('foo/bar/filename.txt'), ['foo', 'bar', 'filename.txt']
Should be equal tlib#file#Split('/foo/bar/filename.txt'), ['', 'foo', 'bar', 'filename.txt']
Should be equal tlib#file#Split('ftp://foo/bar/filename.txt'), ['ftp:/', 'foo', 'bar', 'filename.txt']


It should join filenames.
Should be#Equal tlib#file#Join(['foo', 'bar']), 'foo/bar'
Should be#Equal tlib#file#Join(['foo/', 'bar'], 1), 'foo/bar'
Should be#Equal tlib#file#Join(['', 'bar']), '/bar'
Should be#Equal tlib#file#Join(['/', 'bar'], 1), '/bar'
Should be#Equal tlib#file#Join(['foo', 'bar', 'filename.txt']), 'foo/bar/filename.txt'
Should be#Equal tlib#file#Join(['', 'foo', 'bar', 'filename.txt']), '/foo/bar/filename.txt'
Should be#Equal tlib#file#Join(['ftp:/', 'foo', 'bar', 'filename.txt']), 'ftp://foo/bar/filename.txt'
Should be#Equal tlib#file#Join(['ftp://', 'foo', 'bar', 'filename.txt'], 1), 'ftp://foo/bar/filename.txt'

Should be equal tlib#file#Join(['foo', 'bar', 'filename.txt']), 'foo/bar/filename.txt'
Should be equal tlib#file#Join(['', 'foo', 'bar', 'filename.txt']), '/foo/bar/filename.txt'
Should be equal tlib#file#Join(['ftp:/', 'foo', 'bar', 'filename.txt']), 'ftp://foo/bar/filename.txt'


It should construct relative path names.
Should be#Equal tlib#file#Relative('foo/bar/filename.txt', 'foo'), 'bar/filename.txt'
Should be#Equal tlib#file#Relative('foo/bar/filename.txt', 'foo/base'), '../bar/filename.txt'
Should be#Equal tlib#file#Relative('filename.txt', 'foo/base'), '../../filename.txt'
Should be#Equal tlib#file#Relative('/foo/bar/filename.txt', '/boo/base'), '../../foo/bar/filename.txt'
Should be#Equal tlib#file#Relative('/bar/filename.txt', '/boo/base'), '../../bar/filename.txt'
Should be#Equal tlib#file#Relative('/foo/bar/filename.txt', '/base'), '../foo/bar/filename.txt'
Should be#Equal tlib#file#Relative('c:/bar/filename.txt', 'x:/boo/base'), 'c:/bar/filename.txt'


Should be equal tlib#file#Relative('foo/bar/filename.txt', 'foo'), 'bar/filename.txt'
Should be equal tlib#file#Relative('foo/bar/filename.txt', 'foo/base'), '../bar/filename.txt'
Should be equal tlib#file#Relative('filename.txt', 'foo/base'), '../../filename.txt'
Should be equal tlib#file#Relative('/foo/bar/filename.txt', '/boo/base'), '../../foo/bar/filename.txt'
Should be equal tlib#file#Relative('/bar/filename.txt', '/boo/base'), '../../bar/filename.txt'
Should be equal tlib#file#Relative('/foo/bar/filename.txt', '/base'), '../foo/bar/filename.txt'
Should be equal tlib#file#Relative('c:/bar/filename.txt', 'x:/boo/base'), 'c:/bar/filename.txt'


let &cpo = s:save_cpo
unlet s:save_cpo
