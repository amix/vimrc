" tLib.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=vim-tLib)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2006-12-17.
" @Last Change: 2008-11-23.
" @Revision:    129

if !exists("loaded_tassert")
    echoerr 'tAssert (vimscript #1730) is required'
endif


TAssertBegin! "tlib"


" List {{{2
fun! Add(a,b)
    return a:a + a:b
endf
TAssert IsEqual(tlib#list#Inject([], 0, function('Add')), 0)
TAssert IsEqual(tlib#list#Inject([1,2,3], 0, function('Add')), 6)
delfunction Add

TAssert IsEqual(tlib#list#Compact([]), [])
TAssert IsEqual(tlib#list#Compact([0,1,2,3,[], {}, ""]), [1,2,3])

TAssert IsEqual(tlib#list#Flatten([]), [])
TAssert IsEqual(tlib#list#Flatten([1,2,3]), [1,2,3])
TAssert IsEqual(tlib#list#Flatten([1,2, [1,2,3], 3]), [1,2,1,2,3,3])
TAssert IsEqual(tlib#list#Flatten([0,[1,2,[3,""]]]), [0,1,2,3,""])

TAssert IsEqual(tlib#list#FindAll([1,2,3], 'v:val >= 2'), [2,3])
TAssert IsEqual(tlib#list#FindAll([1,2,3], 'v:val >= 2', 'v:val * 10'), [20,30])

TAssert IsEqual(tlib#list#Find([1,2,3], 'v:val >= 2'), 2)
TAssert IsEqual(tlib#list#Find([1,2,3], 'v:val >= 2', 0, 'v:val * 10'), 20)
TAssert IsEqual(tlib#list#Find([1,2,3], 'v:val >= 5', 10), 10)

TAssert IsEqual(tlib#list#Any([1,2,3], 'v:val >= 2'), 1)
TAssert IsEqual(tlib#list#Any([1,2,3], 'v:val >= 5'), 0)

TAssert IsEqual(tlib#list#All([1,2,3], 'v:val < 5'), 1)
TAssert IsEqual(tlib#list#All([1,2,3], 'v:val >= 2'), 0)

TAssert IsEqual(tlib#list#Remove([1,2,1,2], 2), [1,1,2])
TAssert IsEqual(tlib#list#RemoveAll([1,2,1,2], 2), [1,1])

TAssert IsEqual(tlib#list#Zip([[1,2,3], [4,5,6]]), [[1,4], [2,5], [3,6]])
TAssert IsEqual(tlib#list#Zip([[1,2,3], [4,5,6,7]]), [[1,4], [2,5], [3,6], ['', 7]])
TAssert IsEqual(tlib#list#Zip([[1,2,3], [4,5,6,7]], -1), [[1,4], [2,5], [3,6], [-1,7]])
TAssert IsEqual(tlib#list#Zip([[1,2,3,7], [4,5,6]], -1), [[1,4], [2,5], [3,6], [7,-1]])


" Vars {{{2
let g:foo = 1
let g:bar = 2
let b:bar = 3
let s:bar = 4

TAssert IsEqual(tlib#var#Get('bar', 'bg'), 3)
TAssert IsEqual(tlib#var#Get('bar', 'g'), 2)
TAssert IsEqual(tlib#var#Get('foo', 'bg'), 1)
TAssert IsEqual(tlib#var#Get('foo', 'g'), 1)
TAssert IsEqual(tlib#var#Get('none', 'l'), '')

TAssert IsEqual(eval(tlib#var#EGet('bar', 'bg')), 3)
TAssert IsEqual(eval(tlib#var#EGet('bar', 'g')), 2)
" TAssert IsEqual(eval(tlib#var#EGet('bar', 'sg')), 4)
TAssert IsEqual(eval(tlib#var#EGet('foo', 'bg')), 1)
TAssert IsEqual(eval(tlib#var#EGet('foo', 'g')), 1)
TAssert IsEqual(eval(tlib#var#EGet('none', 'l')), '')

unlet g:foo
unlet g:bar
unlet b:bar



" Filenames {{{2
TAssert IsEqual(tlib#file#Split('foo/bar/filename.txt'), ['foo', 'bar', 'filename.txt'])
TAssert IsEqual(tlib#file#Split('/foo/bar/filename.txt'), ['', 'foo', 'bar', 'filename.txt'])
TAssert IsEqual(tlib#file#Split('ftp://foo/bar/filename.txt'), ['ftp:/', 'foo', 'bar', 'filename.txt'])

TAssert IsEqual(tlib#file#Join(['foo', 'bar', 'filename.txt']), 'foo/bar/filename.txt')
TAssert IsEqual(tlib#file#Join(['', 'foo', 'bar', 'filename.txt']), '/foo/bar/filename.txt')
TAssert IsEqual(tlib#file#Join(['ftp:/', 'foo', 'bar', 'filename.txt']), 'ftp://foo/bar/filename.txt')

TAssert IsEqual(tlib#file#Relative('foo/bar/filename.txt', 'foo'), 'bar/filename.txt')
TAssert IsEqual(tlib#file#Relative('foo/bar/filename.txt', 'foo/base'), '../bar/filename.txt')
TAssert IsEqual(tlib#file#Relative('filename.txt', 'foo/base'), '../../filename.txt')
TAssert IsEqual(tlib#file#Relative('/foo/bar/filename.txt', '/boo/base'), '../../foo/bar/filename.txt')
TAssert IsEqual(tlib#file#Relative('/bar/filename.txt', '/boo/base'), '../../bar/filename.txt')
TAssert IsEqual(tlib#file#Relative('/foo/bar/filename.txt', '/base'), '../foo/bar/filename.txt')
TAssert IsEqual(tlib#file#Relative('c:/bar/filename.txt', 'x:/boo/base'), 'c:/bar/filename.txt')



" Prototype-based programming {{{2
let test = tlib#Test#New()
TAssert test.IsA('Test')
TAssert !test.IsA('foo')
TAssert test.RespondTo('RespondTo')
TAssert !test.RespondTo('RespondToNothing')
let test1 = tlib#Test#New()
TAssert test.IsRelated(test1)
let testworld = tlib#World#New()
TAssert !test.IsRelated(testworld)

let testc = tlib#TestChild#New()
TAssert IsEqual(testc.Dummy(), 'TestChild.vim')
TAssert IsEqual(testc.Super('Dummy', []), 'Test.vim')



" Optional arguments {{{2
function! TestGetArg(...) "{{{3
    exec tlib#arg#Get(1, 'foo', 1)
    return foo
endf

function! TestGetArg1(...) "{{{3
    exec tlib#arg#Get(1, 'foo', 1, '!= ""')
    return foo
endf

TAssert IsEqual(TestGetArg(), 1)
TAssert IsEqual(TestGetArg(''), '')
TAssert IsEqual(TestGetArg(2), 2)
TAssert IsEqual(TestGetArg1(), 1)
TAssert IsEqual(TestGetArg1(''), 1)
TAssert IsEqual(TestGetArg1(2), 2)

function! TestArgs(...) "{{{3
    exec tlib#arg#Let([['foo', "o"], ['bar', 2]])
    return repeat(foo, bar)
endf
TAssert IsEqual(TestArgs(), 'oo')
TAssert IsEqual(TestArgs('a'), 'aa')
TAssert IsEqual(TestArgs('a', 3), 'aaa')

function! TestArgs1(...) "{{{3
    exec tlib#arg#Let(['foo', ['bar', 2]])
    return repeat(foo, bar)
endf
TAssert IsEqual(TestArgs1(), '')
TAssert IsEqual(TestArgs1('a'), 'aa')
TAssert IsEqual(TestArgs1('a', 3), 'aaa')

function! TestArgs2(...) "{{{3
    exec tlib#arg#Let(['foo', 'bar'], 1)
    return repeat(foo, bar)
endf
TAssert IsEqual(TestArgs2(), '1')
TAssert IsEqual(TestArgs2('a'), 'a')
TAssert IsEqual(TestArgs2('a', 3), 'aaa')

function! TestArgs3(...)
    TVarArg ['a', 1], 'b'
    return a . b
endf
TAssert IsEqual(TestArgs3(), '1')
TAssert IsEqual(TestArgs3('a'), 'a')
TAssert IsEqual(TestArgs3('a', 3), 'a3')

delfunction TestGetArg
delfunction TestGetArg1
delfunction TestArgs
delfunction TestArgs1
delfunction TestArgs2
delfunction TestArgs3



" Strings {{{2
TAssert IsString(tlib#string#RemoveBackslashes('foo bar'))
TAssert IsEqual(tlib#string#RemoveBackslashes('foo bar'), 'foo bar')
TAssert IsEqual(tlib#string#RemoveBackslashes('foo\ bar'), 'foo bar')
TAssert IsEqual(tlib#string#RemoveBackslashes('foo\ \\bar'), 'foo \\bar')
TAssert IsEqual(tlib#string#RemoveBackslashes('foo\ \\bar', '\ '), 'foo \bar')



" Regexp {{{2
for c in split('^$.*+\()|{}[]~', '\zs')
    let s = printf('%sfoo%sbar%s', c, c, c)
    TAssert (s =~ '\m^'. tlib#rx#Escape(s, 'm') .'$')
    TAssert (s =~ '\M^'. tlib#rx#Escape(s, 'M') .'$')
    TAssert (s =~ '\v^'. tlib#rx#Escape(s, 'v') .'$')
    TAssert (s =~ '\V\^'. tlib#rx#Escape(s, 'V') .'\$')
endfor


" Encode, decode
TAssert IsEqual(tlib#url#Decode('http://example.com/foo+bar%25bar'), 'http://example.com/foo bar%bar')
TAssert IsEqual(tlib#url#Decode('Hello%20World.%20%20Good%2c%20bye.'), 'Hello World.  Good, bye.')

TAssert IsEqual(tlib#url#Encode('foo bar%bar'), 'foo+bar%%bar')
TAssert IsEqual(tlib#url#Encode('Hello World. Good, bye.'), 'Hello+World.+Good%2c+bye.')

TAssertEnd test test1 testc testworld


TAssert IsEqual(tlib#string#Count("fooo", "o"), 3)
TAssert IsEqual(tlib#string#Count("***", "\\*"), 3)
TAssert IsEqual(tlib#string#Count("***foo", "\\*"), 3)
TAssert IsEqual(tlib#string#Count("foo***", "\\*"), 3)


finish "{{{1


" Input {{{2
echo tlib#input#List('s', 'Test', ['barfoobar', 'barFoobar'])
echo tlib#input#List('s', 'Test', ['barfoobar', 'bar foo bar', 'barFoobar'])
echo tlib#input#List('s', 'Test', ['barfoobar', 'bar1Foo1bar', 'barFoobar'])
echo tlib#input#EditList('Test', ['bar1', 'bar2', 'bar3', 'foo1', 'foo2', 'foo3'])


