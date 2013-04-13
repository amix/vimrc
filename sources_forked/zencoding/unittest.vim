let s:sfile = expand('<sfile>')

function! s:reload(d)
  exe "so" a:d."/plugin/zencoding.vim"
  for f in split(globpath(a:d, 'autoload/**/*.vim'), "\n")
    silent! exe "so" f
  endfor
endfunction

function! s:show_type(type)
  echohl Search | echon "[" a:type "]\n" | echohl None
  echo "\r"
endfunction

function! s:show_category(category)
  echohl MatchParen | echon "[" a:category "]\n" | echohl None
  echo "\r"
endfunction

function! s:show_pass(pass)
  echohl Title | echo "pass".a:pass."\n" | echohl None
endfunction

function! s:show_done()
  echohl IncSearch | echo "done" | echohl None
endfunction

function! s:escape(str)
  let str = a:str
  let str = substitute(str, "\n", '\\n', 'g')
  let str = substitute(str, "\t", '\\t', 'g')
  return str
endfunction

function! s:show_title(no, title)
  let title = s:escape(a:title)
  let width = &columns - 23
  echohl MoreMsg | echon "\rtesting #".printf("%03d", a:no)
  echohl None | echon ": " . (len(title) < width ? (title.repeat(' ', width-len(title))) : strpart(title, 0, width)) . ' ... '
endfunction

function! s:show_skip(no, title)
  let title = s:escape(a:title)
  let width = &columns - 23
  echohl WarningMsg | echon "\rskipped #".printf("%03d", a:no)
  echohl None | echon ": " . (len(title) < width ? (title.repeat(' ', width-len(title))) : strpart(title, 0, width)) . ' ... '
  echo ""
endfunction

function! s:show_ok()
  echohl Title | echon "ok\n" | echohl None
  echo ""
endfunction

function! s:show_ng(no, expect, got)
  echohl WarningMsg | echon "ng\n" | echohl None
  echohl ErrorMsg | echo "failed test #".a:no | echohl None
  set more
  echohl WarningMsg | echo printf("expect(%d):", len(a:expect)) | echohl None
  echo join(split(a:expect, "\n", 1), "|\n")
  echohl WarningMsg | echo printf("got(%d):", len(a:got)) | echohl None
  echo join(split(a:got, "\n", 1), "|\n")
  let cs = split(a:expect, '\zs')
  for c in range(len(cs))
    if c < len(a:got)
      if a:expect[c] != a:got[c]
        echohl WarningMsg | echo "differ at:" | echohl None
        echo a:expect[c :-1]
        break
      endif
    endif
  endfor
  echo ""
  throw "stop"
endfunction

function! s:test(...)
  let type = get(a:000, 0, '')
  let name = get(a:000, 1, '')
  let index = get(a:000, 2, '')

  let testgroups = eval(join(filter(split(substitute(join(readfile(s:sfile), "\n"), '.*\nfinish\n', '', ''), '\n', 1), "v:val !~ '^\"'")))
  for testgroup in testgroups
    if len(type) > 0 && testgroup.type != type | continue | endif
    call s:show_type(testgroup.type)
    for category in testgroup.categories
      if len(name) > 0 && substitute(category.name,' ','_','g') != name | continue | endif
      call s:show_category(category.name)
      let tests = category.tests
      let start = reltime()
      for n in range(len(tests))
        if len(index) > 0 && n != index | continue | endif
        let query = tests[n].query
        let result = tests[n].result
        if has_key(tests[n], 'skip') && tests[n].skip != 0
          call s:show_skip(n+1, query)
          continue
        endif
        if stridx(query, '$$$$') != -1
          silent! 1new
          silent! exe "setlocal ft=".testgroup.type
          silent! let key = matchstr(query, '.*\$\$\$\$\zs.*\ze\$\$\$\$')
          if len(key) > 0
            exe printf('let key = "%s"', key)
          else
            let key = "\<c-y>,"
          endif
          silent! let query = substitute(query, '\$\$\$\$.*\$\$\$\$', '$$$$', '')
          silent! call setline(1, split(query, "\n"))
          let cmd = "normal gg0/\\$\\$\\$\\$\ri\<del>\<del>\<del>\<del>".key
          if stridx(result, '$$$$') != -1
            let cmd .= '$$$$'
          endif
          silent! exe cmd
          unlet! res | let res = join(getline(1, line('$')), "\n")
          silent! bw!
          call s:show_title(n+1, query)
        else
          call s:show_title(n+1, query)
          unlet! res | let res = zencoding#ExpandWord(query, testgroup.type, 0)
        endif
        if stridx(result, '$$$$') != -1
          if res ==# result
            call s:show_ok()
          else
            call s:show_ng(n+1, result, res)
          endif
        else
          if res ==# result
            call s:show_ok()
          else
            call s:show_ng(n+1, result, res)
          endif
        endif
      endfor
      call s:show_pass(reltimestr(reltime(start)))
    endfor
  endfor
endfunction

function! s:do_tests(...)
  try
    if exists('g:user_zen_settings')
      let s:old_user_zen_settings = g:user_zen_settings
      let g:user_zen_settings = { 'indentation': "\t" }
    endif
    let oldmore = &more
    call s:reload(fnamemodify(s:sfile, ':h'))
    let &more = 0
    call call('s:test', a:000)
    call s:show_done()
  catch
    echohl ErrorMsg | echomsg v:exception | echohl None
  finally
    let &more=oldmore
    if exists('g:user_zen_settings')
      let g:user_zen_settings = s:old_user_zen_settings
    endif
  endtry
endfunction

function! g:zencoding_unittest_complete(arglead, cmdline, cmdpos)
  let args = split(a:cmdline, '\s\+', 1)
  let testgroups = eval(join(filter(split(substitute(join(readfile(s:sfile), "\n"), '.*\nfinish\n', '', ''), '\n', 1), "v:val !~ '^\"'")))
  try
    if len(args) == 2
      return filter(map(testgroups, 'v:val.type'), 'stridx(v:val,args[1])!=-1')
    elseif len(args) == 3
      return map(filter(testgroups, 'v:val.type==args[1]')[0].categories, 'substitute(v:val.name," ","_","g")')
    endif
  catch
  endtry
  return []
endfunction

command! -nargs=* -complete=customlist,g:zencoding_unittest_complete ZenCodingUnitTest call s:do_tests(<f-args>)
if s:sfile == expand('%:p')
  ZenCodingUnitTest
endif

finish
[
{
  'type': "html",
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "div",
          'result': "<div></div>\n",
        },
        {
          'query': "div#wrapper",
          'result': "<div id=\"wrapper\"></div>\n",
        },
        {
          'query': "div.box",
          'result': "<div class=\"box\"></div>\n",
        },
        {
          'query': "a[title=TITLE]",
          'result': "<a href=\"\" title=\"TITLE\"></a>\n",
        },
        {
          'query': "div#wrapper.box",
          'result': "<div id=\"wrapper\" class=\"box\"></div>\n",
        },
        {
          'query': "div#wrapper.box.current",
          'result': "<div id=\"wrapper\" class=\"box current\"></div>\n",
        },
        {
          'query': "div#wrapper.box.current[title=TITLE rel]",
          'result': "<div id=\"wrapper\" class=\"box current\" title=\"TITLE\" rel=\"\"></div>\n",
        },
        {
          'query': "div#main+div#sub",
          'result': "<div id=\"main\"></div>\n<div id=\"sub\"></div>\n",
        },
        {
          'query': "div#main>div#sub",
          'result': "<div id=\"main\">\n\t<div id=\"sub\"></div>\n</div>\n",
        },
        {
          'query': "html:xt>div#header>div#logo+ul#nav>li.item-$*5>a",
          'result': "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\n<head>\n\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\" />\n\t<title></title>\n</head>\n<body>\n\t<div id=\"header\">\n\t\t<div id=\"logo\"></div>\n\t\t<ul id=\"nav\">\n\t\t\t<li class=\"item-1\"><a href=\"\"></a></li>\n\t\t\t<li class=\"item-2\"><a href=\"\"></a></li>\n\t\t\t<li class=\"item-3\"><a href=\"\"></a></li>\n\t\t\t<li class=\"item-4\"><a href=\"\"></a></li>\n\t\t\t<li class=\"item-5\"><a href=\"\"></a></li>\n\t\t</ul>\n\t</div>\n\t\n</body>\n</html>",
        },
        {
          'query': "ol>li*2",
          'result': "<ol>\n\t<li></li>\n\t<li></li>\n</ol>\n",
        },
        {
          'query': "a",
          'result': "<a href=\"\"></a>\n",
        },
        {
          'query': "obj",
          'result': "<object data=\"\" type=\"\"></object>\n",
        },
        {
          'query': "cc:ie6>p+blockquote#sample$.so.many.classes*2",
          'result': "<!--[if lte IE 6]>\n\t<p></p>\n\t<blockquote id=\"sample1\" class=\"so many classes\"></blockquote>\n\t<blockquote id=\"sample2\" class=\"so many classes\"></blockquote>\n\t\n<![endif]-->",
        },
        {
          'query': "html:4t>div#wrapper>div#header+div#contents+div#footer",
          'result': "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n<html lang=\"en\">\n<head>\n\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\">\n\t<title></title>\n</head>\n<body>\n\t<div id=\"wrapper\">\n\t\t<div id=\"header\"></div>\n\t\t<div id=\"contents\"></div>\n\t\t<div id=\"footer\"></div>\n\t</div>\n\t\n</body>\n</html>",
        },
        {
          'query': "a[href=http://www.google.com/].foo#hoge",
          'result': "<a id=\"hoge\" class=\"foo\" href=\"http://www.google.com/\"></a>\n",
        },
        {
          'query': "a[href=http://www.google.com/]{Google}",
          'result': "<a href=\"http://www.google.com/\">Google</a>\n",
        },
        {
          'query': "{ZenCoding}",
          'result': "ZenCoding",
        },
        {
          'query': "a+b",
          'result': "<a href=\"\"></a>\n<b></b>\n",
        },
        {
          'query': "a>b>i<b",
          'result': "<a href=\"\"><b><i></i></b><b></b></a>\n",
        },
        {
          'query': "a>b>i^b",
          'result': "<a href=\"\"><b><i></i></b><b></b></a>\n",
        },
        {
          'query': "a>b>i<<b",
          'result': "<a href=\"\"><b><i></i></b></a>\n<b></b>\n",
        },
        {
          'query': "a>b>i^^b",
          'result': "<a href=\"\"><b><i></i></b></a>\n<b></b>\n",
        },
        {
          'query': "blockquote>b>i<<b",
          'result': "<blockquote><b><i></i></b></blockquote>\n<b></b>\n",
        },
        {
          'query': "blockquote>b>i^^b",
          'result': "<blockquote><b><i></i></b></blockquote>\n<b></b>\n",
        },
        {
          'query': "a[href=foo][class=bar]",
          'result': "<a class=\"bar\" href=\"foo\"></a>\n",
        },
        {
          'query': "a[a=b][b=c=d][e]{foo}*2",
          'result': "<a href=\"\" a=\"b\" b=\"c=d\" e=\"\">foo</a>\n<a href=\"\" a=\"b\" b=\"c=d\" e=\"\">foo</a>\n",
        },
        {
          'query': "a[a=b][b=c=d][e]*2{foo}",
          'result': "<a href=\"\" a=\"b\" b=\"c=d\" e=\"\"></a>\n<a href=\"\" a=\"b\" b=\"c=d\" e=\"\"></a>\nfoo",
        },
        {
          'query': "a*2{foo}a",
          'result': "<a href=\"\"></a>\n<a href=\"\"></a>\nfoo<a href=\"\"></a>\n",
        },
        {
          'query': "a{foo}*2>b",
          'result': "<a href=\"\">foo<b></b></a>\n<a href=\"\">foo<b></b></a>\n",
        },
        {
          'query': "a*2{foo}>b",
          'result': "<a href=\"\"></a>\n<a href=\"\"></a>\nfoo",
        },
        {
          'query': "table>tr>td.name#foo+td*3",
          'result': "<table>\n\t<tr>\n\t\t<td id=\"foo\" class=\"name\"></td>\n\t\t<td></td>\n\t\t<td></td>\n\t\t<td></td>\n\t</tr>\n</table>\n",
        },
        {
          'query': "div#header + div#footer",
          'result': "<div id=\"header\"></div>\n<div id=\"footer\"></div>\n",
        },
        {
          'query': "#header + div#footer",
          'result': "<div id=\"header\"></div>\n<div id=\"footer\"></div>\n",
        },
        {
          'query': "#header > ul > li < p{Footer}",
          'result': "<div id=\"header\">\n\t<ul>\n\t\t<li></li>\n\t</ul>\n\t<p>Footer</p>\n</div>\n",
        },
        {
          'query': "#header > ul > li ^ p{Footer}",
          'result': "<div id=\"header\">\n\t<ul>\n\t\t<li></li>\n\t</ul>\n\t<p>Footer</p>\n</div>\n",
        },
        {
          'query': "a#foo$$$*3",
          'result': "<a id=\"foo001\" href=\"\"></a>\n<a id=\"foo002\" href=\"\"></a>\n<a id=\"foo003\" href=\"\"></a>\n",
        },
        {
          'query': "ul+",
          'result': "<ul>\n\t<li></li>\n</ul>\n",
        },
        {
          'query': "table+",
          'result': "<table>\n\t<tr>\n\t\t<td></td>\n\t</tr>\n</table>\n",
        },
        {
          'query': "#header>li<#content",
          'result': "<div id=\"header\">\n\t<li></li>\n</div>\n<div id=\"content\"></div>\n",
        },
        {
          'query': "#header>li^#content",
          'result': "<div id=\"header\">\n\t<li></li>\n</div>\n<div id=\"content\"></div>\n",
        },
        {
          'query': "(#header>li)<#content",
          'result': "<div id=\"header\">\n\t<li></li>\n</div>\n<div id=\"content\"></div>\n",
        },
        {
          'query': "(#header>li)^#content",
          'result': "<div id=\"header\">\n\t<li></li>\n</div>\n<div id=\"content\"></div>\n",
        },
        {
          'query': "a>b>i<<div",
          'result': "<a href=\"\"><b><i></i></b></a>\n<div></div>\n",
        },
        {
          'query': "a>b>i^^div",
          'result': "<a href=\"\"><b><i></i></b></a>\n<div></div>\n",
        },
        {
          'query': "(#header>h1)+#content+#footer",
          'result': "<div id=\"header\">\n\t<h1></h1>\n</div>\n<div id=\"content\"></div>\n<div id=\"footer\"></div>\n",
        },
        {
          'query': "(#header>h1)+(#content>(#main>h2+div#entry$.section*5>(h3>a)+div>p*3+ul+)+(#utilities))+(#footer>address)",
          'result': "<div id=\"header\">\n\t<h1></h1>\n</div>\n<div id=\"content\">\n\t<div id=\"main\">\n\t\t<h2></h2>\n\t\t<div id=\"entry1\" class=\"section\">\n\t\t\t<h3><a href=\"\"></a></h3>\n\t\t\t<div>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<ul>\n\t\t\t\t\t<li></li>\n\t\t\t\t</ul>\n\t\t\t</div>\n\t\t</div>\n\t\t<div id=\"entry2\" class=\"section\">\n\t\t\t<h3><a href=\"\"></a></h3>\n\t\t\t<div>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<ul>\n\t\t\t\t\t<li></li>\n\t\t\t\t</ul>\n\t\t\t</div>\n\t\t</div>\n\t\t<div id=\"entry3\" class=\"section\">\n\t\t\t<h3><a href=\"\"></a></h3>\n\t\t\t<div>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<ul>\n\t\t\t\t\t<li></li>\n\t\t\t\t</ul>\n\t\t\t</div>\n\t\t</div>\n\t\t<div id=\"entry4\" class=\"section\">\n\t\t\t<h3><a href=\"\"></a></h3>\n\t\t\t<div>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<ul>\n\t\t\t\t\t<li></li>\n\t\t\t\t</ul>\n\t\t\t</div>\n\t\t</div>\n\t\t<div id=\"entry5\" class=\"section\">\n\t\t\t<h3><a href=\"\"></a></h3>\n\t\t\t<div>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<p></p>\n\t\t\t\t<ul>\n\t\t\t\t\t<li></li>\n\t\t\t\t</ul>\n\t\t\t</div>\n\t\t</div>\n\t</div>\n\t<div id=\"utilities\"></div>\n</div>\n<div id=\"footer\">\n\t<address></address>\n</div>\n",
        },
        {
          'query': "(div>(ul*2)*2)+(#utilities)",
          'result': "<div>\n\t<ul></ul>\n\t<ul></ul>\n\t<ul></ul>\n\t<ul></ul>\n</div>\n<div id=\"utilities\"></div>\n",
        },
        {
          'query': "table>(tr>td*3)*4",
          'result': "<table>\n\t<tr>\n\t\t<td></td>\n\t\t<td></td>\n\t\t<td></td>\n\t</tr>\n\t<tr>\n\t\t<td></td>\n\t\t<td></td>\n\t\t<td></td>\n\t</tr>\n\t<tr>\n\t\t<td></td>\n\t\t<td></td>\n\t\t<td></td>\n\t</tr>\n\t<tr>\n\t\t<td></td>\n\t\t<td></td>\n\t\t<td></td>\n\t</tr>\n</table>\n",
        },
        {
          'query': "(((a#foo+a#bar)*2)*3)",
          'result': "<a id=\"foo\" href=\"\"></a>\n<a id=\"bar\" href=\"\"></a>\n<a id=\"foo\" href=\"\"></a>\n<a id=\"bar\" href=\"\"></a>\n<a id=\"foo\" href=\"\"></a>\n<a id=\"bar\" href=\"\"></a>\n<a id=\"foo\" href=\"\"></a>\n<a id=\"bar\" href=\"\"></a>\n<a id=\"foo\" href=\"\"></a>\n<a id=\"bar\" href=\"\"></a>\n<a id=\"foo\" href=\"\"></a>\n<a id=\"bar\" href=\"\"></a>\n",
        },
        {
          'query': "div#box$*3>h3+p*2",
          'result': "<div id=\"box1\">\n\t<h3></h3>\n\t<p></p>\n\t<p></p>\n</div>\n<div id=\"box2\">\n\t<h3></h3>\n\t<p></p>\n\t<p></p>\n</div>\n<div id=\"box3\">\n\t<h3></h3>\n\t<p></p>\n\t<p></p>\n</div>\n"
        },
        {
          'query': "div#box.foo$$$.bar$$$*3",
          'result': "<div id=\"box\" class=\"foo001 bar001\"></div>\n<div id=\"box\" class=\"foo002 bar002\"></div>\n<div id=\"box\" class=\"foo003 bar003\"></div>\n",
        },
        {
          'query': "div#box$*3>h3+p.bar*2|e",
          'result': "&lt;div id=\"box1\"&gt;\n\t&lt;h3&gt;&lt;/h3&gt;\n\t&lt;p class=\"bar\"&gt;&lt;/p&gt;\n\t&lt;p class=\"bar\"&gt;&lt;/p&gt;\n&lt;/div&gt;\n&lt;div id=\"box2\"&gt;\n\t&lt;h3&gt;&lt;/h3&gt;\n\t&lt;p class=\"bar\"&gt;&lt;/p&gt;\n\t&lt;p class=\"bar\"&gt;&lt;/p&gt;\n&lt;/div&gt;\n&lt;div id=\"box3\"&gt;\n\t&lt;h3&gt;&lt;/h3&gt;\n\t&lt;p class=\"bar\"&gt;&lt;/p&gt;\n\t&lt;p class=\"bar\"&gt;&lt;/p&gt;\n&lt;/div&gt;\n",
        },
        {
          'query': "div>div#page>p.title+p|c",
          'result': "<div>\n\t<!-- #page -->\n\t<div id=\"page\">\n\t\t<!-- .title -->\n\t\t<p class=\"title\"></p>\n\t\t<!-- /.title -->\n\t\t<p></p>\n\t</div>\n\t<!-- /#page -->\n</div>\n",
        },
        {
          'query': "link:css",
          'result': "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" media=\"all\" />\n",
        },
        {
          'query': "a[title=\"Hello', world\" rel]",
          'result': "<a href=\"\" title=\"Hello', world\" rel=\"\"></a>\n",
        },
        {
          'query': "div>a#foo{bar}",
          'result': "<div><a id=\"foo\" href=\"\">bar</a></div>\n",
        },
        {
          'query': ".content{Hello!}",
          'result': "<div class=\"content\">Hello!</div>\n",
        },
        {
          'query': "div.logo+(div#navigation)+(div#links)",
          'result': "<div class=\"logo\"></div>\n<div id=\"navigation\"></div>\n<div id=\"links\"></div>\n",
        },
        {
          'query': "h1{header}+{Text}+a[href=http://link.org]{linktext}+{again some text}+a[href=http://anoterlink.org]{click me!}+{some final text}",
          'result': "<h1>header</h1>\nText<a href=\"http://link.org\">linktext</a>\nagain some text<a href=\"http://anoterlink.org\">click me!</a>\nsome final text",
        },
        {
          'query': "a{&}+div{&&}",
          'result': "<a href=\"\">&</a>\n<div>&&</div>\n",
        },
        {
          'query': "<foo/>span$$$$\\<c-y>,$$$$",
          'result': "<foo/><span></span>",
        },
        {
          'query': "foo span$$$$\\<c-y>,$$$$",
          'result': "foo <span></span>",
        },
        {
          'query': "foo span$$$$\\<c-y>,$$$$ bar",
          'result': "foo <span></span> bar",
        },
        {
          'query': "foo $$$$\\<c-o>ve\\<c-y>,p\\<cr>$$$$bar baz",
          'result': "foo <p>bar</p> baz",
        },
        {
          'query': "foo $$$$\\<c-o>vee\\<c-y>,p\\<cr>$$$$bar baz",
          'result': "foo <p>bar baz</p>",
        },
        {
          'query': "f div.boxes>article.box2>header>(hgroup>h2{aaa}+h3{bbb})+p{ccc}$$$$",
          'result': "f <div class=\"boxes\">\n\t<article class=\"box2\">\n\t\t<header>\n\t\t\t<hgroup>\n\t\t\t\t<h2>aaa</h2>\n\t\t\t\t<h3>bbb</h3>\n\t\t\t</hgroup>\n\t\t\t<p>ccc</p>\n\t\t</header>\n\t</article>\n</div>",
        },
        {
          'query': "div.boxes>(div.box2>section>h2{a}+p{b})+(div.box1>section>h2{c}+p{d}+p{e}+(bq>h2{f}+h3{g})+p{h})",
          'result': "<div class=\"boxes\">\n\t<div class=\"box2\">\n\t\t<section>\n\t\t\t<h2>a</h2>\n\t\t\t<p>b</p>\n\t\t</section>\n\t</div>\n\t<div class=\"box1\">\n\t\t<section>\n\t\t\t<h2>c</h2>\n\t\t\t<p>d</p>\n\t\t\t<p>e</p>\n\t\t\t<blockquote>\n\t\t\t\t<h2>f</h2>\n\t\t\t\t<h3>g</h3>\n\t\t\t</blockquote>\n\t\t\t<p>h</p>\n\t\t</section>\n\t</div>\n</div>\n",
        },
        {
          'query': "(div>(label+input))+div",
          'result': "<div>\n\t<label for=\"\"></label>\n\t<input type=\"\" />\n</div>\n<div></div>\n",
        },
        {
          'query': "test1\ntest2\ntest3$$$$\\<esc>ggVG\\<c-y>,ul>li>span*>a\\<cr>$$$$",
          'result': "<ul>\n\t<li>\n\t\t<span><a href=\"\">test1</a></span>\n\t\t<span><a href=\"\">test2</a></span>\n\t\t<span><a href=\"\">test3</a></span>\n\t</li>\n</ul>",
        },
        {
          'query': "test1\ntest2\ntest3$$$$\\<esc>ggVG\\<c-y>,input[type=input value=$#]*\\<cr>$$$$",
          'result': "<input type=\"input\" value=\"test1\" />\n<input type=\"input\" value=\"test2\" />\n<input type=\"input\" value=\"test3\" />",
        },
        {
          'query': "div#id-$*5>div#id2-$",
          'result': "<div id=\"id-1\">\n\t<div id=\"id2-1\"></div>\n</div>\n<div id=\"id-2\">\n\t<div id=\"id2-2\"></div>\n</div>\n<div id=\"id-3\">\n\t<div id=\"id2-3\"></div>\n</div>\n<div id=\"id-4\">\n\t<div id=\"id2-4\"></div>\n</div>\n<div id=\"id-5\">\n\t<div id=\"id2-5\"></div>\n</div>\n",
        },
        {
          'query': "{test case $ }*3",
          'result': "test case 1 test case 2 test case 3 ",
        },
        {
          'query': "{test case $${nr}}*3",
          'result': "test case 1\ntest case 2\ntest case 3\n",
        },
        {
          'query': "{test case \\$ }*3",
          'result': "test case $ test case $ test case $ ",
        },
        {
          'query': "{test case $$$ }*3",
          'result': "test case 001 test case 002 test case 003 ",
        },
        {
          'query': "a[title=$#]{foo}",
          'result': "<a href=\"\" title=\"foo\">foo</a>\n",
        },
      ],
    },
    {
      'name': 'split join tag',
      'tests': [
        {
          'query': "<div>\n\t<span>$$$$\\<c-y>j$$$$</span>\n</div>",
          'result': "<div>\n\t<span/>\n</div>",
        },
        {
          'query': "<div>\n\t<span$$$$\\<c-y>j$$$$/>\n</div>",
          'result': "<div>\n\t<span></span>\n</div>",
        },
      ],
    },
    {
      'name': 'toggle comment',
      'tests': [
        {
          'query': "<div>\n\t<span>$$$$\\<c-y>/$$$$</span>\n</div>",
          'result': "<div>\n\t<!-- <span></span> -->\n</div>",
        },
        {
          'query': "<div>\n\t<!-- <span>$$$$\\<c-y>/$$$$</span> -->\n</div>",
          'result': "<div>\n\t<span></span>\n</div>",
        },
      ],
    },
    {
      'name': 'image size',
      'tests': [
        {
          'query': "img[src=http://mattn.kaoriya.net/images/logo.png]$$$$\\<c-y>,\\<c-y>i$$$$",
          'result': "<img src=\"http://mattn.kaoriya.net/images/logo.png\" alt=\"\" width=\"96\" height=\"96\" />",
        },
        {
          'query': "img[src=/logo.png]$$$$\\<c-y>,\\<c-y>i$$$$",
          'result': "<img src=\"/logo.png\" alt=\"\" />",
        },
      ],
    },
    {
      'name': 'move next prev',
      'tests': [
        {
          'query': "foo+bar+baz[dankogai=\"\"]$$$$\\<c-y>,\\<esc>gg0\\<c-y>n\\<c-y>n\\<c-y>n\\<esc>Byw:%d _\\<cr>p$$$$",
          'result': "dankogai",
        },
      ],
    },
  ],
},
{
  'type': 'css',
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "@i",
          'result': "@import url();",
        },
        {
          'query': "fs:n",
          'result': "font-style: normal;",
        },
        {
          'query': "fl:l|fc",
          'result': "float: left;",
        },
        {
          'query': "bg+$$$$",
          'result': "background: #FFF url($$$$) 0 0 no-repeat;",
        },
        {
          'query': "bg+!$$$$",
          'result': "background: #FFF url($$$$) 0 0 no-repeat !important;",
        },
        {
          'query': "m$$$$",
          'result': "margin: $$$$;",
        },
        {
          'query': "m0.1p$$$$",
          'result': "margin: 0.1%;",
        },
        {
          'query': "m1.0$$$$",
          'result': "margin: 1.0em;",
        },
        {
          'query': "m2$$$$",
          'result': "margin: 2px;",
        },
        {
          'query': "bdrs10$$$$",
          'result': "border-radius: 10px;",
        },
        {
          'query': "-bdrs20$$$$",
          'result': "-webkit-border-radius: 20px;\n-moz-border-radius: 20px;\nborder-radius: 20px;",
        },
        {
          'query': "lg(top,#fff,#000)$$$$",
          'result': "background-image:  -webkit-gradient(top, 0 0, 0 100, from(#fff), to(#000));\nbackground-image:  -webkit-linear-gradient(#fff, #000);\nbackground-image:  -moz-linear-gradient(#fff, #000);\nbackground-image:  -o-linear-gradient(#fff, #000);\nbackground-image:  linear-gradient(#fff, #000);\n",
        },
        {
          'query': "m10-5-0$$$$",
          'result': "margin: 10px 5px 0px;",
        },
        {
          'query': "m-10--5$$$$",
          'result': "margin: -10px -5px;",
        },
        {
          'query': "m10-auto$$$$",
          'result': "margin: 10px auto;",
        },
        {
          'query': "w100p$$$$",
          'result': "width: 100%;",
        },
        {
          'query': "h50e$$$$",
          'result': "height: 50em;",
        },
        {
          'query': "(bg+)+c$$$$",
          'result': "background: #FFF url($$$$) 0 0 no-repeat;\ncolor: #000;",
        },
      ],
    },
  ],
},
{
  'type': 'haml',
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "div>p+ul#foo>li.bar$[foo=bar][bar=baz]*3>{baz}",
          'result': "%div\n  %p\n  %ul#foo\n    %li.bar1{ :foo => \"bar\", :bar => \"baz\" } baz\n    %li.bar2{ :foo => \"bar\", :bar => \"baz\" } baz\n    %li.bar3{ :foo => \"bar\", :bar => \"baz\" } baz\n",
        },
        {
          'query': "div>p+ul#foo>li.bar$[foo=bar][bar=baz]*3>{baz}|haml",
          'result': "%div\n  %p\n  %ul#foo\n    %li.bar1{ :foo => \"bar\", :bar => \"baz\" } baz\n    %li.bar2{ :foo => \"bar\", :bar => \"baz\" } baz\n    %li.bar3{ :foo => \"bar\", :bar => \"baz\" } baz\n",
        },
        {
          'query': "a*3|haml",
          'result': "%a{ :href => \"\" }\n%a{ :href => \"\" }\n%a{ :href => \"\" }\n",
        },
        {
          'query': ".content{Hello!}|haml",
          'result': "%div.content Hello!\n",
        },
        {
          'query': "a[title=$#]{foo}",
          'result': "%a{ :href => \"\", :title => \"foo\" } foo\n",
        },
      ],
    },
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "%a foo\n  bar$$$$\\<c-y>j$$$$",
          'result': "%a ",
        },
        {
          'query': "$$$$\\<c-y>j$$$$%a ",
          'result': "%a $$$$",
        },
      ],
    },
    {
      'name': 'toggle comment',
      'tests': [
        {
          'query': "%a{ :href => \"http://www.google.com\"$$$$\\<c-y>/$$$$ } hello",
          'result': "-# %a{ :href => \"http://www.google.com\" } hello",
        },
        {
          'query': "-# %a{ :href => \"http://www.google.com\"$$$$\\<c-y>/$$$$ } hello",
          'result': "%a{ :href => \"http://www.google.com\" } hello",
        },
      ],
    },
  ],
},
{
  'type': 'slim',
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "div>p+ul#foo>li.bar$[foo=bar][bar=baz]*3>{baz}",
          'result': "div\n  p\n  ul id=\"foo\"\n    li class=\"bar1\" foo=\"bar\" bar=\"baz\"\n      | baz\n    li class=\"bar2\" foo=\"bar\" bar=\"baz\"\n      | baz\n    li class=\"bar3\" foo=\"bar\" bar=\"baz\"\n      | baz\n",
        },
        {
          'query': "div>p+ul#foo>li.bar$[foo=bar][bar=baz]*3>{baz}|slim",
          'result': "div\n  p\n  ul id=\"foo\"\n    li class=\"bar1\" foo=\"bar\" bar=\"baz\"\n      | baz\n    li class=\"bar2\" foo=\"bar\" bar=\"baz\"\n      | baz\n    li class=\"bar3\" foo=\"bar\" bar=\"baz\"\n      | baz\n",
        },
        {
          'query': "a*3|slim",
          'result': "a href=\"\"\na href=\"\"\na href=\"\"\n",
        },
        {
          'query': ".content{Hello!}|slim",
          'result': "div class=\"content\"\n  | Hello!\n",
        },
        {
          'query': "a[title=$#]{foo}",
          'result': "a href=\"\" title=\"foo\"\n  | foo\n",
        },
      ],
    },
    {
      'name': 'split join tag',
      'tests': [
        {
          'query': "a\n  | foo$$$$\\<c-y>j$$$$",
          'result': "a",
        },
        {
          'query': "a$$$$\\<c-y>j$$$$",
          'result': "a\n  | $$$$",
        },
      ],
    },
    {
      'name': 'toggle comment',
      'tests': [
        {
          'query': "a href=\"http://www.google.com\"$$$$\\<c-y>/$$$$\n  | hello",
          'result': "/a href=\"http://www.google.com\"\n  | hello",
        },
        {
          'query': "/a href=\"http://www.google.com\"$$$$\\<c-y>/$$$$\n  | hello",
          'result': "a href=\"http://www.google.com\"\n  | hello",
        },
      ],
    },
  ],
},
{
  'type': 'xsl',
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "vari",
          'result': "<xsl:variable name=\"\"></xsl:variable>\n",
        },
        {
          'query': "ap>wp",
          'result': "<xsl:apply-templates select=\"\" mode=\"\">\n\t<xsl:with-param name=\"\" select=\"\"></xsl:with-param>\n</xsl:apply-templates>\n",
        },
      ],
    },
  ],
},
{
  'type': 'xsd',
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "xsd:w3c",
          'result': "<?xml version=\"1.0\"?>\n<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">\n\t<xsd:element name=\"\" type=\"\"/>\n</xsd:schema>",
        },
      ],
    },
  ],
},
{
  'type': 'mustache',
  'categories': [
    {
      'name': 'expand abbreviation',
      'tests': [
        {
          'query': "div#{{foo}}",
          'result': "<div id=\"{{foo}}\"></div>\n",
        },
        {
          'query': "div.{{foo}}",
          'result': "<div class=\"{{foo}}\"></div>\n",
        },
      ],
    },
  ],
},
]
" vim:set et:
