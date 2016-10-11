script_name: Emmet.vim
script_id: '2981'
script_type: utility
script_package: emmet-vim.zip
script_version: '0.86'
required_vim_version: '7.0'
summary: vim plugins for HTML and CSS hi-speed coding.

detailed_description: |

  This is vim script support expanding abbreviation like emmet.
  ref: http://emmet.io/

  There is a movie using emmet.vim
  ref: http://mattn.github.com/emmet-vim

  Source Repository.
  ref: http://github.com/mattn/emmet-vim

  Type abbreviation
     +-------------------------------------
     | html:5_
     +-------------------------------------
  "_" is a cursor position. and type "<c-y>," (Ctrl + y and Comma)
  NOTE: Don't worry about key map. you can change it easily.
     +-------------------------------------
     | <!DOCTYPE HTML>
     | <html lang="en">
     | <head>
     |     <title></title>
     |     <meta charset="UTF-8">
     | </head>
     | <body>
     |      _
     | </body>
     | </html>
     +-------------------------------------
  Type following
     +-------------------------------------
     | div#foo$*2>div.bar
     +-------------------------------------
  And type "<c-y>,"
     +-------------------------------------
     |<div id="foo1">
     |    <div class="bar">_</div>
     |</div>
     |<div id="foo2">
     |    <div class="bar"></div>
     |</div>
     | _
     +-------------------------------------

  Tutorial:

    http://github.com/mattn/emmet-vim/raw/master/TUTORIAL

  How work this:

    http://mattn.github.com/emmet-vim

  Tips:

  You can customize behavior of expanding with overriding config.
  This configuration will be merged at loading plugin.

    let g:user_emmet_settings = {
    \  'indentation' : '  ',
    \  'perl' : {
    \    'aliases' : {
    \      'req' : 'require '
    \    },
    \    'snippets' : {
    \      'use' : "use strict\nuse warnings\n\n",
    \      'warn' : "warn \"|\";",
    \    }
    \  }
    \}

    let g:user_emmet_expandabbr_key = '<c-e>'

    let g:use_emmet_complete_tag = 1

  You can set language attribute in html using emmet_settings['lang'].

install_details: |

  # cd ~/.vim
  # unzip emmet-vim.zip

  or if you install pathogen.vim:

  # cd ~/.vim/bundle # or make directory
  # unzip /path/to/emmet-vim.zip

  if you get sources from repository:

  # cd ~/.vim/bundle # or make directory
  # git clone http://github.com/mattn/emmet-vim.git

versions:
- '0.86': |
   This is an upgrade for Emmet.vim: lot of bug fixes.
- '0.85': |
   This is an upgrade for Emmet.vim: lot of bug fixes.
- '0.84': |
   This is an upgrade for Emmet.vim: lot of bug fixes. fix bug that interpose insert completion plugins.
- '0.83': |
   This is an upgrade for Emmet.vim: lot of bug fixes.
- '0.82': |
   This is an upgrade for Emmet.vim: many bug fixes.
- '0.81': |
   Release of Emmet.vim: renamed from ZenCoding.vim.
- '0.80': |
   This is an upgrade for ZenCoding.vim: add emmet features.
- '0.74': |
   This is an upgrade for ZenCoding.vim: many bug fixes.
- '0.73': |
   This is an upgrade for ZenCoding.vim: many bug fixes. and support slim format (experimental).
- '0.72': |
   This is an upgrade for ZenCoding.vim:
   [fix] fix finding tokens.
- '0.71': |
   This is an upgrade for ZenCoding.vim:
   [fix] fix finding begin of tokens.
- '0.70': |
   This is an upgrade for ZenCoding.vim:
   [mod] Changed behavior of expanding. "div div>a|" should keep first div element.
   [add] Supported slim formatter.
- '0.60': |
   This is an upgrade for ZenCoding.vim:
   [fix] fixed expanding {{}}.
- '0.59': |
   This is an upgrade for ZenCoding.vim:
   [fix] fixed toggleComment and mny bugs.
- '0.58': |
   This is an upgrade for ZenCoding.vim:
   [fix] fixed 'foo+' style expandos.
- '0.57': |
   This is an upgrade for ZenCoding.vim:
   [fix] fixed expandos that don't work 'choose' in xsl.
- '0.56': |
   This is an upgrade for ZenCoding.vim:
   [fix] fixed contents parser.
- '0.55': |
   uploaded again: sorry, files was old.
- '0.54': |
   [add] support sass, xsd.
   [fix] expanding with html tag.
   uploaded again: sorry, fileformat was DOS.
- '0.53': |
   [fix] gif width/height was swapped.
- '0.52': |
   [fix] broken wrap expanding.
- '0.51': |
   This is an upgrade for ZenCoding.vim:
   [fix] wrap expanding with '&'.
   [fix] expand .content to class="content".
   [fix] haml expanding.
   [fix] bg+ snippet
- '0.50': |
   This is an upgrade for ZenCoding.vim:
   [fix] fixed parsing '#{{foo}}' and '.{{bar}}'.
- '0.49': |
   This is an upgrade for ZenCoding.vim:
   [doc] add help manual.
- '0.48': |
   This is an upgrade for ZenCoding.vim:
   [fix] install mappings to global.
- '0.47': |
   This is an upgrade for ZenCoding.vim:
   [drastic changes] enable autoload. you should whole replace older files.
   package was empty. upload again.
- '0.46': |
   This is an upgrade for ZenCoding.vim:
   [drastic changes] enable autoload. you should whole replace older files.
- '0.45': |
   This is an upgrade for ZenCoding.vim:
   fixed attribute parsing like: a[href="hello', world" rel].
- '0.44': |
   This is an upgrade for ZenCoding.vim:
   fixed checking whether have mapping using maparg() / hasmapto().
- '0.43': |
   This is an upgrade for ZenCoding.vim:
   fixed behavior for nested block. like "html:5>#page>(header#globalHeader>(hgroup>h1+h2)+(nav>ul>li*3>a)+(form>p.siteSearch>input+input[type=button]))+(#contents>(#main>(section>h2+p*5)+p.pagetop>a[href=#page])+(#sub>p+(nav>ul>li>a)))+(footer#globalFoooter>(ul>li>a)+(p.copyright>small))"
- '0.42': |
   This is an upgrade for ZenCoding.vim:
   fixed select/option indent.
- '0.41': |
   This is an upgrade for ZenCoding.vim:
   fixed default filter. when using 'e' filter, output become empty.
- '0.40': |
   This is an upgrade for ZenCoding.vim:
   add the pure vimscript code for 'get image size'. you can use it without perl interface just now.
   change key assign of ZenCodingExpandWord from ',' to ';'. it don't effect to most users.
- '0.39': |
   This is an upgrade for ZenCoding.vim: fixed problem about 'selection'. see http://github.com/mattn/zencoding-vim/issues/#issue/2
- '0.38': |
   This is an upgrade for ZenCoding.vim: use v7h"_s instead of v7hs for backspace.
- '0.37': |
   This is an upgrade for ZenCoding.vim: fixed problem that won't working with some 'backspace' options.
- '0.36': |
   This is an upgrade for ZenCoding.vim: fixed problem that filter does not work.
- '0.35': |
   This is an upgrade for ZenCoding.vim: enable zencoding for other languages. (meaning php also)
- '0.34': |
   This is an upgrade for ZenCoding.vim: enable zencoding for xsl. (you should add ~/.vim/ftplugin/xslt/zencoding.vim)
- '0.33': |
   This is an upgrade for ZenCoding.vim: fixed problem breaking multibyte when cursor is in a part of line. enabled zencoding for javascript in html.
- '0.32': |
   This is an upgrade for ZenCoding.vim: fixed indentation. supported extends so that you can enable zencoding for php/xhtml/haml other's section 14 in http://github.com/mattn/zencoding-vim/raw/master/TUTORIAL
- '0.31': |
   This is an upgrade for ZenCoding.vim: fixed indentation and $$$ problem. fixed about missing support multiple classes.
- '0.30': |
   This is an upgrade for ZenCoding.vim: Fixed key assign.
- '0.29': |
   This is an upgrade for ZenCoding.vim: Changed leading key to '<c-y>' from '<c-z>'.
- '0.28': |
   This is an upgrade for ZenCoding.vim: supported 'Balance Tag Inward/Outward', 'Go to Next/Previous Edit Point', 'Update <img> Size', 'Remove Tag', 'Split/Join Tag', 'Toggle Comment'
- '0.27': |
   This is an upgrade for ZenCoding.vim: fixed problem that can't work on the part of multibyte characters. fixed inline elements behavior.
- '0.26': |
   This is an upgrade for ZenCoding.vim: The count of '(((a#foo + a#bar)*2)*3)' should be 12.
- '0.25': |
   This is an upgrade for ZenCoding.vim: store undo before working. good luck about 'table>(tr>td*3)*4'.
- '0.24': |
   This is an upgrade for ZenCoding.vim: fixed behavior of parsing area of visual selection.
- '0.23': |
   This is an upgrade for ZenCoding.vim: pre-expand '#header>li<#content' to 'div#header>li<div#content'. support () expression.
- '0.22': |
   This is an upgrade for ZenCoding.vim: expand 'ul+' to 'ul>li'. fix undo ring. support visual selection. when type trigger key on visual select, it request you leader like 'ul>li'. if you give 'ul>li*' as leader, you'll get each separate 'ul>li' tags. and when you give 'blockquote' as leader, you'll get blocked text.
- '0.21': |
   This is an upgrade for ZenCoding.vim: treat xhtml as html.
- '0.20': |
   This is an upgrade for ZenCoding.vim: add option use_zen_complete_tag for complete abbr.
- '0.19': |
   This is an upgrade for ZenCoding.vim: fixed problem that couldn't expand 'link:css' correctly.
- '0.18': |
   This is an upgrade for ZenCoding.vim: ignore duplicate key map.
- '0.17': |
   This is an upgrade for ZenCoding.vim: fixed key map.
- '0.16': |
   This is an upgrade for ZenCoding.vim: fixed problem 'endless loop'.
- '0.15': |
   This is an upgrade for ZenCoding.vim: set default filetype to 'html'.
- '0.14': |
   This is an upgrade for ZenCoding.vim: fixed tag name like 'fs:n' in 'css'.
- '0.14': |
   This is an upgrade for ZenCoding.vim: indentation for each languages.
- '0.13': |
   This is an upgrade for ZenCoding.vim: user key map.
- '0.12': |
   This is an upgrade for ZenCoding.vim: few extensive notation.
- '0.11': |
   This is an upgrade for ZenCoding.vim: fixed indent.
- '0.10': |
   This is an upgrade for ZenCoding.vim: fixed behavior of '+' operator
- '0.9': |
   This is an upgrade for ZenCoding.vim: fixed single line behavior
- '0.8': |
   This is an upgrade for ZenCoding.vim: support 'a[href=http://www.google.com]{Google}'
- '0.7': |
   This is an upgrade for ZenCoding.vim: fixed behavior in 'a+b'.
- '0.6': |
   This is an upgrade for ZenCoding.vim: fixed strange behavior about '<a href="">b_</a>'.
- '0.5': |
   This is an upgrade for ZenCoding.vim: recover rest part in line.
- '0.4': |
   This is an upgrade for ZenCoding.vim: fixed cursor position. fixed ${lang} replacement.
- '0.3': |
   This is an upgrade for ZenCoding.vim: fixed line expanding.
- '0.2': |
   This is an upgrade for ZenCoding.vim: fixed problem that moving cursor with expanding.
- '0.1': |
   Initial upload

# __END__
# vim: filetype=yaml
