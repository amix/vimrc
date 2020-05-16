Auto Pairs
==========
Insert or delete brackets, parens, quotes in pair.

Installation
------------

* Manual
  * Copy `plugin/auto-pairs.vim` to `~/.vim/plugin`
* [Pathogen](https://github.com/tpope/vim-pathogen)
  * `git clone git://github.com/jiangmiao/auto-pairs.git ~/.vim/bundle/auto-pairs`
* [Vundle](https://github.com/VundleVim/Vundle.vim)
  * `Plugin 'jiangmiao/auto-pairs'`

Features
--------
*   Insert in pair

        input: [
        output: [|]

*   Delete in pair

        input: foo[<BS>]
        output: foo

*   Insert new indented line after Return

        input: {|} (press <CR> at |)
        output: {
            |
        }          (press } to close the pair)
        output: {
        }|         (the inserted blank line will be deleted)


*   Insert spaces before closing characters, only for [], (), {}

        input: {|} (press <SPACE> at |)
        output: { | }

        input: {|} (press <SPACE>foo} at |)
        output: { foo }|

        input: '|' (press <SPACE> at |)
        output: ' |'

*   Skip ' when inside a word

        input: foo| (press ' at |)
        output: foo'

*   Skip closed bracket.

        input: []
        output: []

*   Ignore auto pair when previous character is \

        input: "\'
        output: "\'"

*   Fast Wrap

        input: |[foo, bar()] (press (<M-e> at |)
        output: ([foo, bar()])

*   Quick move char to closed pair

        input: (|){["foo"]} (press <M-}> at |)
        output: ({["foo"]}|)

        input: |[foo, bar()] (press (<M-]> at |)
        output: ([foo, bar()]|)

*   Quick jump to closed pair.

        input:
        {
            something;|
        }

        (press } at |)

        output:
        {

        }|

*  Fly Mode

        input: if(a[3)
        output: if(a[3])| (In Fly Mode)
        output: if(a[3)]) (Without Fly Mode)

        input:
        {
            hello();|
            world();
        }

        (press } at |)

        output:
        {
            hello();
            world();
        }|

        (then press <M-b> at | to do backinsert)
        output:
        {
            hello();}|
            world();
        }

        See Fly Mode section for details

*  Multibyte Pairs
        
        Support any multibyte pairs such as <!-- -->, <% %>, """ """
        See multibyte pairs section for details

Fly Mode
--------
Fly Mode will always force closed-pair jumping instead of inserting. only for ")", "}", "]"

If jumps in mistake, could use AutoPairsBackInsert(Default Key: `<M-b>`) to jump back and insert closed pair.

the most situation maybe want to insert single closed pair in the string, eg ")"

Fly Mode is DISABLED by default.

add **let g:AutoPairsFlyMode = 1** .vimrc to turn it on

Default Options:

    let g:AutoPairsFlyMode = 0
    let g:AutoPairsShortcutBackInsert = '<M-b>'

Shortcuts
---------

    System Shortcuts:
        <CR>  : Insert new indented line after return if cursor in blank brackets or quotes.
        <BS>  : Delete brackets in pair
        <M-p> : Toggle Autopairs (g:AutoPairsShortcutToggle)
        <M-e> : Fast Wrap (g:AutoPairsShortcutFastWrap)
        <M-n> : Jump to next closed pair (g:AutoPairsShortcutJump)
        <M-b> : BackInsert (g:AutoPairsShortcutBackInsert)

    If <M-p> <M-e> or <M-n> conflict with another keys or want to bind to another keys, add

        let g:AutoPairsShortcutToggle = '<another key>'

    to .vimrc, if the key is empty string '', then the shortcut will be disabled.

Options
-------
*   g:AutoPairs

        Default: {'(':')', '[':']', '{':'}',"'":"'",'"':'"', "`":"`", '```':'```', '"""':'"""', "'''":"'''"}

*   b:AutoPairs

        Default: g:AutoPairs

        Buffer level pairs set.

*   g:AutoPairsShortcutToggle

        Default: '<M-p>'

        The shortcut to toggle autopairs.

*   g:AutoPairsShortcutFastWrap

        Default: '<M-e>'

        Fast wrap the word. all pairs will be consider as a block (include <>).
        (|)'hello' after fast wrap at |, the word will be ('hello')
        (|)<hello> after fast wrap at |, the word will be (<hello>)

*   g:AutoPairsShortcutJump

        Default: '<M-n>'

        Jump to the next closed pair

*   g:AutoPairsMapBS

        Default : 1

        Map <BS> to delete brackets, quotes in pair
        execute 'inoremap <buffer> <silent> <BS> <C-R>=AutoPairsDelete()<CR>'

*   g:AutoPairsMapCh

        Default : 1

        Map <C-h> to delete brackets, quotes in pair

*   g:AutoPairsMapCR

        Default : 1

        Map <CR> to insert a new indented line if cursor in (|), {|} [|], '|', "|"
        execute 'inoremap <buffer> <silent> <CR> <C-R>=AutoPairsReturn()<CR>'

*   g:AutoPairsCenterLine

        Default : 1

        When g:AutoPairsMapCR is on, center current line after return if the line is at the bottom 1/3 of the window.

*   g:AutoPairsMapSpace

        Default : 1

        Map <space> to insert a space after the opening character and before the closing one.
        execute 'inoremap <buffer> <silent> <CR> <C-R>=AutoPairsSpace()<CR>'

*   g:AutoPairsFlyMode

        Default : 0

        set it to 1 to enable FlyMode.
        see FlyMode section for details.

*   g:AutoPairsMultilineClose

        Default : 1

        When you press the key for the closing pair (e.g. `)`) it jumps past it.
        If set to 1, then it'll jump to the next line, if there is only whitespace.
        If set to 0, then it'll only jump to a closing pair on the same line.

*   g:AutoPairsShortcutBackInsert

        Default : <M-b>

        Work with FlyMode, insert the key at the Fly Mode jumped postion

*   g:AutoPairsMoveCharacter

        Default: "()[]{}\"'"

        Map <M-(> <M-)> <M-[> <M-]> <M-{> <M-}> <M-"> <M-'> to
        move character under the cursor to the pair.

Buffer Level Pairs Setting
--------------------------

Set b:AutoPairs before BufEnter

eg:

    " When the filetype is FILETYPE then make AutoPairs only match for parenthesis
    au Filetype FILETYPE let b:AutoPairs = {"(": ")"}
    au FileType php      let b:AutoPairs = AutoPairsDefine({'<?' : '?>', '<?php': '?>'})

Multibyte Pairs
---------------

    The default pairs is {'(':')', '[':']', '{':'}',"'":"'",'"':'"', '`':'`'}
    You could also define multibyte pairs such as <!-- -->, <% %> and so on

* Function AutoPairsDefine(addPairs:dict[, removeOpenPairList:list])

        add or delete pairs base on g:AutoPairs

        eg:
            au FileType html let b:AutoPairs = AutoPairsDefine({'<!--' : '-->'}, ['{'])
            add <!-- --> pair and remove '{' for html file

        the pair implict start with \V, so if want to match start of line ^ should be write in \^ vim comment {'\^"': ''}

* General usage

        au FileType php      let b:AutoPairs = AutoPairsDefine({'<?' : '?>', '<?php': '?>'})

        the first key of closed pair ? will be mapped

        pairs: '<?' : '?>', '<?php': '?>'
        input: <?
        output: <?|?>

        input: <?php
        output: <?php|?>

        input: he<?php|?> (press <BS> at|)
        output: he|

        input: <?php|?> (press ? at|)
        output: <?php?>|

        pair: '[[':']]'
        input: [[|]] (press <BS>)
        output: | ([[ and ]] will be deleted the [['s priority is higher than [ for it's longer)

* Modifier

        The text after //  in close pair is modifiers

        n - do not map the first charactor of closed pair to close key
        m - close key jumps through multi line
        s - close key jumps only in the same line
        k[KEY] - map the close key to [KEY]

            by default if open key equals close key the multi line is turn off

            "<?": "?>"      ? jumps only in the same line
            "<?": "?>//m"   force ? jumping through multi line
            "<?php":"?>"    ? will jump through multi line
            "<?php":"?>//s" force ? only jumping in the same line
            "<?": "?>//n"   do not jump totally
            "<?": "?>//k]"  use key ] to jump through ?>

        for 'begin' 'end' pair, e is a charactor, if map e to jump will be annoy, so use modifier 'n' to skip key map

        au FileType ruby     let b:AutoPairs = AutoPairsDefine({'begin': 'end//n]'})


        input: begin
        output: begin|end

        input: begin|end (press <BS> on |)
        output: |

        input: begin|end (press e on |)
        output: begineend (will not jump for e is not mapped)

* Advanced usage

        au FileType rust     let b:AutoPairs = AutoPairsDefine({'\w\zs<': '>'})

        if press < after a word will generate the pair

        when use regexp MUST use \zs to prevent catching
        if use '\w<' without \zs,  for text hello<|> press <BS> on | will output 'hell', the 'o' has been deleted

        pair: '\w\zs<': '>'
        input: h <
        output: h <

        input: h<
        output: h<|>

        input: h<|> press <BS>
        output: h|

        pair: '\w<': '>' (WRONG pair which missed \zs)
        input: h<|> press <BS>
        output: | (charactor 'h' is deleted)


        the 'begin' 'end' pair write in

        au FileType ruby     let b:AutoPairs = AutoPairsDefine({'\v(^|\W)\zsbegin': 'end//n'})

        will be better, only auto pair when at start of line or follow non-word text

TroubleShooting
---------------
    The script will remap keys ([{'"}]) <BS>,
    If auto pairs cannot work, use :imap ( to check if the map is corrected.
    The correct map should be <C-R>=AutoPairsInsert("\(")<CR>
    Or the plugin conflict with some other plugins.
    use command :call AutoPairsInit() to remap the keys.


* How to insert parens purely

    There are 3 ways

    1. use Ctrl-V ) to insert paren without trigger the plugin.

    2. use Alt-P to turn off the plugin.

    3. use DEL or <C-O>x to delete the character insert by plugin.

* Swedish Character Conflict

    Because AutoPairs uses Meta(Alt) key as shortcut, it is conflict with some Swedish character such as å.
    To fix the issue, you need remap or disable the related shortcut.

Known Issues
------------
Breaks '.' - [issue #3](https://github.com/jiangmiao/auto-pairs/issues/3)

    Description: After entering insert mode and inputing `[hello` then leave insert
                 mode by `<ESC>`. press '.' will insert 'hello' instead of '[hello]'.
    Reason: `[` actually equals `[]\<LEFT>` and \<LEFT> will break '.'.
            After version 7.4.849, Vim implements new keyword <C-G>U to avoid the break
    Solution: Update Vim to 7.4.849+

Contributors
------------
* [camthompson](https://github.com/camthompson)


License
-------

Copyright (C) 2011-2013 Miao Jiang

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
