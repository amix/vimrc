Rainbow Parentheses Improved 
===
>	help you read complex code by showing diff level of parentheses in diff color !!

Description [(这里有中文版)](https://github.com/luochen1990/rainbow/blob/master/README_zh.md)
---------------------------------------------------------------------------------------------------

As everyone knows, the most complex codes were composed of a mass of different kinds of parentheses (typically: lisp).
This plugin will help you read these codes by showing different levels of parentheses in different colors.
You can also find this plugin in **[www.vim.org](http://www.vim.org/scripts/script.php?script_id=4176)**.

#### lisp
![lisp](https://raw.githubusercontent.com/luochen1990/rainbow/demo/lisp.png)
#### html
![html](https://raw.githubusercontent.com/luochen1990/rainbow/demo/html.png)
#### [more](https://github.com/luochen1990/rainbow/blob/demo/more.md)

### What is improved ?

- no limit of parentheses levels.
- separately edit guifgs and ctermfgs (the colors used for highlighting).
- now you can design your own parentheses  such as 'begin' and 'end'.
- you can also configure anything separately for different types of files.
- now you can even decide to let some operators (like + - * / , ==) highlighted with the parentheses together.
- dot separated combined filetype support (`:h ft`).
- json style configuration used, more understandable and readable, easier for advanced configuration.
- the code is shorter and easier to read now.
- smoother and faster.
- the Chinese document is added.

### Referenced:
- http://www.vim.org/scripts/script.php?script_id=1561 (Martin Krischik)
- http://www.vim.org/scripts/script.php?script_id=3772 (kien)

Install
-------

#### install via Plug:

```vim
Plug 'luochen1990/rainbow'
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle
```

#### install manually:

- first, execute the following commands (for windows users, use `~/vimfiles` instead of `~/.vim`)

	```sh
	git clone https://github.com/luochen1990/rainbow.git
	cd rainbow
	mkdir -p ~/.vim/plugin ~/.vim/autoload
	cp plugin/* ~/.vim/plugin
	cp autoload/* ~/.vim/autoload
	```

- second, add the follow sentences to your `.vimrc` or `_vimrc` :

	```vim
	let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle
	```

- third, restart your vim and enjoy coding.

Configure
---------

There is an example for advanced configuration, add it to your vimrc and edit it as you wish (just keep the format).

Note: you can remove these lines safely since they are all included by the [source code](https://github.com/luochen1990/rainbow/blob/master/autoload/rainbow_main.vim)).

```vim
let g:rainbow_conf = {
\	'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
\	'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
\	'guis': [''],
\	'cterms': [''],
\	'operators': '_,_',
\	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
\	'separately': {
\		'*': {},
\		'markdown': {
\			'parentheses_options': 'containedin=markdownCode contained', "enable rainbow for code blocks only
\		},
\		'lisp': {
\			'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'], "lisp needs more colors for parentheses :)
\		},
\		'haskell': {
\			'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/\v\{\ze[^-]/ end=/}/ fold'], "the haskell lang pragmas should be excluded
\		},
\		'vim': {
\			'parentheses_options': 'containedin=vimFuncBody', "enable rainbow inside vim function body
\		},
\		'perl': {
\			'syn_name_prefix': 'perlBlockFoldRainbow', "solve the [perl indent-depending-on-syntax problem](https://github.com/luochen1990/rainbow/issues/20)
\		},
\		'stylus': {
\			'parentheses': ['start=/{/ end=/}/ fold contains=@colorableGroup'], "[vim css color](https://github.com/ap/vim-css-color) compatibility
\		},
\		'css': 0, "disable this plugin for css files
\	}
\}
```

- 'guifgs': a list of `guifg` (`:h highlight-guifg`), i.e. colors for gui interface, will be used in order
- 'guis': a list of `gui` (`:h highlight-gui`), will be used in order
- 'ctermfgs': a list of `ctermfg` (`:h highlight-ctermfg`)
- 'cterms': a list of `cterm` (`:h highlight-cterm`)
- 'operators': describe the operators you want to highlight (note: be careful about special characters which needs escaping, you can find more examples [here](https://github.com/luochen1990/rainbow/issues/3), and you can also read the [vim help about syn-pattern](http://vimdoc.sourceforge.net/htmldoc/syntax.html#:syn-pattern)). note that this option will be overwritten by the `step` part of `parentheses`.
- 'parentheses': a list of parentheses definitions, a parentheses definition contains parts like `start=/(/`, `step=/,/`, `stop=/)/`, `fold`, `contained`, `containedin=someSynNames`, `contains=@Spell`, see `:h syntax` for more details. notice that the `step` part is defined by this plugin so it is not described by the official vim doc.
- 'parentheses_options': parentheses options shared between different parentheses, things like `containedin=xxxFuncBody`, `contains=@Spell` (or 'contains=@NoSpell') often appears here. this option is often used to solve [3rd-party-plugin-compatibility]() problems.
- 'separately': configure for specific filetypes (decided by &ft), key `*` for filetypes without separate configuration, value `0` means disable rainbow only for this type of files, value `"default"` means keep the default shim for this filetype (notice: the default shim config will change between plugin version).
- 'syn_name_prefix': add a prefix to name of the syntax definition, this option is often used to solve [3rd-party-plugin-compatibility]() problems.
- 'after': execute some vim commands after the rainbow syntax rules is defined. it is often used like `['syn clear xxx']` to solve [3rd-party-plugin-compatibility]() problems.
- keep a field empty to use the default setting.

To get more advanced config examples, try to search throught this [tag](https://github.com/luochen1990/rainbow/issues?utf8=%E2%9C%93&q=label%3A%22config+reference%22+).

User Command
------------

- **:RainbowToggle**		--you can use it to toggle this plugin.

3rd Party Plugin Compatibility
------------------------------

You should notice that this plugin is very special, Vim plugins is expected to provide syntax definitions vertically, i.e. one filetype, one syntax definition set:

```
----------------------------------------------------
|      cpp       |      java       |    python     |
|                |                 |               |
| syn cppKeyword | syn javaKeyword | syn pyKeyword |
| syn cppFunc    | syn javaFunc    | syn pyLambda  |
| syn cppParen   | syn javaParen   | syn pyParen   |
| ...            | ...             | ...           |
----------------------------------------------------
```

But this plugin provide syntax definitions horizontally, i.e. parentheses syntax for all filetypes:

```
----------------------------------------------------
|      cpp       |      java       |    python     |
|                |                 |               |
| syn cppKeyword | syn javaKeyword | syn pyKeyword |
| syn cppFunc    | syn javaFunc    | syn pyLambda  |
| ...            | ...             | ...           |
----------------------------------------------------
|                     rainbow                      |
|                                                  |
| syn cppRainbow   syn javaRainbow   syn pyRainbow |
----------------------------------------------------
```

You can notice that, to provide rainbow parentheses, this plugin have to define it's own syntax rules, and these rules will overwrite the parentheses syntax provided by the filetype plugin.

It works well at most of the time, but in some special cases, when the parentheses syntax rule is depended somewhere else (e.g. indent, spell checking, nested syntax rules), the things depend on the original syntax rules will be broken.

This plugin has provide some mechanisms to solve the compatibility problems, and have provided default configurations to solve compatibility problems with the default vim syntax files.

But if this plugin is conflicted with some other plugins, you will probably have to solve them by yourself. First, you can search on [this issue tag](https://github.com/luochen1990/rainbow/issues?utf8=%E2%9C%93&q=label%3A%22confliction+with+other+third-party+syntax+plugins%22+) to find whether there is somebody else had the same problem and solved it. Second, you can read the following content about troubleshooting.

Troubleshooting
---------------

- rainbow doesn't work at all: find out the current parentheses syntax name, and use the `after` mechanism to clear these syntax.
- rainbow doesn't work inside some structure: find out the syntax region name, and use the `parentheses_options` mechanism like `'containedin=xxx'`.
- spell checking doesn't work inside parentheses: use the `parentheses_options` mechanism like `'contains=@Spell'`.
- spell checking works inside parentheses but you don't want it: use the `parentheses_options` mechanism like `'contains=@NoSpell'`.
- indent not works correctly: check the indent script you are using and search `synID` inside it to find which syntax name prefix it depends on, and use the `syn_name_prefix` mechanism to solve it.

The following keymappings will help you to check the syntax name and definitions under the cursor, add them to your vimrc and restart vim:

```vim
nnoremap <f1> :echo synIDattr(synID(line('.'), col('.'), 0), 'name')<cr>
nnoremap <f2> :echo ("hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">")<cr>
nnoremap <f3> :echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')<cr>
nnoremap <f4> :exec 'syn list '.synIDattr(synID(line('.'), col('.'), 0), 'name')<cr>
```

Move your cursor to a parentheses and press the keys to use them.

------------------------------------------------------------------
**Rate this script if you like it, and I'll appreciate it and improve this plugin for you because of your support!

Just go to [this page](http://www.vim.org/scripts/script.php?script_id=4176) and choose `Life Changing` and click `rate`**
