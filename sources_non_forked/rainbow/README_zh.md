彩虹括号增强版 (Rainbow Parentheses Improved)
=============================================
>	通过将不同层次的括号高亮为不同的颜色, 帮助你阅读世界上最复杂的代码

插件简介:
---------

众所周知，最复杂的代码都是由一堆乱七八糟的括号组成。这款插件通过以不同的颜色展示不同层次的括号，致力于帮助你阅读这样的复杂代码。 你同样可以在[Vim官网](http://www.vim.org/scripts/script.php?script_id=4176)里看到这款插件

#### lisp
![lisp](https://raw.githubusercontent.com/luochen1990/rainbow/demo/lisp.png)
#### html
![html](https://raw.githubusercontent.com/luochen1990/rainbow/demo/html.png)
#### [more](https://github.com/luochen1990/rainbow/blob/demo/more.md)

### 有哪些改进？

- 更快速和流畅的体验。
- 简短,高质量,并且易读的源代码。
- 现在的版本将不再限制括号的嵌套层数。 
- 现在你可以分别自定义图形界面下和终端上所使用的各种括号颜色。
- 现在你可以自定义括号的形式，不过在这之前你最好了解vim脚本的正则表达式。
- 现在你甚至可以为不同类型的文件设定不同的配置。
- 现在你甚至可以决定是否让某些符号跟着它们所在的括号一起高亮，你也可以对不同类型的文件分别设置。
- 支持点分隔的复合文件类型 (`:h ft`)
- 现在采用json风格的配置文件,更加可读,更易于进行高级配置。
- 最后但并非不重要的一点是，如你所见，现在增加了中文说明。

### 以下是本插件所参考的旧版本： 
- http://www.vim.org/scripts/script.php?script_id=1561 (Martin Krischik)
- http://www.vim.org/scripts/script.php?script_id=3772 (kien)

安装说明:
---------

### 使用Vundle安装:

```vim
Bundle 'luochen1990/rainbow'
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
```

### 手动安装:

- 首先，执行以下命令 (Windows用户需要使用 `~/vimfiles` 替代 `~/.vim`) 。

	```sh
	git clone https://github.com/luochen1990/rainbow.git
	cd rainbow
	cp plugin/* ~/.vim/plugin
	cp autoload/* ~/.vim/autoload
	```

- 然后，将以下句子，加入到你的vim配置文件中（windows下配置文件是`_vimrc`，而linux下是`.vimrc`）

	```vim
	let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
	```

- 最后，重新启动你的vim，你就可以享受coding了。

高级配置:
---------

以下是一个配置的样例（也是我在用的配置），将它加入到你的vimrc并按照你喜欢的方式修改它（但是保持格式）你就可以精确地控制插件的行为了。

```vim
	let g:rainbow_conf = {
	\	'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
	\	'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
	\	'operators': '_,_',
	\	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
	\	'separately': {
	\		'*': {},
	\		'tex': {
	\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
	\		},
	\		'lisp': {
	\			'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
	\		},
	\		'vim': {
	\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
	\		},
	\		'html': {
	\			'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
	\		},
	\		'css': 0,
	\	}
	\}
```

- 'guifgs': 一个`guifg`的列表 (`:h highlight-guifg`), 即GUI界面的括号颜色, 将按顺序循环使用
- 'guis': 一个`gui`的列表 (`:h highlight-gui`), 将按顺序循环使用
- 'ctermfgs': 一个`ctermfg`的列表 (`:h highlight-ctermfg`), 即终端下的括号颜色
- 'cterms': 一个`cterm`的列表 (`:h highlight-cterm`)
- 'operators': 描述你希望哪些运算符跟着与它同级的括号一起高亮(注意：留意需要转义的特殊字符，更多样例见[这里](https://github.com/luochen1990/rainbow/issues/3), 你也可以读[vim帮助 :syn-pattern](http://vimdoc.sourceforge.net/htmldoc/syntax.html#:syn-pattern))
- 'parentheses': 一个关于括号定义的列表, 每一个括号的定义包含形如以下的部分:  `start=/(/`, `step=/,/`, `stop=/)/`, `fold`, `contained`, `containedin=someSynNames`, `contains=@Spell`. 各个部分具体含义可参考 `:h syntax`, 其中 `step` 为本插件的扩展定义, 表示括号中间需要高亮的运算符.
- 'separately': 针对文件类型(由&ft决定)作不同的配置,未被单独设置的文件类型使用`*`下的配置,值为`0`表示仅对该类型禁用插件,值为`"default"`表示使用针对该类型的默认兼容配置 (注意, 默认兼容配置可能随着该插件版本的更新而改变, 如果你不希望它改变, 那么你应该将它拷贝一份放到你的vimrc文件里).
- 省略某个字段以使用默认设置

-------------------------------------------------------------------
**最后，如果你喜欢这个插件，给它一个评价，我会心存感激，并且因为你的肯定继续改进这个插件！（从[该页面](http://www.vim.org/scripts/script.php?script_id=4176)下方，选择`Life Changing`选项，然后点击`rate`）**

