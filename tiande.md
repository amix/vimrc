基本的 vim 操作就不介绍了，看过一本: [《VIM简明手册》](https://github.com/Tiande/Practice/blob/master/resource/VIM_Tutorial.md)

```
### WARNING:
全局的 <leader> 就是指 , (逗号) 了
<C-*> 里 'C' 是指 Ctrl , '-' 号无实际意义
地址连接符不用 Win 的 \ ,均以 Unix 的 / 展示
<space> 就是让你按空格
```

```
" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
绑定 <space> <C-space> 到 / ?
但因为 <C-space> 为切换输入法，所以可能会无效
```

```
" Disable highlight when <leader><cr> is pressed
,<cr>开关搜索词高亮
<cr> 是回车的意思
```

```
" Smart way to move between windows
<c-hjkl> 快速在窗口间切换
```

```
" Switch CWD to the directory of the open buffer
,cd 切换工作目录到当前文件所在目录
```

```
" Move a line of text using ALT+[jk] or Comamnd+[jk] on mac
<alt-jk> 移动当前行(或 visual 选中的多行)，与上下的其他行互换 
炒鸡好玩！！！！(Mac 用户再见;) )
```

```
" Remove the Windows ^M - when the encodings gets messed up
,m 把该死的 utf-8 with BOM(DOS下) 文件格式末尾的 ^M 去掉
```

```
" Quickly open a buffer for scribble
,q 快速打开一个 buffer
```

```
" Quickly open a markdown buffer for scribble
,x 快速打开一个 markdown buffer
```

```
" Toggle paste mode on and off
,pp 开关 paste mode
```

```
" Fast editing and reloading of vimrc configs
,e 快速打开 ./my_configs.vim

" 如果需要折行 ,e 后，将 set nowrap 注释掉(行头加 ")
```

```
目录补全:
输入模式下，按 <c-xf> 开启，然后 <c-n><c-p> 上下选择
你可以现在就试一下，先输入 . 然后使用快捷键补全成下面的样子： 
./README.md
```

```
打开地址下的文件:
gf
例: ./README.md
光标移到地址下然后 gf 试试看！ 可以使用 ,bd 关闭等下打开的窗口回到这里哦！
```

```
MRU
打开最近使用文件列表
open: ,f
close: q
open file: o
open with readonly: v
open in new tab: t
可以使用 <space> 或者 ? 启用搜索

to display only file names containing "vim" in them, you can use the following command ":MRU vim". # 其实就是关键词搜索 我更喜欢直接在 ,f 里 <space>
```

```
主题啊！！ 超多啊！！ 自己也可以去加啊！！
The 3rd theme:
peaksea ir_black mayansmoke solarized pyte gruvbox

These are vim's default theme (大概是这些，可以在 vim 的 colors 里找到): 
blue darkblue default delek desert elflord evening koehler morning murphy pablo peachpuff ron shine slate torte zellner

Edit your "my_configs.vim" (可以使用 ,e 直接打开) and modify:
:colorscheme ir_black # or peaksea ...
```

```
YankRing: 
是一个粘贴强化的功能
1.  paste something first (p), then use <c-p> <c-n> to select previous yanks, changes and deletes.  # p 之后， <c-n><c-p> 就可以选啦！
2.  to see a history of previous yanks, changes and deletes, use:
:YRShow
```

```
snipMate:
就是自动补全
打代码很有用
just <Tab> or <c-j> to finish a definition, function,  etc...

例: 输入 date OR datetime 后， 按 <tab> 或 <c-j> 就会补全日期啦！
2015-07-10
2015-07-10 10:56
```

```
bufexplorer:
缓冲区浏览器
缓冲区 和 缓存 感觉差不多
This Plugin can be opened with
,o
新标签打开
t
close
q
delete 一个
d
手册：
:help bufexplorer

Close the current buffer (关闭当前编辑的文件)
,bd
Close all the buffers (关闭所有 buffer)
,ba
```

```
commentary
使用 gc gcc gcap 快速添加注释
gcc : 只注释/取消注释当前行
gc : 注释/取消注释选定区域
gcap : 注释/取消注释一个区块
```

```
NERD Tree:
目录树

1.  <leader>nn: NERDTreeToggle # 打开
2.  <leader>nf: NERDTreeFind # 在目录树定位到当前文件
3.  <leader>nb: NERDTreeFromBookmark # 打开目录树指定的 mark 文件

命令太多，可以自行查看：
,nn 后按 ?
友情提示： <c-hjkl> 可以快速的在 不同窗口 间切换！
```

```
ctrlp:
Fuzzy file, buffer, mru and tag finder.
文件搜索
不要在磁盘根目录打开，遍历文件会卡死 (不要问我为啥知道...)  (使用 :pwd 查看当前目录)
三个打开命令随便哪个都行， <c-f> <c-b> 可以多按几次看不同效果，其他看帮助

1.  <c-f>
2.  <c-b>
3.  <leader>j # 在 ./my_configs.vim 中被 tab 切换快捷覆盖

for more info read the "./sources_non_forked/ctrlp.vim/readme.md"
```

```
Taglist(现在项目用的是 TagBar )
神器，列出所有函数声明 
!!!! 你要自己去安装 ctags 快到官网愉快的下载吧。
open 
该快捷键配置在 ./my_configs.vim
,tg
添加当前文件到 taglist (如果你勿用 d 删除了它):
,ta
在taglist窗口中看帮助：
<F1>
```

```
vim-multiple-cursors:
超好玩
在 v 模式下
<C-n> 从一个 word 开始选择，设置多焦点
<C-p> 撤消最后一个选项
<C-x> 跳过下一个目标
<ESC> 退出 multiple-cursors 模式

例：
使用 multiple-cursors 将下列 逗号 去掉并合并为一行：
1,
2,
3,
4,
5
结果：
1 2 3 4 5
```

```
vi-expand-region:
和 vim 中用 v V <c-v> 的 visual 模式不同,
直接使用 + - 号来选择区块。
智能度不行，但傻瓜易操作
```

```
vim-airline:
就是状态栏的主题
在 ,e 里配置你想要的色彩：
let g:airline_theme="luna"
```

```
goyo:
简化页面元素，让你专心码字！
1.  use <leader>z to into zoom mode ;)
2.  use <C--> <C-+> to zoom in/out ;)
```

```
syntastic:
语法检查 
,,, 开关
使用 ALT-n ALT-p 遍历出问题的行(MAC 下暂时不会映射按键)
```

```
使用 :SyntasticInfo 查看所用检查器 (checker)
python 需要先安装 pylint flake8 之类的检查器
To tell syntastic to use pylint, you would use this setting:
let g:syntastic_python_checkers = ['pylint']  # 我已在 my_configs.vim 里配置
e.g. Python has the following checkers, among others: flake8, pyflakes, pylint and a native python checker.
具体使用请看 sources_non_forked/syntastic/README.markdown

1.  <leader>ss to set/unset syntax checking.
2.  <leader>sn <leader>sp to move between wrong words.
3.  <leader>sa to add word to dictionary.
4.  <leader>s? to select a right word.
```

```
surround.vim config
在 选中的区域 加前后缀
" Annotate strings with gettext "http://amix.dk/blog/post/19678"
例:
:v # 先选中一段
S" # 此时即可前后加上 "
test ==> "test"
其实和 v 选中后再 $1 ($2 $3...)效果相同
```

```
tab 命令
,tn # 新 tab
,to # 只保留当前 tab
,tc # 关闭当前 tab (等同于 ZQ 或 ZZ 命令)
,tm # 移动标签到指定位置 (输入一个下标)
,t, # 补全一个 tab id 然后切过去

" Let ',tl' toggle between this and the last accessed tab
,tl 可在最近两个 tab 间切换！

将 ALT-u ,ALT-i 映射为 左右切换 tab (在 ./my_configs.vim)
vim 默认使用 gt gT 左右切换 tab
```

```
" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
,te 底部命令栏会出现当前文件目录，需要补全，然后在新 tab 打开文件
,nn 感觉更好用啊
```

```
保存命令
,w

" :W sudo saves the file 
使用 :W 命令，用 sudo 权限保存文件
```

```
" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
visual 模式选中字段后，使用 * OR # 可直接指定该区域为搜索条件
应当只按一次 * # ，然后用 n N 搜索
因为 * # 默认为搜索当前光标下的词(不是句子)！
```

```
" Parenthesis/bracket
visual 模式下！ 各种快捷 在选中的区域前后 插入标点符号
$1 ==> ()
$2 ==> []
$3 ==> {}
$$ ==> ""
$q ==> ''
$e ==> ""

和 S" 模式很像，可以自己再配置(,e)，格式是:
vnoremap $1 <esc>`>a)<esc>`<i(<esc>
" Map auto complete of (, ", ', [
插入模式下的补全
$1 ==> ()
$2 ==> []
$3 ==> {}
$4 ==> {

}
$q ==> ''
$e ==> ""
$t ==> <>
```

```
python 下折叠函数
F

操作：za，打开或关闭当前折叠；zM，关闭所有折叠；zR，打开所有折叠。
关于折叠的详细用法，可以在这里找到：
http://tiandechi.com/2015/07/08/vim%E6%8A%98%E5%8F%A0/
```

```
python 下的快捷插入
$r return
$i import
$p print 
$f #--- PH ----------------------------------------------<esc>FP2xi
搜索
,1 /class
,2 /def
,C ?class
,D ?def
```

```
javascript 下的 快捷
<c-t> $log()
<c-a> alert()
$r return
$f //--- PH ----------------------------------------------<esc>FP2xi
```

```
ACK 
并不会用...
需要机器上有 ACK(>=2.0) http://beyondgrep.com/install/
" When you press gv you Ack after the selected text
vnoremap <silent> gv :call VisualSelection('gv', '')<CR>

" Open Ack and put the cursor in the right position
map <leader>g :Ack 

" When you press <leader>r you can search and replace the selected text
vnoremap <silent> <leader>r :call VisualSelection('replace', '')<CR>

" 完全不知道这些是什么鬼
" Do :help cope if you are unsure what cope is. It's super useful!
"
" When you search with Ack, display your results in cope by doing:
"   <leader>cc
"
" To go to the next search result do:
"   <leader>n
"
" To go to the previous search results do:
"   <leader>p
"
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
map <leader>n :cn<cr>
map <leader>p :cp<cr>
```

```
可以跳过
因为我已经把 submodule 移除了...
use submodule plugin
直接引用了其他的 repository (submodule 是一个很好的功能，大概算是 库中库)
submodule 的信息储存在 ./.gitmodules

要把项目里 子仓库 部署进自家目录，还要再执行一串代码:
deploy
git submodule init
git submodule update --recursive
or use (合并成一句):
git submodule update --init --recursive
add
git submodule add git@github.com:yashiro1899/wheat.git node_modules/wheat
update
git submodule foreach --recursive git pull origin master
delete
git rm --cached node_modules/wheat && rm -rf node_modules/wheat/
```
