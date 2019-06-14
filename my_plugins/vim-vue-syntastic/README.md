# vim-vue-syntastic

### 1\. What?

Support for vue components linting with syntastic and eslint

### 2\. Dependencies

- [Syntastic](https://github.com/scrooloose/syntastic)
- [ESlint](http://eslint.org/)
- [vim-vue](https://github.com/posva/vim-vue)

### 2.1\. Installing vim-vue-syntastic with Pathogen

If you already have [Pathogen][1] working then skip [Step 1](#step1) and go to
[Step 2](#step2).

<a name="step1"></a>

#### 2.1.1\. Step 1: Install pathogen.vim

First I'll show you how to install Tim Pope's [Pathogen][1] so that it's easy to
install syntastic.  Do this in your terminal so that you get the `pathogen.vim`
file and the directories it needs:
```sh
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
```
Next you *need* to add this to your `~/.vimrc`:
```vim
execute pathogen#infect()
```
<a name="step2"></a>

#### 2.1.3\. Step 2: Install vim-vue-syntastic as a Pathogen bundle and friends

You now have pathogen installed and can put syntastic into `~/.vim/bundle` like
this:

```sh
cd ~/.vim/bundle && \
git clone https://github.com/sekel/vim-vue-syntastic.git
```
... and the same song and dance for the dependencies

#### 2.1.3\. Step 3: Optional configuration to make it work with local project eslint

Stick this in your `~/.vimrc`:

```vim
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_vue_checkers = ['eslint']
let local_eslint = finddir('node_modules', '.;') . '/.bin/eslint'
if matchstr(local_eslint, "^\/\\w") == ''
    let local_eslint = getcwd() . "/" . local_eslint
endif
if executable(local_eslint)
    let g:syntastic_javascript_eslint_exec = local_eslint
    let g:syntastic_vue_eslint_exec = local_eslint
endif
```
