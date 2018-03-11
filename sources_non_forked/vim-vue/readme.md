# vim-vue [![CircleCI](https://img.shields.io/circleci/project/github/posva/vim-vue.svg)](https://circleci.com/gh/posva/vim-vue)

Vim syntax highlighting for [Vue
components](https://vuejs.org/v2/guide/single-file-components.html).

This was initially forked from
[darthmall/vim-vue](https://github.com/darthmall/vim-vue). I already have an
implementation for this but found his code much cleaner. That's why I created a
new version instead of a PR.

## Installation

### Install with [Vundle](https://github.com/VundleVim/Vundle.vim)

```viml
Plugin 'posva/vim-vue'
```

### Install with [Pathogen](https://github.com/tpope/vim-pathogen)

```bash
cd ~/.vim/bundle && \
git clone https://github.com/posva/vim-vue.git
```

### Install without a plugin manager (Vim 8)

```bash
git clone https://github.com/posva/vim-vue.git ~/.vim/pack/plugins/start/vim-vue
```

### Integration with [Syntastic](https://github.com/scrooloose/syntastic) or [ALE](https://github.com/w0rp/ale)

Currently only `eslint` is available. Please make sure
[eslint](http://eslint.org/) and
[eslint-plugin-vue](https://github.com/vuejs/eslint-plugin-vue) are installed:

```bash
npm i -g eslint eslint-plugin-vue
```

## Contributing

If your language is not getting highlighted open an issue or a PR with the fix.
You only need to add a line to the `syntax/vue.vim` file.

## FAQ

### Where is Jade?

[Jade has been renamed to pug](https://github.com/pugjs/jade/issues/2184).
Therefore you have to replace all your `jade` occurrences with `pug`. The new
plugin for `pug` can be found on [the same repository](https://github.com/digitaltoad/vim-pug)
(the name has already been updated).

### Typescript support

You can use typescript by adding one of the following attributes/values to
your component's script tag:

```html
<script ts></script>
<script lang="ts"></script>
<script lang="typescript"></script>
```

Choose one that works with your module bundler

### My syntax highlighting stops working randomly

This is because Vim tries to highlight text in an efficient way. Especially in
files that include multiple languages, it can get confused. To work around
this, you can run `:syntax sync fromstart` when it happens.

You can also setup an autocmd for this:

```vim
autocmd FileType vue syntax sync fromstart
```

See `:h :syn-sync-first` and [this article](http://vim.wikia.com/wiki/Fix_syntax_highlighting)
for more details.

### How can I use existing configuration/plugins in Vue files?

If you already have some configuration for filetypes like html, css and
javascript (e.g. linters, completion), you can use them in .vue files by
setting compound filetypes like this:

```vim
autocmd BufRead,BufNewFile *.vue setlocal filetype=vue.html.javascript.css
```

:warning: This may cause problems, because some plugins will then treat the
whole buffer as html/javascript/css instead of only the part inside the tags.

### How can I use NERDCommenter in Vue files?

<details>
<summary>
To use NERDCommenter with Vue files, you can use its "hooks" feature to
temporarily change the filetype. Click for an example.
</summary>

```vim
let g:ft = ''
function! NERDCommenter_before()
  if &ft == 'vue'
    let g:ft = 'vue'
    let stack = synstack(line('.'), col('.'))
    if len(stack) > 0
      let syn = synIDattr((stack)[0], 'name')
      if len(syn) > 0
        exe 'setf ' . substitute(tolower(syn), '^vue_', '', '')
      endif
    endif
  endif
endfunction
function! NERDCommenter_after()
  if g:ft == 'vue'
    setf vue
    let g:ft = ''
  endif
endfunction
```

</details>

### _Vim gets slows down when using this plugin_ How can I fix that?

Add `let g:vue_disable_pre_processors=1` in your .vimrc to disable checking for prepocessors. When checking for prepocessor languages, multiple syntax highlighting checks are done, which can slow down vim. This variable prevents vim-vue from supporting **every** prepocessor language highlighting.
