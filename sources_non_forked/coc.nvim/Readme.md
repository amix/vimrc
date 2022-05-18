<p align="center">
  <a href="https://www.vim.org/scripts/script.php?script_id=5779">
    <img alt="Coc Logo" src="https://user-images.githubusercontent.com/251450/55009068-f4ed2780-501c-11e9-9a3b-cf3aa6ab9272.png" height="160" />
  </a>
  <p align="center">Make your Vim/Neovim as smart as VSCode.</p>
  <p align="center">
    <a href="LICENSE.md"><img alt="Software License" src="https://img.shields.io/badge/license-Anti%20996-brightgreen.svg?style=flat-square"></a>
    <a href="https://github.com/neoclide/coc.nvim/actions"><img alt="Actions" src="https://img.shields.io/github/workflow/status/neoclide/coc.nvim/coc.nvim%20CI?style=flat-square"></a>
      <a href="https://codecov.io/gh/neoclide/coc.nvim"><img alt="Codecov Coverage Status" src="https://img.shields.io/codecov/c/github/neoclide/coc.nvim.svg?style=flat-square"></a>
    <a href="doc/coc.txt"><img alt="Doc" src="https://img.shields.io/badge/doc-%3Ah%20coc.txt-brightgreen.svg?style=flat-square"></a>
    <a href="https://gitter.im/neoclide/coc.nvim"><img alt="Gitter" src="https://img.shields.io/gitter/room/neoclide/coc.nvim.svg?style=flat-square"></a>
  </p>
</p>

---

<img alt="Gif" src="https://user-images.githubusercontent.com/251450/55285193-400a9000-53b9-11e9-8cff-ffe4983c5947.gif" width="60%" />

_True snippet and additional text editing support_

## Why?

- 🚀 **Fast**: [instant increment completion](https://github.com/neoclide/coc.nvim/wiki/Completion-with-sources), increment buffer sync using buffer update events.
- 💎 **Reliable**: typed language, tested with CI.
- 🌟 **Featured**: [full LSP support](https://github.com/neoclide/coc.nvim/wiki/Language-servers#supported-features)
- ❤️ **Flexible**: [configured like VSCode](https://github.com/neoclide/coc.nvim/wiki/Using-the-configuration-file), [extensions work like in VSCode](https://github.com/neoclide/coc.nvim/wiki/Using-coc-extensions)

**Gold Sponsors**

<a href="https://opencollective.com/cocnvim#platinum-sponsors">
  <img src="https://opencollective.com/cocnvim/tiers/gold-sponsors.svg?avatarHeight=36&width=600">
</a>

**Silver Sponsors**

<a href="https://opencollective.com/cocnvim#platinum-sponsors">
  <img src="https://opencollective.com/cocnvim/tiers/silver-sponsors.svg?avatarHeight=36&width=600">
</a>

**Bronze Sponsors**

<a href="https://opencollective.com/cocnvim#platinum-sponsors">
  <img src="https://opencollective.com/cocnvim/tiers/bronze-sponsors.svg?avatarHeight=36&width=600">
</a>

## Quick Start

Install [nodejs](https://nodejs.org/en/download/) >= 12.12:

```bash
curl -sL install-node.vercel.app/lts | bash
```

For [vim-plug](https://github.com/junegunn/vim-plug) users:

```vim
" Use release branch (recommend)
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Or build from source code by using yarn: https://yarnpkg.com
Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}
```

in your `.vimrc` or `init.vim`, then restart Vim and run `:PlugInstall`.

Checkout [Install
coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Install-coc.nvim) for
more info.

You **have to** install coc extension or configure language servers for
LSP support.

Install extensions like:

    :CocInstall coc-json coc-tsserver

Or configure language server in `coc-settings.json` opened by
`:CocConfig`, like:

```json
{
  "languageserver": {
    "go": {
      "command": "gopls",
      "rootPatterns": ["go.mod"],
      "trace.server": "verbose",
      "filetypes": ["go"]
    }
  }
}
```

Checkout wiki for more details:

- [Completion with sources](https://github.com/neoclide/coc.nvim/wiki/Completion-with-sources)
- [Using the configuration file](https://github.com/neoclide/coc.nvim/wiki/Using-the-configuration-file)
- [Using coc extensions](https://github.com/neoclide/coc.nvim/wiki/Using-coc-extensions)
- [Configure language servers](https://github.com/neoclide/coc.nvim/wiki/Language-servers)
- [F.A.Q](https://github.com/neoclide/coc.nvim/wiki/F.A.Q)

Checkout `:h coc-nvim` for vim interface.

## Example vim configuration

Configuration is required to make coc.nvim easier to work with, since it
doesn't change your key-mappings or Vim options. This is done as much as
possible to avoid conflict with your other plugins.

**❗️Important**: Some Vim plugins could change key mappings. Please use
command like`:verbose imap <tab>` to make sure that your keymap has taken effect.

```vim
" Set internal encoding of vim, not needed on neovim, since coc.nvim using some
" unicode characters in the file autoload/float.vim
set encoding=utf-8

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ CheckBackspace() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Run the Code Lens action on the current line.
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
```

## Articles

- [coc.nvim 插件体系介绍](https://zhuanlan.zhihu.com/p/65524706)
- [CocList 入坑指南](https://zhuanlan.zhihu.com/p/71846145)
- [Create coc.nvim extension to improve Vim experience](https://medium.com/@chemzqm/create-coc-nvim-extension-to-improve-vim-experience-4461df269173)
- [How to write a coc.nvim extension (and why)](https://samroeca.com/coc-plugin.html)

## Trouble shooting

Try these steps when you have problem with coc.nvim.

- Make sure your Vim version >= 8.0 by command `:version`.
- If service failed to start, use command `:CocInfo` or `:checkhealth` on Neovim.
- Checkout the log of coc.nvim by command `:CocOpenLog`.
- When you have issues with the language server, it's recommended to [checkout
  the output](https://github.com/neoclide/coc.nvim/wiki/Debug-language-server#using-output-channel).

## Feedback

- If you think Coc is useful, consider giving it a star.
- If you have a question, [ask on gitter](https://gitter.im/neoclide/coc.nvim)
- 中文用户请到 [中文 gitter](https://gitter.im/neoclide/coc-cn) 讨论
- If something is not working, [create an
  issue](https://github.com/neoclide/coc.nvim/issues/new).

## Backers

[Become a backer](https://opencollective.com/cocnvim#backer) and get your image on our README on Github with a link to your site.

<a href="https://opencollective.com/cocnvim/backer/0/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/0/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/1/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/1/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/2/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/2/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/3/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/3/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/4/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/4/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/5/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/5/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/6/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/6/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/7/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/7/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/8/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/8/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/9/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/9/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/10/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/10/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/11/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/11/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/12/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/12/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/13/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/13/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/14/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/14/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/15/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/15/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/16/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/16/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/17/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/17/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/18/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/18/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/19/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/19/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/20/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/20/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/21/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/21/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/22/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/22/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/23/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/23/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/24/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/24/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/25/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/25/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/26/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/26/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/27/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/27/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/28/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/28/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/29/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/29/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/30/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/30/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/31/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/31/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/32/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/32/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/33/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/33/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/34/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/34/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/35/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/35/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/36/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/36/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/37/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/37/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/38/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/38/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/39/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/39/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/40/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/40/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/41/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/41/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/42/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/42/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/43/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/43/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/44/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/44/avatar.svg?requireActive=false"></a>
<a href="https://opencollective.com/cocnvim/backer/45/website?requireActive=false" target="_blank"><img src="https://opencollective.com/cocnvim/backer/45/avatar.svg?requireActive=false"></a>

<a href="https://opencollective.com/cocnvim#backer" target="_blank"><img src="https://images.opencollective.com/static/images/become_backer.svg"></a>

## Contributors

<a href="https://github.com/neoclide/coc.nvim/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=neoclide/coc.nvim" />
</a>

## License

Anti 996
