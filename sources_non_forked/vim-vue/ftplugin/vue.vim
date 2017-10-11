" Vim filetype plugin
" Language: Vue.js
" Maintainer: Eduardo San Martin Morote
" Author: Adriaan Zonnenberg

if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/html.vim

setlocal suffixesadd+=.vue

if exists('g:loaded_ale')
  let g:ale_linters = get(g:, 'ale_linters', {})
  let g:ale_linters.vue = get(g:ale_linters, 'vue', ['eslint'])
  let g:ale_linter_aliases = get(g:, 'ale_linter_aliases', {})
  let g:ale_linter_aliases.vue = get(g:ale_linter_aliases, 'vue', 'javascript')
endif
