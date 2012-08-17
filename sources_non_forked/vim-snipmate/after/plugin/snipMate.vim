" These are the mappings for snipMate.vim. Putting it here ensures that it
" will be mapped after other plugins such as supertab.vim.
if !exists('loaded_snips') || exists('s:did_snips_mappings')
	finish
endif
let s:did_snips_mappings = 1

" This is put here in the 'after' directory in order for snipMate to override
" other plugin mappings (e.g., supertab).
"
" To adjust the tirgger key see (:h snipMate-trigger)
"
if !exists('g:snips_trigger_key')
  let g:snips_trigger_key = '<tab>'
endif

if !exists('g:snips_trigger_key_backwards')
  let g:snips_trigger_key_backwards = '<s-' . substitute(g:snips_trigger_key, '[<>]', '', 'g')
endif

exec 'ino <silent> ' . g:snips_trigger_key . ' <c-g>u<c-r>=snipMate#TriggerSnippet()<cr>'
exec 'snor <silent> ' . g:snips_trigger_key . ' <esc>i<right><c-r>=snipMate#TriggerSnippet()<cr>'
exec 'ino <silent> ' . g:snips_trigger_key_backwards . '> <c-r>=snipMate#BackwardsSnippet()<cr>'
exec 'snor <silent> ' . g:snips_trigger_key_backwards . '> <esc>i<right><c-r>=snipMate#BackwardsSnippet()<cr>'
exec 'ino <silent> <c-r>' . g:snips_trigger_key . ' <c-r>=snipMate#ShowAvailableSnips()<cr>'

" maybe there is a better way without polluting registers ?
exec 'xnoremap ' . g:snips_trigger_key. ' s<c-o>:let<space>g:snipmate_content_visual=getreg('1')<cr>'

" The default mappings for these are annoying & sometimes break snipMate.
" You can change them back if you want, I've put them here for convenience.
snor <bs> b<bs>
snor <right> <esc>a
snor <left> <esc>bi
snor ' b<bs>'
snor ` b<bs>`
snor % b<bs>%
snor U b<bs>U
snor ^ b<bs>^
snor \ b<bs>\
snor <c-x> b<bs><c-x>

" By default load snippets in snippets_dir
if empty(snippets_dir)
	finish
endif

" vim:noet:sw=4:ts=4:ft=vim
