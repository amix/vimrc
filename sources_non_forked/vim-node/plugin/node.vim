if exists("g:loaded_node") || &cp || v:version < 700 | finish | endif
let g:loaded_node = 1

function! s:detect(path)
	if exists("b:node_root") | return | endif
	let path = a:path

	while 1
		let is_node = 0
		let is_node = is_node || filereadable(path . "/package.json")
		let is_node = is_node || isdirectory(path . "/node_modules")
		if is_node | return node#initialize(path) | endif

		let parent = fnamemodify(path, ":h")
		if parent == path | return | endif
		let path = parent
	endwhile
endfunction

augroup Node
	au!
	au VimEnter * if empty(expand("<amatch>")) | call s:detect(getcwd()) | endif
	au BufRead,BufNewFile * call s:detect(expand("<amatch>:p"))
augroup end
