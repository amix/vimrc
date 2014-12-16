" Copyright 2013 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" lint.vim: Vim command to lint Go files with golint.
"
"   https://github.com/golang/lint
"
" This filetype plugin add a new commands for go buffers:
"
"   :GoLint
"
"       Run golint for the current Go file.
"
if !exists("g:go_golint_bin")
    let g:go_golint_bin = "golint"
endif

function! go#lint#Run() abort
	let bin_path = go#tool#BinPath(g:go_golint_bin) 
	if empty(bin_path) 
		return 
	endif

    silent cexpr system(bin_path . " " . shellescape(expand('%')))
    cwindow
endfunction


" vim:ts=4:sw=4:et
