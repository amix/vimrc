" Vim plugin for Racer
" (by Phil Dawes)
"
" 1. Edit the variables below (or override in .vimrc)
" 2. copy this file into .vim/plugin/
" 3. - now in insert mode do 'C-x C-o' to autocomplete the thing at the cursor
"    - in normal mode do 'gd' to go to definition
"    - 'gD' goes to the definition in a new vertical split
"
" (This plugin is best used with the 'hidden' option enabled so that switching buffers doesn't force you to save) 

if exists('g:loaded_racer')
  finish
endif

let g:loaded_racer = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:racer_cmd')
    let path = escape(expand('<sfile>:p:h'), '\') . '/../target/release/'
    if isdirectory(path)
        let s:pathsep = has("win32") ? ';' : ':'
        let $PATH .= s:pathsep . path
    endif
    let g:racer_cmd = 'racer'

    if !(executable(g:racer_cmd))
      echohl WarningMsg | echomsg "No racer executable found in $PATH (" . $PATH . ")"
    endif
endif

if !exists('$RUST_SRC_PATH')
    let s:rust_src_default = 1
    if isdirectory("/usr/local/src/rust/src")
        let $RUST_SRC_PATH="/usr/local/src/rust/src"
    endif
    if isdirectory("/usr/src/rust/src")
        let $RUST_SRC_PATH="/usr/src/rust/src"
    endif
    if isdirectory("C:\\rust\\src")
        let $RUST_SRC_PATH="C:\\rust\\src"
    endif
endif
if !isdirectory($RUST_SRC_PATH)
    if exists('s:rust_src_default')
      echohl WarningMsg | echomsg "No RUST_SRC_PATH environment variable present, nor could default installation be found at: " . $RUST_SRC_PATH
    else
      echohl WarningMsg | echomsg "No directory was found at provided RUST_SRC_PATH: " . $RUST_SRC_PATH
    endif
endif

if !exists('g:racer_experimental_completer')
    let g:racer_experimental_completer = 0
endif

if !exists('g:racer_insert_paren')
    let g:racer_insert_paren = 1
endif

function! RacerGetPrefixCol(base)
    let col = col(".")-1
    let b:racer_col = col
    let b:tmpfname = tempname()
    call writefile(RacerGetBufferContents(a:base), b:tmpfname)
    let cmd = g:racer_cmd." prefix ".line(".")." ".col." ".b:tmpfname
    let res = system(cmd)
    let prefixline = split(res, "\\n")[0]
    let startcol = split(prefixline[7:], ",")[0]
    return startcol
endfunction

function! RacerGetExpCompletions(base)
    let col = strlen(getline('.')) + strlen(a:base)     " use the column from the previous RacerGetPrefixCol() call, since vim ammends it afterwards
    call writefile(RacerGetBufferContents(a:base), b:tmpfname)
    let fname = expand("%:p")
    let cmd = g:racer_cmd." complete ".line(".")." ".col." ".fname." ".b:tmpfname
    let res = system(cmd)

    let typeMap = {
        \ 'Struct' : 's', 'Module' : 'M', 'Function' : 'f',
        \ 'Crate' : 'C', 'Let' : 'v', 'StructField' : 'm',
        \ 'Impl' : 'i', 'Enum' : 'e', 'EnumVariant' : 'E',
        \ 'Type' : 't', 'FnArg' : 'v', 'Trait' : 'T'
        \ }

    let lines = split(res, "\\n")
    let out = []

    for line in lines
        if line =~ "^MATCH"
            let completions = split(line[6:], ",")
            let kind = get(typeMap, completions[4])
            let completion = {'kind' : kind, 'word' : completions[0], 'dup':1 }

            if kind ==# 'f' " function
                let completion['menu'] = substitute(substitute(substitute(join(completions[5:], ','), '\(pub\|fn\) ',"","g"), '{*$', "", ""), ' where\s\?.*$', "", "")
                if g:racer_insert_paren == 1
                    let completion['abbr'] = completions[0]
                    let completion['word'] .= "("
                endif
                let completion['info'] = join(completions[5:], ',')
            elseif kind ==# 's' " struct
                let completion['menu'] = substitute(substitute(join(completions[5:], ','), '\(pub\|struct\) ',"","g"), '{*$', "", "")
            endif

            let out = add(out, completion)
        endif
    endfor
    call delete(b:tmpfname)
    return out
endfunction

function! RacerGetCompletions(base)
    let col = strlen(getline('.')) + strlen(a:base)     " use the column from the previous RacerGetPrefixCol() call, since vim ammends it afterwards
    call writefile(RacerGetBufferContents(a:base), b:tmpfname)
    let fname = expand("%:p")
    let cmd = g:racer_cmd." complete ".line(".")." ".col." ".fname." ".b:tmpfname
    let res = system(cmd)
    let lines = split(res, "\\n")
    let out = []
    for line in lines
       if line =~ "^MATCH"
           let completion = split(line[6:], ",")[0]
           let out = add(out, completion)
       endif
    endfor
    call delete(b:tmpfname)
    return out
endfunction

function! RacerGoToDefinition()
    let col = col(".")-1
    let b:racer_col = col
    let fname = expand("%:p")
    let tmpfname = tempname()
    call writefile(getline(1, '$'), tmpfname)
    let cmd = g:racer_cmd." find-definition ".line(".")." ".col." ".fname." ".tmpfname
    let res = system(cmd)
    let lines = split(res, "\\n")
    for line in lines
        if line =~ "^MATCH"
             let linenum = split(line[6:], ",")[1]
             let colnum = split(line[6:], ",")[2]
             let fname = split(line[6:], ",")[3]
             call RacerJumpToLocation(fname, linenum, colnum)
             break
        endif
    endfor
    call delete(tmpfname)
endfunction

function! RacerGetBufferContents(base)
    " Re-combine the completion base word from omnicomplete with the current
    " line contents. Since the base word gets remove from the buffer before
    " this function is invoked we have to put it back in to out tmpfile.
    let col = col(".")-1
    let buf_lines = getline(1, '$')
    let line_contents = getline('.')
    let buf_lines[line('.') - 1] = strpart(line_contents, 0, col).a:base.strpart(line_contents, col, len(line_contents))
    return buf_lines
endfunction

function! RacerJumpToLocation(filename, linenum, colnum)
    if(a:filename != '')
        " Record jump mark
        normal! m`
        if a:filename != bufname('%')
            exec 'keepjumps e ' . fnameescape(a:filename)
        endif
        call cursor(a:linenum, a:colnum+1)
        " Center definition on screen
        normal! zz
    endif
endfunction

function! RacerComplete(findstart, base)
    if a:findstart
        return RacerGetPrefixCol(a:base)
    else
        if g:racer_experimental_completer == 1
            return RacerGetExpCompletions(a:base)
        else
            return RacerGetCompletions(a:base)
        endif
    endif
endfunction

autocmd FileType rust setlocal omnifunc=RacerComplete
autocmd FileType rust nnoremap <buffer>gd :call RacerGoToDefinition()<cr>
autocmd FileType rust nnoremap <buffer>gD :vsplit<cr>:call RacerGoToDefinition()<cr>

let &cpo = s:save_cpo
unlet s:save_cpo

