scriptencoding utf-8

" ------------------------------------------------------------------------
" Settings initialization
" ------------------------------------------------------------------------
let s:deprecations = {
    \ 'get_definition_command':     'goto_definitions_command',
    \ 'pydoc':                      'documentation_command',
    \ 'related_names_command':      'usages_command',
    \ 'autocompletion_command':     'completions_command',
    \ 'show_function_definition':   'show_call_signatures',
\ }

let s:default_settings = {
    \ 'use_tabs_not_buffers': 0,
    \ 'use_splits_not_buffers': 1,
    \ 'auto_initialization': 1,
    \ 'auto_vim_configuration': 1,
    \ 'goto_command': "'<leader>d'",
    \ 'goto_assignments_command': "'<leader>g'",
    \ 'goto_definitions_command': "''",
    \ 'completions_command': "'<C-Space>'",
    \ 'call_signatures_command': "'<leader>n'",
    \ 'usages_command': "'<leader>n'",
    \ 'rename_command': "'<leader>r'",
    \ 'popup_on_dot': 1,
    \ 'documentation_command': "'K'",
    \ 'show_call_signatures': 1,
    \ 'call_signature_escape': "'=`='",
    \ 'auto_close_doc': 1,
    \ 'max_doc_height': 30,
    \ 'popup_select_first': 1,
    \ 'quickfix_window_height': 10,
    \ 'completions_enabled': 1,
    \ 'force_py_version': "'auto'"
\ }

for [key, val] in items(s:deprecations)
    if exists('g:jedi#'.key)
        echom "'g:jedi#".key."' is deprecated. Please use 'g:jedi#".val."' instead. Sorry for the inconvenience."
        exe 'let g:jedi#'.val.' = g:jedi#'.key
    endif
endfor

for [key, val] in items(s:default_settings)
    if !exists('g:jedi#'.key)
        exe 'let g:jedi#'.key.' = '.val
    endif
endfor


" ------------------------------------------------------------------------
" Python initialization
" ------------------------------------------------------------------------
let s:script_path = fnameescape(expand('<sfile>:p:h:h'))

function! s:init_python()
    if g:jedi#force_py_version != 'auto'
        " Always use the user supplied version.
        try
            return jedi#force_py_version(g:jedi#force_py_version)
        catch
            throw "Could not setup g:jedi#force_py_version: ".v:exception
        endtry
    endif

    " Handle "auto" version.
    if has('nvim') || (has('python') && has('python3'))
        " Neovim usually has both python providers. Skipping the `has` check
        " avoids starting both of them.

        " Get default python version from interpreter in $PATH.
        let s:def_py = system("python -c 'import sys; sys.stdout.write(str(sys.version_info[0]))'")
        if v:shell_error != 0 || !len(s:def_py)
            if !exists("g:jedi#squelch_py_warning")
                echohl WarningMsg
                echom "Warning: jedi-vim failed to get Python version from sys.version_info: " . s:def_py
                echom "Falling back to version 2."
                echohl None
            endif
            let s:def_py = 2
        elseif &verbose
            echom "jedi-vim: auto-detected Python: ".s:def_py
        endif

        " Make sure that the auto-detected version is available in Vim.
        if !has('nvim') || has('python'.(s:def_py == 2 ? '' : s:def_py))
            return jedi#force_py_version(s:def_py)
        endif
    endif

    if has('python')
        call jedi#setup_py_version(2)
    elseif has('python3')
        call jedi#setup_py_version(3)
    else
        throw "jedi-vim requires Vim with support for Python 2 or 3."
    endif
    return 1
endfunction


function! jedi#init_python()
    if !exists('s:_init_python')
        try
            let s:_init_python = s:init_python()
        catch
            if !exists("g:jedi#squelch_py_warning")
                echohl WarningMsg
                echom "Error: jedi-vim failed to initialize Python: ".v:exception." (in ".v:throwpoint.")"
                echohl None
            endif
            let s:_init_python = 0
        endtry
    endif
    return s:_init_python
endfunction


function! jedi#setup_py_version(py_version)
    if a:py_version == 2
        let cmd_init = 'pyfile'
        let cmd_exec = 'python'
    elseif a:py_version == 3
        let cmd_init = 'py3file'
        let cmd_exec = 'python3'
    else
        throw "jedi#setup_py_version: invalid py_version: ".a:py_version
    endif

    try
        execute cmd_init.' '.s:script_path.'/initialize.py'
        execute 'command! -nargs=1 PythonJedi '.cmd_exec.' <args>'
        return 1
    catch
        throw "jedi#setup_py_version: ".v:exception
    endtry
endfunction


function! jedi#force_py_version(py_version)
    let g:jedi#force_py_version = a:py_version
    return jedi#setup_py_version(a:py_version)
endfunction


function! jedi#force_py_version_switch()
    if g:jedi#force_py_version == 2
        call jedi#force_py_version(3)
    elseif g:jedi#force_py_version == 3
        call jedi#force_py_version(2)
    else
        throw "Don't know how to switch from ".g:jedi#force_py_version."!"
    endif
endfunction


" Helper function instead of `python vim.eval()`, and `.command()` because
" these also return error definitions.
function! jedi#_vim_exceptions(str, is_eval)
    let l:result = {}
    try
        if a:is_eval
            let l:result.result = eval(a:str)
        else
            execute a:str
            let l:result.result = ''
        endif
    catch
        let l:result.exception = v:exception
        let l:result.throwpoint = v:throwpoint
    endtry
    return l:result
endfunction


if !jedi#init_python()
    " Do not define any functions when Python initialization failed.
    finish
endif


" ------------------------------------------------------------------------
" functions that call python code
" ------------------------------------------------------------------------
function! jedi#goto()
    PythonJedi jedi_vim.goto(mode="goto")
endfunction

function! jedi#goto_assignments()
    PythonJedi jedi_vim.goto(mode="assignment")
endfunction

function! jedi#goto_definitions()
    PythonJedi jedi_vim.goto(mode="definition")
endfunction

function! jedi#usages()
    PythonJedi jedi_vim.goto(mode="related_name")
endfunction

function! jedi#rename(...)
    PythonJedi jedi_vim.rename()
endfunction

function! jedi#rename_visual(...)
    PythonJedi jedi_vim.rename_visual()
endfunction

function! jedi#completions(findstart, base)
    PythonJedi jedi_vim.completions()
endfunction

function! jedi#enable_speed_debugging()
    PythonJedi jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout, speed=True, warnings=False, notices=False)
endfunction

function! jedi#enable_debugging()
    PythonJedi jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout)
endfunction

function! jedi#disable_debugging()
    PythonJedi jedi_vim.jedi.set_debug_function(None)
endfunction

function! jedi#py_import(args)
    PythonJedi jedi_vim.py_import()
endfun

function! jedi#py_import_completions(argl, cmdl, pos)
    PythonJedi jedi_vim.py_import_completions()
endfun


" ------------------------------------------------------------------------
" show_documentation
" ------------------------------------------------------------------------
function! jedi#show_documentation()
    PythonJedi if jedi_vim.show_documentation() is None: vim.command('return')

    let bn = bufnr("__doc__")
    if bn > 0
        let wi=index(tabpagebuflist(tabpagenr()), bn)
        if wi >= 0
            " If the __doc__ buffer is open in the current tab, jump to it
            silent execute (wi+1).'wincmd w'
        else
            silent execute "sbuffer ".bn
        endif
    else
        split '__doc__'
    endif

    setlocal modifiable
    setlocal noswapfile
    setlocal buftype=nofile
    silent normal! ggdG
    silent $put=l:doc
    silent normal! 1Gdd
    setlocal nomodifiable
    setlocal nomodified
    setlocal filetype=rst

    if l:doc_lines > g:jedi#max_doc_height " max lines for plugin
        let l:doc_lines = g:jedi#max_doc_height
    endif
    execute "resize ".l:doc_lines

    " quit comands
    nnoremap <buffer> q ZQ
    execute "nnoremap <buffer> ".g:jedi#documentation_command." ZQ"

    " highlight python code within rst
    unlet! b:current_syntax
    syn include @rstPythonScript syntax/python.vim
    " 4 spaces
    syn region rstPythonRegion start=/^\v {4}/ end=/\v^( {4}|\n)@!/ contains=@rstPythonScript
    " >>> python code -> (doctests)
    syn region rstPythonRegion matchgroup=pythonDoctest start=/^>>>\s*/ end=/\n/ contains=@rstPythonScript
    let b:current_syntax = "rst"
endfunction

" ------------------------------------------------------------------------
" helper functions
" ------------------------------------------------------------------------

function! jedi#add_goto_window(len)
    set lazyredraw
    cclose
    let height = min([a:len, g:jedi#quickfix_window_height])
    execute 'belowright copen '.height
    set nolazyredraw
    if g:jedi#use_tabs_not_buffers == 1
        noremap <buffer> <CR> :call jedi#goto_window_on_enter()<CR>
    endif
    au WinLeave <buffer> q  " automatically leave, if an option is chosen
    redraw!
endfunction


function! jedi#goto_window_on_enter()
    let l:list = getqflist()
    let l:data = l:list[line('.') - 1]
    if l:data.bufnr
        " close goto_window buffer
        normal ZQ
        PythonJedi jedi_vim.new_buffer(vim.eval('bufname(l:data.bufnr)'))
        call cursor(l:data.lnum, l:data.col)
    else
        echohl WarningMsg | echo "Builtin module cannot be opened." | echohl None
    endif
endfunction


function! s:syn_stack()
    if !exists("*synstack")
        return []
    endif
    return map(synstack(line('.'), col('.') - 1), 'synIDattr(v:val, "name")')
endfunc


function! jedi#do_popup_on_dot_in_highlight()
    let highlight_groups = s:syn_stack()
    for a in highlight_groups
        if a == 'pythonDoctest'
            return 1
        endif
    endfor

    for a in highlight_groups
        for b in ['pythonString', 'pythonComment', 'pythonNumber']
            if a == b
                return 0 
            endif
        endfor
    endfor
    return 1
endfunc


function! jedi#configure_call_signatures()
    if g:jedi#show_call_signatures == 2  " Command line call signatures
        autocmd InsertEnter <buffer> let g:jedi#first_col = s:save_first_col()
    endif
    autocmd InsertLeave <buffer> PythonJedi jedi_vim.clear_call_signatures()
    autocmd CursorMovedI <buffer> PythonJedi jedi_vim.show_call_signatures()
endfunction


" Determine where the current window is on the screen for displaying call
" signatures in the correct column.
function! s:save_first_col()
    if bufname('%') ==# '[Command Line]' || winnr('$') == 1
        return 0
    endif

    let startwin = winnr()
    let winwidth = winwidth(0)
    if winwidth == &columns
        return 0
    elseif winnr('$') == 2
        return startwin == 1 ? 0 : (winwidth(1) + 1)
    elseif winnr('$') == 3
        if startwin == 1
            return 0
        endif
        let ww1 = winwidth(1)
        let ww2 = winwidth(2)
        let ww3 = winwidth(3)
        if ww1 + ww2 + ww3 + 2 == &columns
            if startwin == 2
                return ww1 + 1
            else
                return ww1 + ww2 + 2
            endif
        elseif startwin == 2
            if ww2 + ww3 + 1 == &columns
                return 0
            else
                return ww1 + 1
            endif
        else " startwin == 3
            if ww2 + ww3 + 1 == &columns
                return ww2 + 1
            else
                return ww1 + 1
            endif
        endif
    endif
    return 0
endfunction


function! jedi#complete_string(is_popup_on_dot)

    if a:is_popup_on_dot && !(g:jedi#popup_on_dot && jedi#do_popup_on_dot_in_highlight())
        return ''

    endif
    if pumvisible() && !a:is_popup_on_dot
        return "\<C-n>"
    else
        return "\<C-x>\<C-o>\<C-r>=jedi#complete_opened(".a:is_popup_on_dot.")\<CR>"
    endif
endfunction


function! jedi#complete_opened(is_popup_on_dot)
    if pumvisible()
        " Only go down if it is visible, user-enabled and the longest
        " option is set.
        if g:jedi#popup_select_first && stridx(&completeopt, 'longest') > -1
            return "\<Down>"
        endif
        if a:is_popup_on_dot
            " Prevent completion of the first entry with dot completion.
            return "\<C-p>"
        endif
    endif
    return ""
endfunction


"PythonJedi jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout, speed=True, warnings=False, notices=False)
"PythonJedi jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout)

" vim: set et ts=4:
