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
    \ 'goto_stubs_command': "'<leader>s'",
    \ 'completions_command': "'<C-Space>'",
    \ 'call_signatures_command': "'<leader>n'",
    \ 'usages_command': "'<leader>n'",
    \ 'rename_command': "'<leader>r'",
    \ 'completions_enabled': 1,
    \ 'popup_on_dot': 'g:jedi#completions_enabled',
    \ 'documentation_command': "'K'",
    \ 'show_call_signatures': has('conceal') ? 1 : 2,
    \ 'show_call_signatures_delay': 500,
    \ 'call_signature_escape': "'?!?'",
    \ 'auto_close_doc': 1,
    \ 'max_doc_height': 30,
    \ 'popup_select_first': 1,
    \ 'quickfix_window_height': 10,
    \ 'force_py_version': "'auto'",
    \ 'environment_path': "'auto'",
    \ 'added_sys_path': '[]',
    \ 'project_path': "'auto'",
    \ 'smart_auto_mappings': 0,
    \ 'case_insensitive_completion': 1,
    \ 'use_tag_stack': 1
\ }

for [s:key, s:val] in items(s:deprecations)
    if exists('g:jedi#'.s:key)
        echom "'g:jedi#".s:key."' is deprecated. Please use 'g:jedi#".s:val."' instead. Sorry for the inconvenience."
        exe 'let g:jedi#'.s:val.' = g:jedi#'.s:key
    endif
endfor

for [s:key, s:val] in items(s:default_settings)
    if !exists('g:jedi#'.s:key)
        exe 'let g:jedi#'.s:key.' = '.s:val
    endif
endfor

let s:supports_buffer_usages = has('nvim') || exists('*prop_add')


" ------------------------------------------------------------------------
" Python initialization
" ------------------------------------------------------------------------
let s:script_path = expand('<sfile>:p:h:h')

function! s:init_python() abort
    " Use g:jedi#force_py_version for loading Jedi, or fall back to using
    " `has()` - preferring Python 3.
    if !has('python3')
        throw 'jedi-vim requires Vim with support for Python 3.'
    endif
    call jedi#setup_python_imports()
    return 1
endfunction


function! jedi#reinit_python() abort
    let s:_init_python = -1
    call jedi#init_python()
endfunction


" This is meant to be called with `:unsilent` (for &shortmess+=F).
function! s:display_exception() abort
    let error_lines = split(v:exception, '\n')
    let msg = 'Error: jedi-vim failed to initialize Python: '
                \ .error_lines[0].' (in '.v:throwpoint.')'
    if len(error_lines) > 1
        echohl ErrorMsg
        echom 'jedi-vim error: '.error_lines[0]
        for line in error_lines[1:]
            echom line
        endfor
        echohl None
        let help_cmd = ':JediDebugInfo'
        if exists(':checkhealth') == 2
            let help_cmd .= ' / :checkhealth'
        endif
        let msg .= printf('. See :messages and/or %s for more information.',
              \ help_cmd)
    endif
    redraw  " Redraw to only have the main message by default.
    echoerr msg
endfunction


let s:_init_python = -1
function! jedi#init_python() abort
    if s:_init_python == -1
        let s:_init_python = 0
        try
            let s:_init_python = s:init_python()
            let s:_init_python = 1
        catch /^jedi/
            " Only catch errors from jedi-vim itself here, so that for
            " unexpected Python exceptions the traceback will be shown
            " (e.g. with NameError in jedi#setup_python_imports's code).
            if !exists('g:jedi#squelch_py_warning')
                unsilent call s:display_exception()
            endif
        endtry
    endif
    return s:_init_python
endfunction


function! jedi#setup_python_imports() abort
    let g:_jedi_init_error = 0
    let init_lines = [
          \ 'import vim',
          \ 'def _jedi_handle_exc(exc_info):',
          \ '    try:',
          \ '        from jedi_vim_debug import format_exc_info',
          \ '        vim.vars["_jedi_init_error"] = format_exc_info(exc_info)',
          \ '    except Exception:',
          \ '        import traceback',
          \ '        vim.vars["_jedi_init_error"] = "\\n".join(traceback.format_exception(*exc_info))',
          \ 'try:',
          \ '    import jedi_vim',
          \ '    if hasattr(jedi_vim, "jedi_import_error"):',
          \ '        _jedi_handle_exc(jedi_vim.jedi_import_error)',
          \ 'except Exception as exc:',
          \ '    _jedi_handle_exc(sys.exc_info())',
          \ ]
    exe 'python3 exec('''.escape(join(init_lines, '\n'), "'").''')'
    if g:_jedi_init_error isnot 0
        throw printf('jedi#setup_python_imports: %s', g:_jedi_init_error)
    endif
    return 1
endfunction


function! jedi#debug_info() abort
    if &verbose
      if &filetype !=# 'python'
        echohl WarningMsg | echo 'You should run this in a buffer with filetype "python".' | echohl None
      endif
    endif
    let spath = shellescape(s:script_path)
    echo '#### Jedi-vim debug information'
    echo "\n"
    echo '##### jedi-vim version'
    echo "\n"
    echo ' - jedi-vim git version: '
    echon substitute(system('git -C '.spath.' describe --tags --always --dirty'), '\v\n$', '', '')
    echo ' - jedi git submodule status: '
    echon substitute(system('git -C '.spath.' submodule status pythonx/jedi'), '\v\n$', '', '')
    echo ' - parso git submodule status: '
    echon substitute(system('git -C '.spath.' submodule status pythonx/parso'), '\v\n$', '', '')
    echo "\n"
    echo '##### Global Python'
    echo "\n"
    echo 'Using Python version 3 to access Jedi.'
    let s:pythonjedi_called = 0
    try
      python3 import vim; vim.command('let s:pythonjedi_called = 1')
    catch
      echo 'Error when trying to import vim: '.v:exception
    endtry
    if !s:pythonjedi_called
      echohl WarningMsg
      echom 'python3 failed to run, likely a Python config issue.'
      if exists(':checkhealth') == 2
        echom 'Try :checkhealth for more information.'
      endif
      echohl None
    else
      try
        python3 from jedi_vim_debug import display_debug_info
        python3 display_debug_info()
      catch
        echohl WarningMsg
        echo 'Error when running display_debug_info: '.v:exception
        echohl None
      endtry
    endif
    echo "\n"
    echo '##### Settings'
    echo "\n"
    echo '```'
    let jedi_settings = items(filter(copy(g:), "v:key =~# '\\v^jedi#'"))
    let has_nondefault_settings = 0
    for [k, V] in jedi_settings
      exe 'let default = '.get(s:default_settings,
            \ substitute(k, '\v^jedi#', '', ''), "'-'")
      " vint: -ProhibitUsingUndeclaredVariable
      if default !=# V
        echo printf('g:%s = %s (default: %s)', k, string(V), string(default))
        unlet! V  " Fix variable type mismatch with Vim 7.3.
        let has_nondefault_settings = 1
      endif
      " vint: +ProhibitUsingUndeclaredVariable
    endfor
    if has_nondefault_settings
      echo "\n"
    endif
    verb set omnifunc? completeopt?
    echo '```'

    if &verbose
      echo "\n"
      echo '#### :version'
      echo '```'
      version
      echo '```'
      echo "\n"
      echo '#### :messages'
      echo '```'
      messages
      echo '```'
      echo "\n"
      echo '<details><summary>:scriptnames</summary>'
      echo "\n"
      echo '```'
      scriptnames
      echo '```'
      echo '</details>'
    endif
endfunction

" Helper function instead of `python vim.eval()`, and `.command()` because
" these also return error definitions.
function! jedi#_vim_exceptions(str, is_eval) abort
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

call jedi#init_python()  " Might throw an error.

" ------------------------------------------------------------------------
" functions that call python code
" ------------------------------------------------------------------------
function! jedi#goto() abort
    python3 jedi_vim.goto(mode="goto")
endfunction

function! jedi#goto_assignments() abort
    python3 jedi_vim.goto(mode="assignment")
endfunction

function! jedi#goto_definitions() abort
    python3 jedi_vim.goto(mode="definition")
endfunction

function! jedi#goto_stubs() abort
    python3 jedi_vim.goto(mode="stubs")
endfunction

function! jedi#usages() abort
    if exists('#jedi_usages#BufWinEnter')
        call jedi#clear_usages()
    endif
    python3 jedi_vim.usages()
endfunction

if !s:supports_buffer_usages
" Hide usages in the current window.
" Only handles the current window due to matchdelete() restrictions.
function! jedi#_hide_usages_in_win() abort
    let winnr = winnr()
    let matchids = getwinvar(winnr, '_jedi_usages_vim_matchids', [])

    for matchid in matchids[1:]
        call matchdelete(matchid)
    endfor
    call setwinvar(winnr, '_jedi_usages_vim_matchids', [])

    " Remove the autocommands that might have triggered this function.
    augroup jedi_usages
        exe 'autocmd! * <buffer='.winbufnr(winnr).'>'
    augroup END
    unlet! b:_jedi_usages_needs_clear
endfunction

" Show usages for current window (Vim without textprops only).
function! jedi#_show_usages_in_win() abort
    python3 jedi_vim.highlight_usages_for_vim_win()

    if !exists('#jedi_usages#TextChanged#<buffer>')
        augroup jedi_usages
          " Unset highlights on any changes to this buffer.
          " NOTE: Neovim's API handles movement of highlights, but would only
          " need to clear highlights that are changed inline.
          autocmd TextChanged <buffer> call jedi#_clear_buffer_usages()

          " Hide usages when the buffer is removed from the window, or when
          " entering insert mode (but keep them for later).
          autocmd BufWinLeave,InsertEnter <buffer> call jedi#_hide_usages_in_win()
        augroup END
    endif
endfunction

" Remove usages for the current buffer (and all its windows).
function! jedi#_clear_buffer_usages() abort
    let bufnr = bufnr('%')
    let nvim_src_ids = getbufvar(bufnr, '_jedi_usages_src_ids', [])
    if !empty(nvim_src_ids)
        for src_id in nvim_src_ids
            " TODO: could only clear highlights below/after changed line?!
            call nvim_buf_clear_highlight(bufnr, src_id, 0, -1)
        endfor
    else
        call jedi#_hide_usages_in_win()
    endif
endfunction
endif

" Remove/unset global usages.
function! jedi#clear_usages() abort
    augroup jedi_usages
        autocmd! BufWinEnter
        autocmd! WinEnter
    augroup END

    if !s:supports_buffer_usages
        " Vim without textprops: clear current window,
        " autocommands will clean others on demand.
        call jedi#_hide_usages_in_win()

        " Setup autocommands to clear remaining highlights on WinEnter.
        augroup jedi_usages
        for b in range(1, bufnr('$'))
            if getbufvar(b, '_jedi_usages_needs_clear')
                exe 'autocmd WinEnter <buffer='.b.'> call jedi#_hide_usages_in_win()'
            endif
        endfor
        augroup END
    endif

    python3 jedi_vim.clear_usages()
endfunction

function! jedi#rename(...) abort
    python3 jedi_vim.rename()
endfunction

function! jedi#rename_visual(...) abort
    python3 jedi_vim.rename_visual()
endfunction

function! jedi#completions(findstart, base) abort
    python3 jedi_vim.completions()
endfunction

function! jedi#enable_speed_debugging() abort
    python3 jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout, speed=True, warnings=False, notices=False)
endfunction

function! jedi#enable_debugging() abort
    python3 jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout)
endfunction

function! jedi#disable_debugging() abort
    python3 jedi_vim.jedi.set_debug_function(None)
endfunction

function! jedi#py_import(args) abort
    python3 jedi_vim.py_import()
endfun

function! jedi#choose_environment(args) abort
    python3 jedi_vim.choose_environment()
endfun

function! jedi#load_project(args) abort
    python3 jedi_vim.load_project()
endfun

function! jedi#py_import_completions(argl, cmdl, pos) abort
    python3 jedi_vim.py_import_completions()
endfun

function! jedi#clear_cache(bang) abort
    if a:bang
        python3 jedi_vim.jedi.cache.clear_time_caches(True)
    else
        python3 jedi_vim.jedi.cache.clear_time_caches(False)
    endif
endfunction


" ------------------------------------------------------------------------
" show_documentation
" ------------------------------------------------------------------------
function! jedi#show_documentation() abort
    python3 if jedi_vim.show_documentation() is None: vim.command('return')

    let bn = bufnr('__doc__')
    if bn > 0
        let wi=index(tabpagebuflist(tabpagenr()), bn)
        if wi >= 0
            " If the __doc__ buffer is open in the current tab, jump to it
            silent execute (wi+1).'wincmd w'
        else
            silent execute 'sbuffer '.bn
        endif
    else
        split __doc__
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
    setlocal foldlevel=200 " do not fold in __doc__

    if l:doc_lines > g:jedi#max_doc_height " max lines for plugin
        let l:doc_lines = g:jedi#max_doc_height
    endif
    execute 'resize '.l:doc_lines

    " quit comands
    nnoremap <buffer> q ZQ
    if len(g:jedi#documentation_command)
      execute 'nnoremap <buffer> '.g:jedi#documentation_command.' ZQ'
    endif
endfunction

" ------------------------------------------------------------------------
" helper functions
" ------------------------------------------------------------------------

function! jedi#add_goto_window(for_usages, len) abort
    let height = min([a:len, g:jedi#quickfix_window_height])

    " Use :copen to go to the window always - the user should select an entry.
    execute 'belowright copen '.height

    if &filetype !=# 'qf'
        echoerr printf('jedi-vim: unexpected ft with current window (%s), please report!', &filetype)
    endif
    if g:jedi#use_tabs_not_buffers == 1
        noremap <buffer> <CR> :call jedi#goto_window_on_enter()<CR>
    endif

    augroup jedi_goto_window
        if a:for_usages
            autocmd BufWinLeave <buffer> call jedi#clear_usages()
        else
            autocmd WinLeave <buffer> q  " automatically leave, if an option is chosen
        endif
    augroup END

    if a:for_usages && !has('nvim')
        if s:supports_buffer_usages
            " Setup autocommand for pending highlights with Vim's textprops.
            " (cannot be added to unlisted buffers)
            augroup jedi_usages
              autocmd! BufWinEnter * call s:usages_for_pending_buffers()
            augroup END
        else
            " Setup global autocommand to display any usages for a window.
            " Gets removed when closing the quickfix window that displays them, or
            " when clearing them (e.g. on TextChanged).
            augroup jedi_usages
              autocmd! BufWinEnter,WinEnter * call jedi#_show_usages_in_win()
            augroup END
        endif
    endif
endfunction

" Highlight usages for a buffer if not done so yet (Neovim only).
function! s:usages_for_pending_buffers() abort
    python3 jedi_vim._handle_pending_usages_for_buf()
endfunction


function! jedi#goto_window_on_enter() abort
    let l:list = getqflist()
    let l:data = l:list[line('.') - 1]
    if l:data.bufnr
        " close goto_window buffer
        normal! ZQ
        python3 jedi_vim.set_buffer(vim.eval('bufname(l:data.bufnr)'))
        call cursor(l:data.lnum, l:data.col)
    else
        echohl WarningMsg | echo 'Builtin module cannot be opened.' | echohl None
    endif
endfunction


function! s:syn_stack() abort
    if !exists('*synstack')
        return []
    endif
    return map(synstack(line('.'), col('.') - 1), "synIDattr(v:val, 'name')")
endfunc


function! jedi#do_popup_on_dot_in_highlight() abort
    let highlight_groups = s:syn_stack()
    for a in highlight_groups
        if a ==# 'pythonDoctest'
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


let s:show_call_signatures_last = [0, 0, '']
function! jedi#show_call_signatures() abort
    if s:_init_python == 0
        return 1
    endif
    let [line, col] = [line('.'), col('.')]
    let curline = getline(line)
    let reload_signatures = 1

    " Caching.  On the same line only.
    if line == s:show_call_signatures_last[0]
        " Check if the number of special signs before or after the
        " cursor has not changed since the last call, which means that the
        " argument position was not changed and we can skip repainting.
        let prevcol = s:show_call_signatures_last[1]
        let prevline = s:show_call_signatures_last[2]
        let no_special = '[^,()=]'
        if substitute(curline[:col-2], no_special, '', 'g')
                    \ == substitute(prevline[:prevcol-2], no_special, '', 'g')
                    \ && substitute(curline[(col-2):], no_special, '', 'g')
                    \ == substitute(prevline[(prevcol-2):], no_special, '', 'g')
            let reload_signatures = 0
        endif
    endif
    let s:show_call_signatures_last = [line, col, curline]

    if reload_signatures
        python3 jedi_vim.show_call_signatures()
    endif
endfunction


function! jedi#clear_call_signatures() abort
    if s:_init_python == 0
        return 1
    endif

    let s:show_call_signatures_last = [0, 0, '']
    python3 jedi_vim.clear_call_signatures()
endfunction


function! jedi#configure_call_signatures() abort
    augroup jedi_call_signatures
    autocmd! * <buffer>
    if g:jedi#show_call_signatures == 2  " Command line call signatures
        autocmd InsertEnter <buffer> let g:jedi#first_col = s:save_first_col()
    endif
    autocmd InsertEnter <buffer> let s:show_call_signatures_last = [0, 0, '']
    autocmd InsertLeave <buffer> call jedi#clear_call_signatures()
    if g:jedi#show_call_signatures_delay > 0
        autocmd InsertEnter <buffer> let b:_jedi_orig_updatetime = &updatetime
                    \ | let &updatetime = g:jedi#show_call_signatures_delay
        autocmd InsertLeave <buffer> if exists('b:_jedi_orig_updatetime')
                    \ |   let &updatetime = b:_jedi_orig_updatetime
                    \ |   unlet b:_jedi_orig_updatetime
                    \ | endif
        autocmd CursorHoldI <buffer> call jedi#show_call_signatures()
    else
        autocmd CursorMovedI <buffer> call jedi#show_call_signatures()
    endif
    augroup END
endfunction


" Determine where the current window is on the screen for displaying call
" signatures in the correct column.
function! s:save_first_col() abort
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


function! jedi#complete_string(autocomplete) abort
    if a:autocomplete
        if !(g:jedi#popup_on_dot && jedi#do_popup_on_dot_in_highlight())
            return ''
        endif

        let s:saved_completeopt = &completeopt
        set completeopt-=longest
        set completeopt+=menuone
        set completeopt-=menu
        if &completeopt !~# 'noinsert\|noselect'
            " Patch 775 introduced noinsert and noselect, previously these
            " options didn't exist. Setting them in earlier versions results in
            " errors (E474).
            if has('patch-7.4-775')
                if g:jedi#popup_select_first
                    set completeopt+=noinsert
                else
                    set completeopt+=noselect
                endif
            else
                " To pass the tests we use this, it seems to get the closest to
                " the other options. I'm really not sure if this properly
                " works, but VIM 7.4-775 is already pretty old, so it might not
                " be a problem anymore in a few years.
                set completeopt+=longest
            endif
        endif
    elseif pumvisible()
        return "\<C-n>"
    endif
    return "\<C-x>\<C-o>\<C-r>=jedi#complete_opened(".a:autocomplete.")\<CR>"
endfunction


function! jedi#complete_opened(autocomplete) abort
    if a:autocomplete
        let &completeopt = s:saved_completeopt
        unlet s:saved_completeopt
    elseif pumvisible() && g:jedi#popup_select_first && stridx(&completeopt, 'longest') > -1
        return "\<Down>"
    endif
    return ''
endfunction


function! jedi#smart_auto_mappings() abort
    " Auto put import statement after from module.name<space> and complete
    if search('\m^\s*from\s\+[A-Za-z0-9._]\{1,50}\%#\s*$', 'bcn', line('.'))
        " Enter character and start completion.
        return "\<space>import \<C-r>=jedi#complete_string(1)\<CR>"
    endif
    return "\<space>"
endfunction


function! jedi#setup_completion() abort
    " We need our own omnifunc, so this overrides the omnifunc set by
    " $VIMRUNTIME/ftplugin/python.vim.
    setlocal omnifunc=jedi#completions

    " map ctrl+space for autocompletion
    if g:jedi#completions_command ==# '<C-Space>'
        " In terminals, <C-Space> sometimes equals <Nul>.
        imap <buffer> <Nul> <C-Space>
        smap <buffer> <Nul> <C-Space>
    endif
    if len(g:jedi#completions_command)
        execute 'inoremap <expr> <buffer> '.g:jedi#completions_command.' jedi#complete_string(0)'
        " A separate mapping for select mode: deletes and completes.
        execute 'snoremap <expr> <buffer> '.g:jedi#completions_command." '\<C-g>c'.jedi#complete_string(0)"
    endif
endfunction

"python3 jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout, speed=True, warnings=False, notices=False)
"python3 jedi_vim.jedi.set_debug_function(jedi_vim.print_to_stdout)

" vim: set et ts=4:
