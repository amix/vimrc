" plugin/editorconfig.vim: EditorConfig native Vimscript plugin file
" Copyright (c) 2011-2019 EditorConfig Team
" All rights reserved.
"
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions are met:
"
" 1. Redistributions of source code must retain the above copyright notice,
"    this list of conditions and the following disclaimer.
" 2. Redistributions in binary form must reproduce the above copyright notice,
"    this list of conditions and the following disclaimer in the documentation
"    and/or other materials provided with the distribution.
"
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
" IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
" ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
" LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
" CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
" SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
" INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
" CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
" ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
" POSSIBILITY OF SUCH DAMAGE.
"

" check for Vim versions and duplicate script loading.
if v:version < 700 || exists("g:loaded_EditorConfig")
    finish
endif
let g:loaded_EditorConfig = 1

let s:saved_cpo = &cpo
set cpo&vim

" variables {{{1

" Make sure the globals all exist
if !exists('g:EditorConfig_exec_path')
    let g:EditorConfig_exec_path = ''
endif

if !exists('g:EditorConfig_verbose')
    let g:EditorConfig_verbose = 0
endif

if !exists('g:EditorConfig_preserve_formatoptions')
    let g:EditorConfig_preserve_formatoptions = 0
endif

if !exists('g:EditorConfig_max_line_indicator')
    let g:EditorConfig_max_line_indicator = 'line'
endif

if !exists('g:EditorConfig_exclude_patterns')
    let g:EditorConfig_exclude_patterns = []
endif

if !exists('g:EditorConfig_disable_rules')
    let g:EditorConfig_disable_rules = []
endif

if !exists('g:EditorConfig_enable_for_new_buf')
    let g:EditorConfig_enable_for_new_buf = 0
endif

if !exists('g:EditorConfig_softtabstop_space')
    let g:EditorConfig_softtabstop_space = 1
endif

if !exists('g:EditorConfig_softtabstop_tab')
    let g:EditorConfig_softtabstop_tab = 1
endif

" Copy some of the globals into script variables --- changes to these
" globals won't affect the plugin until the plugin is reloaded.
if exists('g:EditorConfig_core_mode') && !empty(g:EditorConfig_core_mode)
    let s:editorconfig_core_mode = g:EditorConfig_core_mode
else
    let s:editorconfig_core_mode = ''
endif

if exists('g:EditorConfig_exec_path') && !empty(g:EditorConfig_exec_path)
    let s:editorconfig_exec_path = g:EditorConfig_exec_path
else
    let s:editorconfig_exec_path = ''
endif

let s:initialized = 0

" }}}1

" shellslash handling {{{1
function! s:DisableShellSlash() " {{{2
    " disable shellslash for proper escaping of Windows paths

    " In Windows, 'shellslash' also changes the behavior of 'shellescape'.
    " It makes 'shellescape' behave like in UNIX environment. So ':setl
    " noshellslash' before evaluating 'shellescape' and restore the
    " settings afterwards when 'shell' does not contain 'sh' somewhere.
    if has('win32') && empty(matchstr(&shell, 'sh'))
        let s:old_shellslash = &l:shellslash
        setlocal noshellslash
    endif
endfunction " }}}2

function! s:ResetShellSlash() " {{{2
    " reset shellslash to the user-set value, if any
    if exists('s:old_shellslash')
        let &l:shellslash = s:old_shellslash
        unlet! s:old_shellslash
    endif
endfunction " }}}2
" }}}1

" Mode initialization functions {{{1

function! s:InitializeVimCore()
" Initialize vim core.  Returns 1 on failure; 0 on success
" At the moment, all we need to do is to check that it is installed.
    try
        let l:vim_core_ver = editorconfig_core#version()
    catch
        return 1
    endtry
    return 0
endfunction

function! s:InitializeExternalCommand()
" Initialize external_command mode

    if empty(s:editorconfig_exec_path)
        echo 'Please specify a g:EditorConfig_exec_path'
        return 1
    endif

    if g:EditorConfig_verbose
        echo 'Checking for external command ' . s:editorconfig_exec_path . ' ...'
    endif

    if !executable(s:editorconfig_exec_path)
        echo 'File ' . s:editorconfig_exec_path . ' is not executable.'
        return 1
    endif

    return 0
endfunction
" }}}1

function! s:Initialize() " Initialize the plugin.  {{{1
    " Returns truthy on error, falsy on success.

    if empty(s:editorconfig_core_mode)
        let s:editorconfig_core_mode = 'vim_core'   " Default core choice
    endif

    if s:editorconfig_core_mode ==? 'external_command'
        if s:InitializeExternalCommand()
            echohl WarningMsg
            echo 'EditorConfig: Failed to initialize external_command mode.  ' .
                \ 'Falling back to vim_core mode.'
            echohl None
            let s:editorconfig_core_mode = 'vim_core'
        endif
    endif

    if s:editorconfig_core_mode ==? 'vim_core'
        if s:InitializeVimCore()
            echohl ErrorMsg
            echo 'EditorConfig: Failed to initialize vim_core mode.  ' .
                \ 'The plugin will not function.'
            echohl None
            return 1
        endif

    elseif s:editorconfig_core_mode ==? 'external_command'
        " Nothing to do here, but this elseif is required to avoid
        " external_command falling into the else clause.

    else    " neither external_command nor vim_core
        echohl ErrorMsg
        echo "EditorConfig: I don't know how to use mode " . s:editorconfig_core_mode
        echohl None
        return 1
    endif

    let s:initialized = 1
    return 0
endfunction " }}}1

function! s:GetFilenames(path, filename) " {{{1
" Yield full filepath for filename in each directory in and above path

    let l:path_list = []
    let l:path = a:path
    while 1
        let l:path_list += [l:path . '/' . a:filename]
        let l:newpath = fnamemodify(l:path, ':h')
        if l:path == l:newpath
            break
        endif
        let l:path = l:newpath
    endwhile
    return l:path_list
endfunction " }}}1

function! s:UseConfigFiles() abort " Apply config to the current buffer {{{1
    let b:editorconfig_tried = 1
    let l:buffer_name = expand('%:p')

    " Only process normal buffers (do not treat help files as '.txt' files)
    " When starting Vim with a directory, the buftype might not yet be set:
    " Therefore, also check if buffer_name is a directory.
    if index(['', 'acwrite'], &buftype) == -1 || isdirectory(l:buffer_name)
        return
    endif

    if empty(l:buffer_name)
        if g:EditorConfig_enable_for_new_buf
            let l:buffer_name = getcwd() . "/."
        else
            return
        endif
    endif

    if exists("b:EditorConfig_disable") && b:EditorConfig_disable
        if g:EditorConfig_verbose
            echo 'Skipping EditorConfig for buffer "' . l:buffer_name . '"'
        endif
        return
    endif

    " Check if any .editorconfig does exist
    let l:conf_files = s:GetFilenames(expand('%:p:h'), '.editorconfig')
    let l:conf_found = 0
    for conf_file in conf_files
        if filereadable(conf_file)
            let l:conf_found = 1
            break
        endif
    endfor
    if !l:conf_found
        return
    endif

    if !s:initialized
        if s:Initialize()
            return
        endif
    endif

    if g:EditorConfig_verbose
        echo 'Applying EditorConfig ' . s:editorconfig_core_mode .
            \ ' on file "' . l:buffer_name . '"'
    endif

    " Ignore specific patterns
    for pattern in g:EditorConfig_exclude_patterns
        if l:buffer_name =~ pattern
            return
        endif
    endfor

    if s:editorconfig_core_mode ==? 'vim_core'
        if s:UseConfigFiles_VimCore(l:buffer_name) == 0
            let b:editorconfig_applied = 1
        endif
    elseif s:editorconfig_core_mode ==? 'external_command'
        call s:UseConfigFiles_ExternalCommand(l:buffer_name)
        let b:editorconfig_applied = 1
    else
        echohl Error |
                    \ echo "Unknown EditorConfig Core: " .
                    \ s:editorconfig_core_mode |
                    \ echohl None
    endif
endfunction " }}}1

" Custom commands, and autoloading {{{1

" Autocommands, and function to enable/disable the plugin {{{2
function! s:EditorConfigEnable(should_enable)
    augroup editorconfig
        autocmd!
        if a:should_enable
            autocmd BufNewFile,BufReadPost,BufFilePost * call s:UseConfigFiles()
            autocmd VimEnter,BufNew * call s:UseConfigFiles()
        endif
    augroup END
endfunction

" }}}2

" Commands {{{2
command! EditorConfigEnable call s:EditorConfigEnable(1)
command! EditorConfigDisable call s:EditorConfigEnable(0)

command! EditorConfigReload call s:UseConfigFiles() " Reload EditorConfig files
" }}}2

" On startup, enable the autocommands
call s:EditorConfigEnable(1)

" }}}1

" UseConfigFiles function for different modes {{{1

function! s:UseConfigFiles_VimCore(target)
" Use the vimscript EditorConfig core
    try
        let l:config = editorconfig_core#handler#get_configurations(
            \ { 'target': a:target } )
        call s:ApplyConfig(l:config)
        return 0    " success
    catch
        return 1    " failure
    endtry
endfunction

function! s:UseConfigFiles_ExternalCommand(target)
" Use external EditorConfig core (e.g., the C core)

    call s:DisableShellSlash()
    let l:exec_path = shellescape(s:editorconfig_exec_path)
    call s:ResetShellSlash()

    call s:SpawnExternalParser(l:exec_path, a:target)
endfunction

function! s:SpawnExternalParser(cmd, target) " {{{2
" Spawn external EditorConfig. Used by s:UseConfigFiles_ExternalCommand()

    let l:cmd = a:cmd

    if empty(l:cmd)
        throw 'No cmd provided'
    endif

    let l:config = {}

    call s:DisableShellSlash()
    let l:cmd = l:cmd . ' ' . shellescape(a:target)
    call s:ResetShellSlash()

    let l:parsing_result = split(system(l:cmd), '\v[\r\n]+')

    " if editorconfig core's exit code is not zero, give out an error
    " message
    if v:shell_error != 0
        echohl ErrorMsg
        echo 'Failed to execute "' . l:cmd . '". Exit code: ' .
                    \ v:shell_error
        echo ''
        echo 'Message:'
        echo l:parsing_result
        echohl None
        return
    endif

    if g:EditorConfig_verbose
        echo 'Output from EditorConfig core executable:'
        echo l:parsing_result
    endif

    for one_line in l:parsing_result
        let l:eq_pos = stridx(one_line, '=')

        if l:eq_pos == -1 " = is not found. Skip this line
            continue
        endif

        let l:eq_left = strpart(one_line, 0, l:eq_pos)
        if l:eq_pos + 1 < strlen(one_line)
            let l:eq_right = strpart(one_line, l:eq_pos + 1)
        else
            let l:eq_right = ''
        endif

        let l:config[l:eq_left] = l:eq_right
    endfor

    call s:ApplyConfig(l:config)
endfunction " }}}2

" }}}1

function! s:ApplyConfig(config) abort " Set the buffer options {{{1
    if g:EditorConfig_verbose
        echo 'Options: ' . string(a:config)
    endif

    if s:IsRuleActive('indent_style', a:config)
        if a:config["indent_style"] == "tab"
            setl noexpandtab
        elseif a:config["indent_style"] == "space"
            setl expandtab
        endif
    endif

    if s:IsRuleActive('tab_width', a:config)
        let &l:tabstop = str2nr(a:config["tab_width"])
    endif

    if s:IsRuleActive('indent_size', a:config)
        " if indent_size is 'tab', set shiftwidth to tabstop;
        " if indent_size is a positive integer, set shiftwidth to the integer
        " value
        if a:config["indent_size"] == "tab"
            let &l:shiftwidth = &l:tabstop
            if type(g:EditorConfig_softtabstop_tab) != type([])
                let &l:softtabstop = g:EditorConfig_softtabstop_tab > 0 ?
                            \ &l:shiftwidth : g:EditorConfig_softtabstop_tab
            endif
        else
            let l:indent_size = str2nr(a:config["indent_size"])
            if l:indent_size > 0
                let &l:shiftwidth = l:indent_size
                if type(g:EditorConfig_softtabstop_space) != type([])
                    let &l:softtabstop = g:EditorConfig_softtabstop_space > 0 ?
                            \ &l:shiftwidth : g:EditorConfig_softtabstop_space
                endif
            endif
        endif

    endif

    if s:IsRuleActive('end_of_line', a:config) &&
                \ &l:modifiable
        if a:config["end_of_line"] == "lf"
            setl fileformat=unix
        elseif a:config["end_of_line"] == "crlf"
            setl fileformat=dos
        elseif a:config["end_of_line"] == "cr"
            setl fileformat=mac
        endif
    endif

    if s:IsRuleActive('charset', a:config) &&
                \ &l:modifiable
        if a:config["charset"] == "utf-8"
            setl fileencoding=utf-8
            setl nobomb
        elseif a:config["charset"] == "utf-8-bom"
            setl fileencoding=utf-8
            setl bomb
        elseif a:config["charset"] == "latin1"
            setl fileencoding=latin1
            setl nobomb
        elseif a:config["charset"] == "utf-16be"
            setl fileencoding=utf-16be
            setl bomb
        elseif a:config["charset"] == "utf-16le"
            setl fileencoding=utf-16le
            setl bomb
        endif
    endif

    augroup editorconfig_trim_trailing_whitespace
        autocmd! BufWritePre <buffer>
        if s:IsRuleActive('trim_trailing_whitespace', a:config) &&
                    \ get(a:config, 'trim_trailing_whitespace', 'false') ==# 'true'
            autocmd BufWritePre <buffer> call s:TrimTrailingWhitespace()
        endif
    augroup END

    if s:IsRuleActive('insert_final_newline', a:config)
        if exists('+fixendofline')
            if a:config["insert_final_newline"] == "false"
                setl nofixendofline
            else
                setl fixendofline
            endif
        elseif  exists(':SetNoEOL') == 2
            if a:config["insert_final_newline"] == "false"
                silent! SetNoEOL    " Use the PreserveNoEOL plugin to accomplish it
            endif
        endif
    endif

    " highlight the columns following max_line_length
    if s:IsRuleActive('max_line_length', a:config) &&
                \ a:config['max_line_length'] != 'off'
        let l:max_line_length = str2nr(a:config['max_line_length'])

        if l:max_line_length >= 0
            let &l:textwidth = l:max_line_length
            if g:EditorConfig_preserve_formatoptions == 0
                setlocal formatoptions+=tc
            endif
        endif

        if exists('+colorcolumn')
            if l:max_line_length > 0
                if g:EditorConfig_max_line_indicator == 'line'
                    setlocal colorcolumn+=+1
                elseif g:EditorConfig_max_line_indicator == 'fill' &&
                            \ l:max_line_length < &l:columns
                    " Fill only if the columns of screen is large enough
                    let &l:colorcolumn = join(
                                \ range(l:max_line_length+1,&l:columns),',')
                elseif g:EditorConfig_max_line_indicator == 'exceeding'
                    let &l:colorcolumn = ''
                    for l:match in getmatches()
                        if get(l:match, 'group', '') == 'ColorColumn'
                            call matchdelete(get(l:match, 'id'))
                        endif
                    endfor
                    call matchadd('ColorColumn',
                        \ '\%' . (l:max_line_length + 1) . 'v.', 100)
                elseif g:EditorConfig_max_line_indicator == 'fillexceeding'
                    let &l:colorcolumn = ''
                    for l:match in getmatches()
                        if get(l:match, 'group', '') == 'ColorColumn'
                            call matchdelete(get(l:match, 'id'))
                        endif
                    endfor
                    call matchadd('ColorColumn',
                        \ '\%'. (l:max_line_length + 1) . 'v.\+', -1)
                endif
            endif
        endif
    endif

    call editorconfig#ApplyHooks(a:config)
endfunction

" }}}1

function! s:TrimTrailingWhitespace() " {{{1
    if &l:modifiable
        " don't lose user position when trimming trailing whitespace
        let s:view = winsaveview()
        try
            silent! keeppatterns keepjumps %s/\s\+$//e
        finally
            call winrestview(s:view)
        endtry
    endif
endfunction " }}}1

function! s:IsRuleActive(name, config) " {{{1
    return index(g:EditorConfig_disable_rules, a:name) < 0 &&
                 \ has_key(a:config, a:name)
endfunction "}}}1

let &cpo = s:saved_cpo
unlet! s:saved_cpo

" vim: fdm=marker fdc=3
