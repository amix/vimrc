"============================================================================
"File:        syntastic.vim
"Description: Vim plugin for on the fly syntax checking.
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_plugin') || &compatible
    finish
endif
let g:loaded_syntastic_plugin = 1

if has('reltime')
    let g:_SYNTASTIC_START = reltime()
    lockvar! g:_SYNTASTIC_START
endif

let g:_SYNTASTIC_VERSION = '3.7.0-144'
lockvar g:_SYNTASTIC_VERSION

" Sanity checks {{{1

if v:version < 700 || (v:version == 700 && !has('patch175'))
    call syntastic#log#error('need Vim version 7.0.175 or later')
    finish
endif

for s:feature in [
            \ 'autocmd',
            \ 'eval',
            \ 'file_in_path',
            \ 'modify_fname',
            \ 'quickfix',
            \ 'reltime',
            \ 'user_commands'
        \ ]
    if !has(s:feature)
        call syntastic#log#error('need Vim compiled with feature ' . s:feature)
        finish
    endif
endfor

let s:_running_windows = syntastic#util#isRunningWindows()
lockvar s:_running_windows

if !exists('g:syntastic_shell')
    let g:syntastic_shell = &shell
endif

if s:_running_windows
    let g:_SYNTASTIC_UNAME = 'Windows'
elseif executable('uname')
    try
        let g:_SYNTASTIC_UNAME = split(syntastic#util#system('uname'), "\n")[0]
    catch /\m^Vim\%((\a\+)\)\=:E484/
        call syntastic#log#error("your shell " .  syntastic#util#var('shell') . " can't handle traditional UNIX syntax for redirections")
        finish
    catch /\m^Vim\%((\a\+)\)\=:E684/
        let g:_SYNTASTIC_UNAME = 'Unknown'
    endtry
else
    let g:_SYNTASTIC_UNAME = 'Unknown'
endif
lockvar g:_SYNTASTIC_UNAME

" }}}1

" Defaults {{{1

let g:_SYNTASTIC_DEFAULTS = {
        \ 'aggregate_errors':         0,
        \ 'always_populate_loc_list': 0,
        \ 'auto_jump':                0,
        \ 'auto_loc_list':            2,
        \ 'check_on_open':            0,
        \ 'check_on_wq':              1,
        \ 'cursor_columns':           1,
        \ 'debug':                    0,
        \ 'echo_current_error':       1,
        \ 'enable_balloons':          1,
        \ 'enable_highlighting':      1,
        \ 'enable_signs':             1,
        \ 'error_symbol':             '>>',
        \ 'exit_checks':              !(s:_running_windows && syntastic#util#var('shell', &shell) =~? '\m\<cmd\.exe$'),
        \ 'filetype_map':             {},
        \ 'full_redraws':             !(has('gui_running') || has('gui_macvim')),
        \ 'id_checkers':              1,
        \ 'ignore_extensions':        '\c\v^([gx]?z|lzma|bz2)$',
        \ 'ignore_files':             [],
        \ 'loc_list_height':          10,
        \ 'nested_autocommands':      0,
        \ 'quiet_messages':           {},
        \ 'reuse_loc_lists':          1,
        \ 'shell':                    &shell,
        \ 'sort_aggregated_errors':   1,
        \ 'stl_format':               '[Syntax: line:%F (%t)]',
        \ 'style_error_symbol':       'S>',
        \ 'style_warning_symbol':     'S>',
        \ 'warning_symbol':           '>>'
    \ }
lockvar! g:_SYNTASTIC_DEFAULTS

for s:key in keys(g:_SYNTASTIC_DEFAULTS)
    if !exists('g:syntastic_' . s:key)
        let g:syntastic_{s:key} = copy(g:_SYNTASTIC_DEFAULTS[s:key])
    endif
endfor

if exists('g:syntastic_quiet_warnings')
    call syntastic#log#oneTimeWarn("variable g:syntastic_quiet_warnings is deprecated, please use let g:syntastic_quiet_messages = {'level': 'warnings'} instead")
    if g:syntastic_quiet_warnings
        let s:quiet_warnings = get(g:syntastic_quiet_messages, 'type', [])
        if type(s:quiet_warnings) != type([])
            let s:quiet_warnings = [s:quiet_warnings]
        endif
        call add(s:quiet_warnings, 'warnings')
        let g:syntastic_quiet_messages['type'] = s:quiet_warnings
    endif
endif

" }}}1

" Debug {{{1

let s:_DEBUG_DUMP_OPTIONS = [
        \ 'shell',
        \ 'shellcmdflag',
        \ 'shellpipe',
        \ 'shellquote',
        \ 'shellredir',
        \ 'shellslash',
        \ 'shelltemp',
        \ 'shellxquote'
    \ ]
if exists('+shellxescape')
    call add(s:_DEBUG_DUMP_OPTIONS, 'shellxescape')
endif
lockvar! s:_DEBUG_DUMP_OPTIONS

" debug constants
let     g:_SYNTASTIC_DEBUG_TRACE         = 1
lockvar g:_SYNTASTIC_DEBUG_TRACE
let     g:_SYNTASTIC_DEBUG_LOCLIST       = 2
lockvar g:_SYNTASTIC_DEBUG_LOCLIST
let     g:_SYNTASTIC_DEBUG_NOTIFICATIONS = 4
lockvar g:_SYNTASTIC_DEBUG_NOTIFICATIONS
let     g:_SYNTASTIC_DEBUG_AUTOCOMMANDS  = 8
lockvar g:_SYNTASTIC_DEBUG_AUTOCOMMANDS
let     g:_SYNTASTIC_DEBUG_VARIABLES     = 16
lockvar g:_SYNTASTIC_DEBUG_VARIABLES
let     g:_SYNTASTIC_DEBUG_CHECKERS      = 32
lockvar g:_SYNTASTIC_DEBUG_CHECKERS

" }}}1

runtime! plugin/syntastic/*.vim

let s:registry = g:SyntasticRegistry.Instance()
let s:notifiers = g:SyntasticNotifiers.Instance()
let s:modemap = g:SyntasticModeMap.Instance()

let s:_quit_pre = []

" Commands {{{1

" @vimlint(EVL103, 1, a:cursorPos)
" @vimlint(EVL103, 1, a:cmdLine)
" @vimlint(EVL103, 1, a:argLead)
function! s:CompleteCheckerName(argLead, cmdLine, cursorPos) abort " {{{2
    let checker_names = []
    for ft in s:_resolve_filetypes([])
        call extend(checker_names, s:registry.getNamesOfAvailableCheckers(ft))
    endfor
    return join(checker_names, "\n")
endfunction " }}}2
" @vimlint(EVL103, 0, a:cursorPos)
" @vimlint(EVL103, 0, a:cmdLine)
" @vimlint(EVL103, 0, a:argLead)


" @vimlint(EVL103, 1, a:cursorPos)
" @vimlint(EVL103, 1, a:cmdLine)
" @vimlint(EVL103, 1, a:argLead)
function! s:CompleteFiletypes(argLead, cmdLine, cursorPos) abort " {{{2
    return join(s:registry.getKnownFiletypes(), "\n")
endfunction " }}}2
" @vimlint(EVL103, 0, a:cursorPos)
" @vimlint(EVL103, 0, a:cmdLine)
" @vimlint(EVL103, 0, a:argLead)

command! -bar -nargs=* -complete=custom,s:CompleteCheckerName SyntasticCheck call SyntasticCheck(<f-args>)
command! -bar -nargs=? -complete=custom,s:CompleteFiletypes   SyntasticInfo  call SyntasticInfo(<f-args>)
command! -bar Errors              call SyntasticErrors()
command! -bar SyntasticReset      call SyntasticReset()
command! -bar SyntasticToggleMode call SyntasticToggleMode()
command! -bar SyntasticSetLoclist call SyntasticSetLoclist()

command! SyntasticJavacEditClasspath runtime! syntax_checkers/java/*.vim | SyntasticJavacEditClasspath
command! SyntasticJavacEditConfig    runtime! syntax_checkers/java/*.vim | SyntasticJavacEditConfig

" }}}1

" Public API {{{1

function! SyntasticCheck(...) abort " {{{2
    call s:UpdateErrors(0, a:000)
    call syntastic#util#redraw(g:syntastic_full_redraws)
endfunction " }}}2

function! SyntasticInfo(...) abort " {{{2
    call s:modemap.modeInfo(a:000)
    call s:registry.echoInfoFor(s:_resolve_filetypes(a:000))
    call s:_explain_skip(a:000)
endfunction " }}}2

function! SyntasticErrors() abort " {{{2
    call g:SyntasticLoclist.current().show()
endfunction " }}}2

function! SyntasticReset() abort " {{{2
    call s:ClearCache()
    call s:notifiers.refresh(g:SyntasticLoclist.New([]))
endfunction " }}}2

function! SyntasticToggleMode() abort " {{{2
    call s:modemap.toggleMode()
    call s:ClearCache()
    call s:notifiers.refresh(g:SyntasticLoclist.New([]))
    call s:modemap.echoMode()
endfunction " }}}2

function! SyntasticSetLoclist() abort " {{{2
    call g:SyntasticLoclist.current().setloclist()
endfunction " }}}2

" }}}1

" Autocommands {{{1

augroup syntastic
    autocmd!
    autocmd BufEnter * call s:BufEnterHook()
augroup END

if g:syntastic_nested_autocommands
    augroup syntastic
        autocmd BufReadPost  * nested call s:BufReadPostHook()
        autocmd BufWritePost * nested call s:BufWritePostHook()
    augroup END
else
    augroup syntastic
        autocmd BufReadPost  * call s:BufReadPostHook()
        autocmd BufWritePost * call s:BufWritePostHook()
    augroup END
endif

if exists('##QuitPre')
    " QuitPre was added in Vim 7.3.544
    augroup syntastic
        autocmd QuitPre * call s:QuitPreHook(expand('<amatch>', 1))
    augroup END
endif

function! s:BufReadPostHook() abort " {{{2
    if g:syntastic_check_on_open
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_AUTOCOMMANDS,
            \ 'autocmd: BufReadPost, buffer ' . bufnr('') . ' = ' . string(bufname(str2nr(bufnr('')))))
        call s:UpdateErrors(1, [])
    endif
endfunction " }}}2

function! s:BufWritePostHook() abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_AUTOCOMMANDS,
        \ 'autocmd: BufWritePost, buffer ' . bufnr('') . ' = ' . string(bufname(str2nr(bufnr('')))))
    call s:UpdateErrors(1, [])
endfunction " }}}2

function! s:BufEnterHook() abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_AUTOCOMMANDS,
        \ 'autocmd: BufEnter, buffer ' . bufnr('') . ' = ' . string(bufname(str2nr(bufnr('')))) .
        \ ', &buftype = ' . string(&buftype))
    if &buftype ==# ''
        call s:notifiers.refresh(g:SyntasticLoclist.current())
    elseif &buftype ==# 'quickfix'
        " TODO: this is needed because in recent versions of Vim lclose
        " can no longer be called from BufWinLeave
        " TODO: at this point there is no b:syntastic_loclist
        let loclist = filter(copy(getloclist(0)), 'v:val["valid"] == 1')
        let owner = str2nr(getbufvar(bufnr(''), 'syntastic_owner_buffer'))
        let buffers = syntastic#util#unique(map(loclist, 'v:val["bufnr"]') + (owner ? [owner] : []))
        if !empty(get(w:, 'syntastic_loclist_set', [])) && !empty(loclist) && empty(filter( buffers, 'syntastic#util#bufIsActive(v:val)' ))
            call SyntasticLoclistHide()
        endif
    endif
endfunction " }}}2

function! s:QuitPreHook(fname) abort " {{{2
    let buf = bufnr(fnameescape(a:fname))
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_AUTOCOMMANDS, 'autocmd: QuitPre, buffer ' . buf . ' = ' . string(a:fname))

    if !syntastic#util#var('check_on_wq')
        call syntastic#util#setWids()
        call add(s:_quit_pre, buf . '_' . getbufvar(buf, 'changetick') . '_' . w:syntastic_wid)
    endif

    if !empty(get(w:, 'syntastic_loclist_set', []))
        call SyntasticLoclistHide()
    endif
endfunction " }}}2

" }}}1

" Main {{{1

"refresh and redraw all the error info for this buf when saving or reading
function! s:UpdateErrors(auto_invoked, checker_names) abort " {{{2
    call syntastic#log#debugShowVariables(g:_SYNTASTIC_DEBUG_TRACE, 'version')
    call syntastic#log#debugShowOptions(g:_SYNTASTIC_DEBUG_TRACE, s:_DEBUG_DUMP_OPTIONS)
    call syntastic#log#debugDump(g:_SYNTASTIC_DEBUG_VARIABLES)
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'UpdateErrors' . (a:auto_invoked ? ' (auto)' : '') .
        \ ': ' . (len(a:checker_names) ? join(a:checker_names) : 'default checkers'))

    call s:modemap.synch()

    if s:_skip_file()
        return
    endif

    let run_checks = !a:auto_invoked || s:modemap.doAutoChecking()
    if run_checks
        call s:CacheErrors(a:checker_names)
        call syntastic#util#setChangedtick()
    else
        if a:auto_invoked
            return
        endif
    endif

    let loclist = g:SyntasticLoclist.current()

    if exists('*SyntasticCheckHook')
        call SyntasticCheckHook(loclist.getRaw())
    endif

    " populate loclist and jump {{{3
    let do_jump = syntastic#util#var('auto_jump') + 0
    if do_jump == 2
        let do_jump = loclist.getFirstError(1)
    elseif do_jump == 3
        let do_jump = loclist.getFirstError()
    elseif 0 > do_jump || do_jump > 3
        let do_jump = 0
    endif

    let w:syntastic_loclist_set = []
    if syntastic#util#var('always_populate_loc_list') || do_jump
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'loclist: setloclist (new)')
        call setloclist(0, loclist.getRaw())
        if !exists('b:syntastic_changedtick')
            call syntastic#util#setChangedtick()
        endif
        let w:syntastic_loclist_set = [bufnr(''), b:syntastic_changedtick]
        if run_checks && do_jump && !loclist.isEmpty()
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'loclist: jump')
            execute 'silent! lrewind ' . do_jump

            " XXX: Vim doesn't call autocmd commands in a predictible
            " order, which can lead to missing filetype when jumping
            " to a new file; the following is a workaround for the
            " resulting brain damage
            if &filetype ==# ''
                silent! filetype detect
            endif
        endif
    endif
    " }}}3

    call s:notifiers.refresh(loclist)
endfunction " }}}2

"clear the loc list for the buffer
function! s:ClearCache() abort " {{{2
    call s:notifiers.reset(g:SyntasticLoclist.current())
    call b:syntastic_loclist.destroy()
endfunction " }}}2

"detect and cache all syntax errors in this buffer
function! s:CacheErrors(checker_names) abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'CacheErrors: ' .
        \ (len(a:checker_names) ? join(a:checker_names) : 'default checkers'))
    call s:ClearCache()
    let newLoclist = g:SyntasticLoclist.New([])

    if !s:_skip_file()
        " debug logging {{{3
        call syntastic#log#debugShowVariables(g:_SYNTASTIC_DEBUG_TRACE, 'aggregate_errors')
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_CHECKERS, '$PATH = ' . string($PATH))
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'getcwd() = ' . string(getcwd()))
        " }}}3

        let filetypes = s:_resolve_filetypes([])
        let aggregate_errors = syntastic#util#var('aggregate_errors') || len(filetypes) > 1
        let decorate_errors = aggregate_errors && syntastic#util#var('id_checkers')
        let sort_aggregated_errors = aggregate_errors && syntastic#util#var('sort_aggregated_errors')

        let clist = []
        for type in filetypes
            call extend(clist, s:registry.getCheckers(type, a:checker_names))
        endfor

        let names = []
        let unavailable_checkers = 0
        for checker in clist
            let cname = checker.getFiletype() . '/' . checker.getName()
            if !checker.isAvailable()
                call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'CacheErrors: Checker ' . cname . ' is not available')
                let unavailable_checkers += 1
                continue
            endif

            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'CacheErrors: Invoking checker: ' . cname)

            let loclist = checker.getLocList()

            if !loclist.isEmpty()
                if decorate_errors
                    call loclist.decorate(cname)
                endif
                call add(names, cname)
                if checker.wantSort() && !sort_aggregated_errors
                    call loclist.sort()
                    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'sorted:', loclist)
                endif

                let newLoclist = newLoclist.extend(loclist)

                if !aggregate_errors
                    break
                endif
            endif
        endfor

        " set names {{{3
        if !empty(names)
            if len(syntastic#util#unique(map( copy(names), 'substitute(v:val, "\\m/.*", "", "")' ))) == 1
                let type = substitute(names[0], '\m/.*', '', '')
                let name = join(map( names, 'substitute(v:val, "\\m.\\{-}/", "", "")' ), ', ')
                call newLoclist.setName( name . ' ('. type . ')' )
            else
                " checkers from mixed types
                call newLoclist.setName(join(names, ', '))
            endif
        endif
        " }}}3

        " issue warning about no active checkers {{{3
        if len(clist) == unavailable_checkers
            if !empty(a:checker_names)
                if len(a:checker_names) == 1
                    call syntastic#log#warn('checker ' . a:checker_names[0] . ' is not available')
                else
                    call syntastic#log#warn('checkers ' . join(a:checker_names, ', ') . ' are not available')
                endif
            else
                call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'CacheErrors: no checkers available for ' . &filetype)
            endif
        endif
        " }}}3

        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'aggregated:', newLoclist)
        if sort_aggregated_errors
            call newLoclist.sort()
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'sorted:', newLoclist)
        endif
    endif

    call newLoclist.deploy()
endfunction " }}}2

"Emulates the :lmake command. Sets up the make environment according to the
"options given, runs make, resets the environment, returns the location list
"
"a:options can contain the following keys:
"    'makeprg'
"    'errorformat'
"
"The corresponding options are set for the duration of the function call. They
"are set with :let, so dont escape spaces.
"
"a:options may also contain:
"   'defaults' - a dict containing default values for the returned errors
"   'subtype' - all errors will be assigned the given subtype
"   'preprocess' - a function to be applied to the error file before parsing errors
"   'postprocess' - a list of functions to be applied to the error list
"   'cwd' - change directory to the given path before running the checker
"   'env' - environment variables to set before running the checker
"   'returns' - a list of valid exit codes for the checker
" @vimlint(EVL102, 1, l:env_save)
function! SyntasticMake(options) abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'SyntasticMake: called with options:', a:options)

    " save options and locale env variables {{{3
    let old_local_errorformat = &l:errorformat
    let old_errorformat = &errorformat
    let old_cwd = getcwd()
    " }}}3

    if has_key(a:options, 'errorformat')
        let &errorformat = a:options['errorformat']
        set errorformat<
    endif

    if has_key(a:options, 'cwd')
        execute 'lcd ' . fnameescape(a:options['cwd'])
    endif

    " set environment variables {{{3
    let env_save = {}
    if has_key(a:options, 'env') && len(a:options['env'])
        for key in keys(a:options['env'])
            if key =~? '\m^[a-z_][a-z0-9_]*$'
                execute 'let env_save[' . string(key) . '] = $' . key
                execute 'let $' . key . ' = ' . string(a:options['env'][key])
            endif
        endfor
    endif
    " }}}3

    let err_lines = split(syntastic#util#system(a:options['makeprg']), "\n", 1)

    " restore environment variables {{{3
    if len(env_save)
        for key in keys(env_save)
            execute 'let $' . key . ' = ' . string(env_save[key])
        endfor
    endif
    " }}}3

    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'checker output:', err_lines)

    " Does it still make sense to go on?
    let bailout =
        \ syntastic#util#var('exit_checks') &&
        \ has_key(a:options, 'returns') &&
        \ index(a:options['returns'], v:shell_error) == -1

    if !bailout
        if has_key(a:options, 'Preprocess')
            let err_lines = call(a:options['Preprocess'], [err_lines])
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'preprocess (external):', err_lines)
        elseif has_key(a:options, 'preprocess')
            let err_lines = call('syntastic#preprocess#' . a:options['preprocess'], [err_lines])
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'preprocess:', err_lines)
        endif
        lgetexpr err_lines

        let errors = deepcopy(getloclist(0))

        if has_key(a:options, 'cwd')
            execute 'lcd ' . fnameescape(old_cwd)
        endif

        try
            silent lolder
        catch /\m^Vim\%((\a\+)\)\=:E380/
            " E380: At bottom of quickfix stack
            call setloclist(0, [], 'r')
        catch /\m^Vim\%((\a\+)\)\=:E776/
            " E776: No location list
            " do nothing
        endtry
    else
        let errors = []
    endif

    " restore options {{{3
    let &errorformat = old_errorformat
    let &l:errorformat = old_local_errorformat
    " }}}3

    if !s:_running_windows && (s:_os_name() =~? 'FreeBSD' || s:_os_name() =~? 'OpenBSD')
        call syntastic#util#redraw(g:syntastic_full_redraws)
    endif

    if bailout
        call syntastic#log#ndebug(g:_SYNTASTIC_DEBUG_LOCLIST, 'checker output:', err_lines)
        throw 'Syntastic: checker error'
    endif

    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'raw loclist:', errors)

    if has_key(a:options, 'defaults')
        call s:_add_to_errors(errors, a:options['defaults'])
    endif

    " Add subtype info if present.
    if has_key(a:options, 'subtype')
        call s:_add_to_errors(errors, { 'subtype': a:options['subtype'] })
    endif

    if has_key(a:options, 'Postprocess') && !empty(a:options['Postprocess'])
        for rule in a:options['Postprocess']
            let errors = call(rule, [errors])
        endfor
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'postprocess (external):', errors)
    elseif has_key(a:options, 'postprocess') && !empty(a:options['postprocess'])
        for rule in a:options['postprocess']
            let errors = call('syntastic#postprocess#' . rule, [errors])
        endfor
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'postprocess:', errors)
    endif

    return errors
endfunction " }}}2
" @vimlint(EVL102, 0, l:env_save)

"return a string representing the state of buffer according to
"g:syntastic_stl_format
"
"return '' if no errors are cached for the buffer
function! SyntasticStatuslineFlag() abort " {{{2
    return g:SyntasticLoclist.current().getStatuslineFlag()
endfunction " }}}2

" }}}1

" Utilities {{{1

function! s:_resolve_filetypes(filetypes) abort " {{{2
    let type = len(a:filetypes) ? a:filetypes[0] : &filetype
    return split( get(g:syntastic_filetype_map, type, type), '\m\.' )
endfunction " }}}2

function! s:_ignore_file(filename) abort " {{{2
    let fname = fnamemodify(a:filename, ':p')
    for pattern in g:syntastic_ignore_files
        if fname =~# pattern
            return 1
        endif
    endfor
    return 0
endfunction " }}}2

function! s:_is_quitting(buf) abort " {{{2
    let quitting = 0
    if exists('w:syntastic_wid')
        let key = a:buf . '_' . getbufvar(a:buf, 'changetick') . '_' . w:syntastic_wid
        let idx = index(s:_quit_pre, key)
        if idx >= 0
            call remove(s:_quit_pre, idx)
            let quitting = 1
        endif
    endif

    return quitting
endfunction " }}}2

" Skip running in special buffers
function! s:_skip_file() abort " {{{2
    let fname = expand('%', 1)
    let skip = s:_is_quitting(bufnr('%')) || get(b:, 'syntastic_skip_checks', 0) ||
        \ (&buftype !=# '') || !filereadable(fname) || getwinvar(0, '&diff') ||
        \ getwinvar(0, '&previewwindow') || s:_ignore_file(fname) ||
        \ fnamemodify(fname, ':e') =~? g:syntastic_ignore_extensions
    if skip
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, '_skip_file: skipping checks')
    endif
    return skip
endfunction " }}}2

" Explain why checks will be skipped for the current file
function! s:_explain_skip(filetypes) abort " {{{2
    if empty(a:filetypes) && s:_skip_file()
        let why = []
        let fname = expand('%', 1)

        if s:_is_quitting(bufnr('%'))
            call add(why, 'quitting buffer')
        endif
        if get(b:, 'syntastic_skip_checks', 0)
            call add(why, 'b:syntastic_skip_checks set')
        endif
        if &buftype !=# ''
            call add(why, 'buftype = ' . string(&buftype))
        endif
        if !filereadable(fname)
            call add(why, 'file not readable / not local')
        endif
        if getwinvar(0, '&diff')
            call add(why, 'diff mode')
        endif
        if getwinvar(0, '&previewwindow')
            call add(why, 'preview window')
        endif
        if s:_ignore_file(fname)
            call add(why, 'filename matching g:syntastic_ignore_files')
        endif
        if fnamemodify(fname, ':e') =~? g:syntastic_ignore_extensions
            call add(why, 'extension matching g:syntastic_ignore_extensions')
        endif

        echomsg 'The current file will not be checked (' . join(why, ', ') . ')'
    endif
endfunction " }}}2

" Take a list of errors and add default values to them from a:options
function! s:_add_to_errors(errors, options) abort " {{{2
    for err in a:errors
        for key in keys(a:options)
            if !has_key(err, key) || empty(err[key])
                let err[key] = a:options[key]
            endif
        endfor
    endfor

    return a:errors
endfunction " }}}2

function! s:_os_name() abort " {{{2
    return g:_SYNTASTIC_UNAME
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
