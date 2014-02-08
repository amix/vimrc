"============================================================================
"File:        syntastic.vim
"Description: Vim plugin for on the fly syntax checking.
"Version:     3.4.0-pre
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_plugin = 1

if has('reltime')
    let g:syntastic_start = reltime()
endif

runtime! plugin/syntastic/*.vim

let s:running_windows = syntastic#util#isRunningWindows()

for s:feature in ['autocmd', 'eval', 'modify_fname', 'quickfix', 'user_commands']
    if !has(s:feature)
        call syntastic#log#error("need Vim compiled with feature " . s:feature)
        finish
    endif
endfor

if !s:running_windows && executable('uname')
    try
        let s:uname = system('uname')
    catch /^Vim\%((\a\+)\)\=:E484/
        call syntastic#log#error("your shell " . &shell . " doesn't use traditional UNIX syntax for redirections")
        finish
    endtry
endif

if !exists("g:syntastic_always_populate_loc_list")
    let g:syntastic_always_populate_loc_list = 0
endif

if !exists("g:syntastic_auto_jump")
    let g:syntastic_auto_jump = 0
endif

if !exists("g:syntastic_quiet_messages")
    let g:syntastic_quiet_messages = {}
endif

if !exists("g:syntastic_stl_format")
    let g:syntastic_stl_format = '[Syntax: line:%F (%t)]'
endif

if !exists("g:syntastic_check_on_open")
    let g:syntastic_check_on_open = 0
endif

if !exists("g:syntastic_check_on_wq")
    let g:syntastic_check_on_wq = 1
endif

if !exists("g:syntastic_aggregate_errors")
    let g:syntastic_aggregate_errors = 0
endif

if !exists("g:syntastic_id_checkers")
    let g:syntastic_id_checkers = 1
endif

if !exists("g:syntastic_loc_list_height")
    let g:syntastic_loc_list_height = 10
endif

if !exists("g:syntastic_ignore_files")
    let g:syntastic_ignore_files = []
endif

if !exists("g:syntastic_filetype_map")
    let g:syntastic_filetype_map = {}
endif

if !exists("g:syntastic_full_redraws")
    let g:syntastic_full_redraws = !(has('gui_running') || has('gui_macvim'))
endif

" TODO: not documented
if !exists("g:syntastic_reuse_loc_lists")
    " a relevant bug has been fixed in one of the pre-releases of Vim 7.4
    let g:syntastic_reuse_loc_lists = (v:version >= 704)
endif

if exists("g:syntastic_quiet_warnings")
    call syntastic#log#deprecationWarn("variable g:syntastic_quiet_warnings is deprecated, please use let g:syntastic_quiet_messages = {'level': 'warnings'} instead")
    if g:syntastic_quiet_warnings
        let s:quiet_warnings = get(g:syntastic_quiet_messages, 'type', [])
        if type(s:quiet_warnings) != type([])
            let s:quiet_warnings = [s:quiet_warnings]
        endif
        call add(s:quiet_warnings, 'warnings')
        let g:syntastic_quiet_messages['type'] = s:quiet_warnings
    endif
endif


" debug constants
let g:SyntasticDebugTrace         = 1
let g:SyntasticDebugLoclist       = 2
let g:SyntasticDebugNotifications = 4
let g:SyntasticDebugAutocommands  = 8
let g:SyntasticDebugVariables     = 16

let s:registry = g:SyntasticRegistry.Instance()
let s:notifiers = g:SyntasticNotifiers.Instance()
let s:modemap = g:SyntasticModeMap.Instance()


" @vimlint(EVL103, 1, a:cursorPos)
" @vimlint(EVL103, 1, a:cmdLine)
" @vimlint(EVL103, 1, a:argLead)
function! s:CompleteCheckerName(argLead, cmdLine, cursorPos)
    let checker_names = []
    for ft in s:ResolveFiletypes()
        for checker in s:registry.availableCheckersFor(ft)
            call add(checker_names, checker.getName())
        endfor
    endfor
    return join(checker_names, "\n")
endfunction
" @vimlint(EVL103, 0, a:cursorPos)
" @vimlint(EVL103, 0, a:cmdLine)
" @vimlint(EVL103, 0, a:argLead)


" @vimlint(EVL103, 1, a:cursorPos)
" @vimlint(EVL103, 1, a:cmdLine)
" @vimlint(EVL103, 1, a:argLead)
function! s:CompleteFiletypes(argLead, cmdLine, cursorPos)
    return join(s:registry.knownFiletypes(), "\n")
endfunction
" @vimlint(EVL103, 0, a:cursorPos)
" @vimlint(EVL103, 0, a:cmdLine)
" @vimlint(EVL103, 0, a:argLead)


command! SyntasticToggleMode call s:ToggleMode()
command! -nargs=* -complete=custom,s:CompleteCheckerName SyntasticCheck
            \ call s:UpdateErrors(0, <f-args>) <bar>
            \ call syntastic#util#redraw(g:syntastic_full_redraws)
command! Errors call s:ShowLocList()
command! -nargs=? -complete=custom,s:CompleteFiletypes SyntasticInfo
            \ call s:modemap.echoMode() |
            \ call s:registry.echoInfoFor(s:ResolveFiletypes(<f-args>))
command! SyntasticReset
            \ call s:ClearCache() |
            \ call s:notifiers.refresh(g:SyntasticLoclist.New([]))
command! SyntasticSetLoclist call g:SyntasticLoclist.current().setloclist()

augroup syntastic
    autocmd BufReadPost * call s:BufReadPostHook()
    autocmd BufWritePost * call s:BufWritePostHook()

    autocmd BufWinEnter * call s:BufWinEnterHook()

    " TODO: the next autocmd should be "autocmd BufWinLeave * if &buftype == '' | lclose | endif"
    " but in recent versions of Vim lclose can no longer be called from BufWinLeave
    autocmd BufEnter * call s:BufEnterHook()
augroup END

if v:version > 703 || (v:version == 703 && has('patch544'))
    " QuitPre was added in Vim 7.3.544
    augroup syntastic
        autocmd QuitPre * call s:QuitPreHook()
    augroup END
endif


function! s:BufReadPostHook()
    if g:syntastic_check_on_open
        call syntastic#log#debug(g:SyntasticDebugAutocommands,
            \ 'autocmd: BufReadPost, buffer ' . bufnr("") . ' = ' . string(bufname(str2nr(bufnr("")))))
        call s:UpdateErrors(1)
    endif
endfunction

function! s:BufWritePostHook()
    call syntastic#log#debug(g:SyntasticDebugAutocommands,
        \ 'autocmd: BufWritePost, buffer ' . bufnr("") . ' = ' . string(bufname(str2nr(bufnr("")))))
    call s:UpdateErrors(1)
endfunction

function! s:BufWinEnterHook()
    call syntastic#log#debug(g:SyntasticDebugAutocommands,
        \ 'autocmd: BufWinEnter, buffer ' . bufnr("") . ' = ' . string(bufname(str2nr(bufnr("")))) .
        \ ', &buftype = ' . string(&buftype))
    if &buftype == ''
        call s:notifiers.refresh(g:SyntasticLoclist.current())
    endif
endfunction

function! s:BufEnterHook()
    call syntastic#log#debug(g:SyntasticDebugAutocommands,
        \ 'autocmd: BufEnter, buffer ' . bufnr("") . ' = ' . string(bufname(str2nr(bufnr("")))) .
        \ ', &buftype = ' . string(&buftype))
    " TODO: at this point there is no b:syntastic_loclist
    let loclist = filter(getloclist(0), 'v:val["valid"] == 1')
    let buffers = syntastic#util#unique(map( loclist, 'v:val["bufnr"]' ))
    if &buftype == 'quickfix' && !empty(loclist) && empty(filter( buffers, 'syntastic#util#bufIsActive(v:val)' ))
        call g:SyntasticLoclistHide()
    endif
endfunction

function! s:QuitPreHook()
    call syntastic#log#debug(g:SyntasticDebugAutocommands,
        \ 'autocmd: QuitPre, buffer ' . bufnr("") . ' = ' . string(bufname(str2nr(bufnr("")))))
    let b:syntastic_skip_checks = !g:syntastic_check_on_wq
    call g:SyntasticLoclistHide()
endfunction

"refresh and redraw all the error info for this buf when saving or reading
function! s:UpdateErrors(auto_invoked, ...)
    if s:SkipFile()
        return
    endif

    call s:modemap.synch()
    let run_checks = !a:auto_invoked || s:modemap.allowsAutoChecking(&filetype)
    if run_checks
        call s:CacheErrors(a:000)
    endif

    let loclist = g:SyntasticLoclist.current()

    let w:syntastic_loclist_set = 0
    let do_jump = g:syntastic_auto_jump
    if g:syntastic_auto_jump == 2
        let first = loclist.getFirstIssue()
        let type = get(first, 'type', '')
        let do_jump = type ==? 'E'
    endif

    if g:syntastic_always_populate_loc_list || do_jump
        call syntastic#log#debug(g:SyntasticDebugNotifications, 'loclist: setloclist (new)')
        call setloclist(0, loclist.getRaw())
        let w:syntastic_loclist_set = 1
        if run_checks && do_jump && !loclist.isEmpty()
            call syntastic#log#debug(g:SyntasticDebugNotifications, 'loclist: jump')
            silent! lrewind

            " XXX: Vim doesn't call autocmd commands in a predictible
            " order, which can lead to missing filetype when jumping
            " to a new file; the following is a workaround for the
            " resulting brain damage
            if &filetype == ''
                silent! filetype detect
            endif
        endif
    endif

    call s:notifiers.refresh(loclist)
endfunction

"clear the loc list for the buffer
function! s:ClearCache()
    call s:notifiers.reset(g:SyntasticLoclist.current())
    unlet! b:syntastic_loclist
endfunction

function! s:ResolveFiletypes(...)
    let type = a:0 ? a:1 : &filetype
    return split( get(g:syntastic_filetype_map, type, type), '\m\.' )
endfunction

"detect and cache all syntax errors in this buffer
function! s:CacheErrors(checkers)
    call s:ClearCache()
    let newLoclist = g:SyntasticLoclist.New([])

    if !s:SkipFile()
        let active_checkers = 0
        let names = []

        call syntastic#log#debugShowOptions(g:SyntasticDebugTrace, [
            \ 'shell', 'shellcmdflag', 'shellquote', 'shellxquote', 'shellredir',
            \ 'shellslash', 'shellpipe', 'shelltemp', 'shellxescape', 'shellxquote' ])
        call syntastic#log#debugDump(g:SyntasticDebugVariables)
        call syntastic#log#debugShowVariables(g:SyntasticDebugTrace, 'syntastic_aggregate_errors')

        let filetypes = s:ResolveFiletypes()
        let aggregate_errors =
            \ exists('b:syntastic_aggregate_errors') ? b:syntastic_aggregate_errors : g:syntastic_aggregate_errors
        let decorate_errors = (aggregate_errors || len(filetypes) > 1) &&
            \ (exists('b:syntastic_id_checkers') ? b:syntastic_id_checkers : g:syntastic_id_checkers)

        for ft in filetypes
            let clist = empty(a:checkers) ? s:registry.getActiveCheckers(ft) : s:registry.getCheckers(ft, a:checkers)

            for checker in clist
                let active_checkers += 1
                call syntastic#log#debug(g:SyntasticDebugTrace, "CacheErrors: Invoking checker: " . checker.getName())

                let loclist = checker.getLocList()

                if !loclist.isEmpty()
                    if decorate_errors
                        call loclist.decorate(checker.getName(), checker.getFiletype())
                    endif
                    call add(names, [checker.getName(), checker.getFiletype()])

                    let newLoclist = newLoclist.extend(loclist)

                    if !aggregate_errors
                        break
                    endif
                endif
            endfor
        endfor

        if !empty(names)
            if len(syntastic#util#unique(map(copy(names), 'v:val[1]'))) == 1
                let type = names[0][1]
                let name = join(map(names, 'v:val[0]'), ', ')
                call newLoclist.setName( name . ' ('. type . ')' )
            else
                " checkers from mixed types
                call newLoclist.setName(join(map(names, 'v:val[1] . "/" . v:val[0]'), ', '))
            endif
        endif

        if !active_checkers
            if !empty(a:checkers)
                if len(a:checkers) == 1
                    call syntastic#log#warn('checker ' . a:checkers[0] . ' is not active for filetype ' . &filetype)
                else
                    call syntastic#log#warn('checkers ' . join(a:checkers, ', ') . ' are not active for filetype ' . &filetype)
                endif
            else
                call syntastic#log#debug(g:SyntasticDebugTrace, 'CacheErrors: no active checkers for filetype ' . &filetype)
            endif
        endif

        call syntastic#log#debug(g:SyntasticDebugLoclist, "aggregated:", newLoclist)

        if type(g:syntastic_quiet_messages) == type({}) && !empty(g:syntastic_quiet_messages)
            call newLoclist.quietMessages(g:syntastic_quiet_messages)
            call syntastic#log#debug(g:SyntasticDebugLoclist, "filtered by g:syntastic_quiet_messages:", newLoclist)
        endif
    endif

    let b:syntastic_loclist = newLoclist
endfunction

function! s:ToggleMode()
    call s:modemap.toggleMode()
    call s:ClearCache()
    call s:UpdateErrors(1)
    call s:modemap.echoMode()
endfunction

"display the cached errors for this buf in the location list
function! s:ShowLocList()
    call g:SyntasticLoclist.current().show()
endfunction

"the script changes &shellredir and &shell to stop the screen flicking when
"shelling out to syntax checkers. Not all OSs support the hacks though
function! s:OSSupportsShellredirHack()
    return !s:running_windows && executable('/bin/bash') && (s:uname() !~ "FreeBSD") && (s:uname() !~ "OpenBSD")
endfunction

function! s:IsRedrawRequiredAfterMake()
    return !s:running_windows && (s:uname() =~ "FreeBSD" || s:uname() =~ "OpenBSD")
endfunction

function! s:IgnoreFile(filename)
    let fname = fnamemodify(a:filename, ':p')
    for pattern in g:syntastic_ignore_files
        if fname =~# pattern
            return 1
        endif
    endfor
    return 0
endfunction

" Skip running in special buffers
function! s:SkipFile()
    let force_skip = exists('b:syntastic_skip_checks') ? b:syntastic_skip_checks : 0
    let fname = expand('%')
    return force_skip || (&buftype != '') || !filereadable(fname) || getwinvar(0, '&diff') || s:IgnoreFile(fname)
endfunction

function! s:uname()
    if !exists('s:uname')
        let s:uname = system('uname')
    endif
    return s:uname
endfunction

"return a string representing the state of buffer according to
"g:syntastic_stl_format
"
"return '' if no errors are cached for the buffer
function! SyntasticStatuslineFlag()
    return g:SyntasticLoclist.current().getStatuslineFlag()
endfunction

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
"   'returns' - a list of valid exit codes for the checker
function! SyntasticMake(options)
    call syntastic#log#debug(g:SyntasticDebugTrace, 'SyntasticMake: called with options:', a:options)

    let old_shell = &shell
    let old_shellredir = &shellredir
    let old_local_errorformat = &l:errorformat
    let old_errorformat = &errorformat
    let old_cwd = getcwd()
    let old_lc_messages = $LC_MESSAGES
    let old_lc_all = $LC_ALL

    if s:OSSupportsShellredirHack()
        "this is a hack to stop the screen needing to be ':redraw'n when
        "when :lmake is run. Otherwise the screen flickers annoyingly
        let &shellredir = '&>'
        let &shell = '/bin/bash'
    endif

    if has_key(a:options, 'errorformat')
        let &errorformat = a:options['errorformat']
    endif

    if has_key(a:options, 'cwd')
        execute 'lcd ' . fnameescape(a:options['cwd'])
    endif

    let $LC_MESSAGES = 'C'
    let $LC_ALL = ''
    let err_lines = split(system(a:options['makeprg']), "\n", 1)
    let $LC_ALL = old_lc_all
    let $LC_MESSAGES = old_lc_messages

    call syntastic#log#debug(g:SyntasticDebugLoclist, "checker output:", err_lines)

    if has_key(a:options, 'preprocess')
        let err_lines = call(a:options['preprocess'], [err_lines])
        call syntastic#log#debug(g:SyntasticDebugLoclist, "preprocess:", err_lines)
    endif
    lgetexpr err_lines

    let errors = copy(getloclist(0))

    if has_key(a:options, 'cwd')
        execute 'lcd ' . fnameescape(old_cwd)
    endif

    silent! lolder
    let &errorformat = old_errorformat
    let &l:errorformat = old_local_errorformat
    let &shellredir = old_shellredir
    let &shell = old_shell

    if s:IsRedrawRequiredAfterMake()
        call syntastic#util#redraw(g:syntastic_full_redraws)
    endif

    call syntastic#log#debug(g:SyntasticDebugLoclist, "raw loclist:", errors)

    if has_key(a:options, 'returns') && index(a:options['returns'], v:shell_error) == -1
        throw 'Syntastic: checker error'
    endif

    if has_key(a:options, 'defaults')
        call SyntasticAddToErrors(errors, a:options['defaults'])
    endif

    " Add subtype info if present.
    if has_key(a:options, 'subtype')
        call SyntasticAddToErrors(errors, { 'subtype': a:options['subtype'] })
    endif

    if has_key(a:options, 'postprocess') && !empty(a:options['postprocess'])
        for rule in a:options['postprocess']
            let errors = call('syntastic#postprocess#' . rule, [errors])
        endfor
        call syntastic#log#debug(g:SyntasticDebugLoclist, "postprocess:", errors)
    endif

    return errors
endfunction

"take a list of errors and add default values to them from a:options
function! SyntasticAddToErrors(errors, options)
    for err in a:errors
        for key in keys(a:options)
            if !has_key(err, key) || empty(err[key])
                let err[key] = a:options[key]
            endif
        endfor
    endfor

    return a:errors
endfunction

" vim: set et sts=4 sw=4:
