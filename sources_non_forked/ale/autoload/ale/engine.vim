" Author: w0rp <devw0rp@gmail.com>
" Description: Backend execution and job management
"   Executes linters in the background, using NeoVim or Vim 8 jobs

" Remapping of linter problems.
let g:ale_type_map = get(g:, 'ale_type_map', {})
let g:ale_filename_mappings = get(g:, 'ale_filename_mappings', {})

if !has_key(s:, 'executable_cache_map')
    let s:executable_cache_map = {}
endif

function! ale#engine#CleanupEveryBuffer() abort
    for l:key in keys(g:ale_buffer_info)
        " The key could be a filename or a buffer number, so try and
        " convert it to a number. We need a number for the other
        " functions.
        let l:buffer = str2nr(l:key)

        if l:buffer > 0
            " Stop all jobs and clear the results for everything, and delete
            " all of the data we stored for the buffer.
            call ale#engine#Cleanup(l:buffer)
        endif
    endfor
endfunction

function! ale#engine#MarkLinterActive(info, linter) abort
    let l:found = 0

    for l:other_linter in a:info.active_linter_list
        if l:other_linter.name is# a:linter.name
            let l:found = 1
            break
        endif
    endfor

    if !l:found
        call add(a:info.active_linter_list, a:linter)
    endif
endfunction

function! ale#engine#MarkLinterInactive(info, linter_name) abort
    call filter(a:info.active_linter_list, 'v:val.name isnot# a:linter_name')
endfunction

function! ale#engine#ResetExecutableCache() abort
    let s:executable_cache_map = {}
endfunction

" Check if files are executable, and if they are, remember that they are
" for subsequent calls. We'll keep checking until programs can be executed.
function! ale#engine#IsExecutable(buffer, executable) abort
    if empty(a:executable)
        " Don't log the executable check if the executable string is empty.
        return 0
    endif

    " Check for a cached executable() check.
    let l:result = get(s:executable_cache_map, a:executable, v:null)

    if l:result isnot v:null
        return l:result
    endif

    " Check if the file is executable, and convert -1 to 1.
    let l:result = executable(a:executable) isnot 0

    " Cache the executable check if we found it, or if the option to cache
    " failing checks is on.
    if l:result || get(g:, 'ale_cache_executable_check_failures', 0)
        let s:executable_cache_map[a:executable] = l:result
    endif

    if g:ale_history_enabled
        call ale#history#Add(a:buffer, l:result, 'executable', a:executable)
    endif

    return l:result
endfunction

function! ale#engine#InitBufferInfo(buffer) abort
    if !has_key(g:ale_buffer_info, a:buffer)
        " active_linter_list will hold the list of active linter names
        " loclist holds the loclist items after all jobs have completed.
        let g:ale_buffer_info[a:buffer] = {
        \   'active_linter_list': [],
        \   'active_other_sources_list': [],
        \   'loclist': [],
        \}

        return 1
    endif

    return 0
endfunction

" This function is documented and part of the public API.
"
" Return 1 if ALE is busy checking a given buffer
function! ale#engine#IsCheckingBuffer(buffer) abort
    let l:info = get(g:ale_buffer_info, a:buffer, {})

    return !empty(get(l:info, 'active_linter_list', []))
    \   || !empty(get(l:info, 'active_other_sources_list', []))
endfunction

function! ale#engine#HandleLoclist(linter_name, buffer, loclist, from_other_source) abort
    let l:info = get(g:ale_buffer_info, a:buffer, {})

    if empty(l:info)
        return
    endif

    if !a:from_other_source
        " Remove this linter from the list of active linters.
        " This may have already been done when the job exits.
        call filter(l:info.active_linter_list, 'v:val.name isnot# a:linter_name')
    endif

    " Make some adjustments to the loclists to fix common problems, and also
    " to set default values for loclist items.
    let l:linter_loclist = ale#engine#FixLocList(
    \   a:buffer,
    \   a:linter_name,
    \   a:from_other_source,
    \   a:loclist,
    \)

    " Remove previous items for this linter.
    call filter(l:info.loclist, 'v:val.linter_name isnot# a:linter_name')

    " We don't need to add items or sort the list when this list is empty.
    if !empty(l:linter_loclist)
        " Add the new items.
        call extend(l:info.loclist, l:linter_loclist)

        " Sort the loclist again.
        " We need a sorted list so we can run a binary search against it
        " for efficient lookup of the messages in the cursor handler.
        call sort(l:info.loclist, 'ale#util#LocItemCompare')
    endif

    if ale#ShouldDoNothing(a:buffer)
        return
    endif

    call ale#engine#SetResults(a:buffer, l:info.loclist)
endfunction

function! s:HandleExit(job_info, buffer, output, data) abort
    let l:buffer_info = get(g:ale_buffer_info, a:buffer)

    if empty(l:buffer_info)
        return
    endif

    let l:linter = a:job_info.linter
    let l:executable = a:job_info.executable

    " Remove this job from the list.
    call ale#engine#MarkLinterInactive(l:buffer_info, l:linter.name)

    " Stop here if we land in the handle for a job completing if we're in
    " a sandbox.
    if ale#util#InSandbox()
        return
    endif

    if has('nvim') && !empty(a:output) && empty(a:output[-1])
        call remove(a:output, -1)
    endif

    try
        let l:loclist = ale#util#GetFunction(l:linter.callback)(a:buffer, a:output)
    " Handle the function being unknown, or being deleted.
    catch /E700/
        let l:loclist = []
    endtry

    call ale#engine#HandleLoclist(l:linter.name, a:buffer, l:loclist, 0)
endfunction

function! ale#engine#SetResults(buffer, loclist) abort
    let l:linting_is_done = !ale#engine#IsCheckingBuffer(a:buffer)

    if g:ale_use_neovim_diagnostics_api
        call ale#engine#SendResultsToNeovimDiagnostics(a:buffer, a:loclist)
    endif

    " Set signs first. This could potentially fix some line numbers.
    " The List could be sorted again here by SetSigns.
    if !g:ale_use_neovim_diagnostics_api && g:ale_set_signs
        call ale#sign#SetSigns(a:buffer, a:loclist)
    endif

    if g:ale_set_quickfix || g:ale_set_loclist
        call ale#list#SetLists(a:buffer, a:loclist)
    endif

    if exists('*ale#statusline#Update')
        " Don't load/run if not already loaded.
        call ale#statusline#Update(a:buffer, a:loclist)
    endif

    if !g:ale_use_neovim_diagnostics_api && g:ale_set_highlights
        call ale#highlight#SetHighlights(a:buffer, a:loclist)
    endif

    if !g:ale_use_neovim_diagnostics_api
    \&& (g:ale_virtualtext_cursor is# 'all' || g:ale_virtualtext_cursor == 2)
        call ale#virtualtext#SetTexts(a:buffer, a:loclist)
    endif

    if l:linting_is_done
        if g:ale_echo_cursor
            " Try and echo the warning now.
            " This will only do something meaningful if we're in normal mode.
            call ale#cursor#EchoCursorWarning()
        endif

        if !g:ale_use_neovim_diagnostics_api
        \&& (g:ale_virtualtext_cursor is# 'current' || g:ale_virtualtext_cursor == 1)
            " Try and show the warning now.
            " This will only do something meaningful if we're in normal mode.
            call ale#virtualtext#ShowCursorWarning()
        endif

        " Reset the save event marker, used for opening windows, etc.
        call setbufvar(a:buffer, 'ale_save_event_fired', 0)
        " Set a marker showing how many times a buffer has been checked.
        call setbufvar(
        \   a:buffer,
        \   'ale_linted',
        \   getbufvar(a:buffer, 'ale_linted', 0) + 1
        \)

        " Automatically remove all managed temporary files and directories
        " now that all jobs have completed.
        call ale#command#RemoveManagedFiles(a:buffer)

        " Call user autocommands. This allows users to hook into ALE's lint cycle.
        silent doautocmd <nomodeline> User ALELintPost
    endif
endfunction

function! ale#engine#SendResultsToNeovimDiagnostics(buffer, loclist) abort
    if !has('nvim-0.6')
        " We will warn the user on startup as well if they try to set
        " g:ale_use_neovim_diagnostics_api outside of a Neovim context.
        return
    endif

    " Keep the Lua surface area really small in the VimL part of ALE,
    " and just require the diagnostics.lua module on demand.
    let l:SendDiagnostics = luaeval('require("diagnostics").sendAleResultsToDiagnostics')
    call l:SendDiagnostics(a:buffer, a:loclist)
endfunction

function! s:RemapItemTypes(type_map, loclist) abort
    for l:item in a:loclist
        let l:key = l:item.type
        \   . (get(l:item, 'sub_type', '') is# 'style' ? 'S' : '')
        let l:new_key = get(a:type_map, l:key, '')

        if l:new_key is# 'E'
        \|| l:new_key is# 'ES'
        \|| l:new_key is# 'W'
        \|| l:new_key is# 'WS'
        \|| l:new_key is# 'I'
            let l:item.type = l:new_key[0]

            if l:new_key is# 'ES' || l:new_key is# 'WS'
                let l:item.sub_type = 'style'
            elseif has_key(l:item, 'sub_type')
                call remove(l:item, 'sub_type')
            endif
        endif
    endfor
endfunction

function! ale#engine#FixLocList(buffer, linter_name, from_other_source, loclist) abort
    let l:mappings = ale#GetFilenameMappings(a:buffer, a:linter_name)

    if !empty(l:mappings)
        " We need to apply reverse filename mapping here.
        let l:mappings = ale#filename_mapping#Invert(l:mappings)
    endif

    let l:bufnr_map = {}
    let l:new_loclist = []

    " Some errors have line numbers beyond the end of the file,
    " so we need to adjust them so they set the error at the last line
    " of the file instead.
    let l:last_line_number = ale#util#GetLineCount(a:buffer)

    for l:old_item in a:loclist
        " Copy the loclist item with some default values and corrections.
        "
        " line and column numbers will be converted to numbers.
        " The buffer will default to the buffer being checked.
        " The vcol setting will default to 0, a byte index.
        " The error type will default to 'E' for errors.
        " The error number will default to -1.
        "
        " The line number and text are the only required keys.
        "
        " The linter_name will be set on the errors so it can be used in
        " output, filtering, etc..
        let l:item = {
        \   'bufnr': a:buffer,
        \   'text': l:old_item.text,
        \   'lnum': str2nr(l:old_item.lnum),
        \   'col': str2nr(get(l:old_item, 'col', 0)),
        \   'vcol': 0,
        \   'type': get(l:old_item, 'type', 'E'),
        \   'nr': get(l:old_item, 'nr', -1),
        \   'linter_name': a:linter_name,
        \}

        if a:from_other_source
            let l:item.from_other_source = 1
        endif

        if has_key(l:old_item, 'code')
            let l:item.code = l:old_item.code
        endif

        let l:old_name = get(l:old_item, 'filename', '')

        " Map parsed from output to local filesystem files.
        if !empty(l:old_name) && !empty(l:mappings)
            let l:old_name = ale#filename_mapping#Map(l:old_name, l:mappings)
        endif

        if !empty(l:old_name) && !ale#path#IsTempName(l:old_name)
            " Use the filename given.
            " Temporary files are assumed to be for this buffer,
            " and the filename is not included then, because it looks bad
            " in the loclist window.
            let l:filename = l:old_name
            let l:item.filename = l:filename

            if has_key(l:old_item, 'bufnr')
                " If a buffer number is also given, include that too.
                " If Vim detects that he buffer number is valid, it will
                " be used instead of the filename.
                let l:item.bufnr = l:old_item.bufnr
            elseif has_key(l:bufnr_map, l:filename)
                " Get the buffer number from the map, which can be faster.
                let l:item.bufnr = l:bufnr_map[l:filename]
            else
                " Look up the buffer number.
                let l:item.bufnr = bufnr(l:filename)
                let l:bufnr_map[l:filename] = l:item.bufnr
            endif
        elseif has_key(l:old_item, 'bufnr')
            let l:item.bufnr = l:old_item.bufnr
        endif

        if has_key(l:old_item, 'detail')
            let l:item.detail = l:old_item.detail
        endif

        " Pass on a end_col key if set, used for highlights.
        if has_key(l:old_item, 'end_col')
            let l:item.end_col = str2nr(l:old_item.end_col)
        endif

        if has_key(l:old_item, 'end_lnum')
            let l:item.end_lnum = str2nr(l:old_item.end_lnum)

            " When the error ends after the end of the file, put it at the
            " end. This is only done for the current buffer.
            if l:item.bufnr == a:buffer && l:item.end_lnum > l:last_line_number
                let l:item.end_lnum = l:last_line_number
            endif
        endif

        if has_key(l:old_item, 'sub_type')
            let l:item.sub_type = l:old_item.sub_type
        endif

        if l:item.lnum < 1
            " When errors appear before line 1, put them at line 1.
            let l:item.lnum = 1
        elseif l:item.bufnr == a:buffer && l:item.lnum > l:last_line_number
            " When errors go beyond the end of the file, put them at the end.
            " This is only done for the current buffer.
            let l:item.lnum = l:last_line_number
        elseif get(l:old_item, 'vcol', 0)
            " Convert virtual column positions to byte positions.
            " The positions will be off if the buffer has changed recently.
            let l:line = getbufline(a:buffer, l:item.lnum)[0]

            let l:item.col = ale#util#Col(l:line, l:item.col)

            if has_key(l:item, 'end_col')
                let l:end_line = get(l:item, 'end_lnum', l:line) != l:line
                \   ? getbufline(a:buffer, l:item.end_lnum)[0]
                \   : l:line

                let l:item.end_col = ale#util#Col(l:end_line, l:item.end_col)
            endif
        endif

        call add(l:new_loclist, l:item)
    endfor

    let l:type_map = get(ale#Var(a:buffer, 'type_map'), a:linter_name, {})

    if !empty(l:type_map)
        call s:RemapItemTypes(l:type_map, l:new_loclist)
    endif

    return l:new_loclist
endfunction

" Given part of a command, replace any % with %%, so that no characters in
" the string will be replaced with filenames, etc.
function! ale#engine#EscapeCommandPart(command_part) abort
    " TODO: Emit deprecation warning here later.
    return ale#command#EscapeCommandPart(a:command_part)
endfunction

" Run a job.
"
" Returns 1 when a job was started successfully.
function! s:RunJob(command, options) abort
    if ale#command#IsDeferred(a:command)
        let a:command.result_callback = {
        \   command -> s:RunJob(command, a:options)
        \}

        return 1
    endif

    let l:command = a:command

    if empty(l:command)
        return 0
    endif

    let l:cwd = a:options.cwd
    let l:executable = a:options.executable
    let l:buffer = a:options.buffer
    let l:linter = a:options.linter
    let l:output_stream = a:options.output_stream
    let l:read_buffer = a:options.read_buffer && !a:options.lint_file
    let l:info = g:ale_buffer_info[l:buffer]

    let l:Callback = function('s:HandleExit', [{
    \   'linter': l:linter,
    \   'executable': l:executable,
    \}])
    let l:result = ale#command#Run(l:buffer, l:command, l:Callback, {
    \   'cwd': l:cwd,
    \   'output_stream': l:output_stream,
    \   'executable': l:executable,
    \   'read_buffer': l:read_buffer,
    \   'log_output': 1,
    \   'filename_mappings': ale#GetFilenameMappings(l:buffer, l:linter.name),
    \})

    " Only proceed if the job is being run.
    if empty(l:result)
        return 0
    endif

    call ale#engine#MarkLinterActive(l:info, l:linter)

    silent doautocmd <nomodeline> User ALEJobStarted

    return 1
endfunction

function! s:StopCurrentJobs(buffer, clear_lint_file_jobs, linter_slots) abort
    let l:info = get(g:ale_buffer_info, a:buffer, {})
    call ale#command#StopJobs(a:buffer, 'linter')

    " Update the active linter list, clearing out anything not running.
    if a:clear_lint_file_jobs
        call ale#command#StopJobs(a:buffer, 'file_linter')
        let l:info.active_linter_list = []
    else
        let l:lint_file_map = {}

        " Use a previously computed map of `lint_file` values to find
        " linters that are used for linting files.
        for [l:lint_file, l:linter] in a:linter_slots
            if l:lint_file is 1
                let l:lint_file_map[l:linter.name] = 1
            endif
        endfor

        " Keep jobs for linting files when we're only linting buffers.
        call filter(l:info.active_linter_list, 'get(l:lint_file_map, v:val.name)')
    endif
endfunction

function! ale#engine#Stop(buffer) abort
    call s:StopCurrentJobs(a:buffer, 1, [])
endfunction

function! s:RemoveProblemsForDisabledLinters(buffer, linters) abort
    " Figure out which linters are still enabled, and remove
    " problems for linters which are no longer enabled.
    " Problems from other sources will be kept.
    let l:name_map = {}

    for l:linter in a:linters
        let l:name_map[l:linter.name] = 1
    endfor

    call filter(
    \   get(g:ale_buffer_info[a:buffer], 'loclist', []),
    \   'get(v:val, ''from_other_source'') || get(l:name_map, get(v:val, ''linter_name''))',
    \)
endfunction

function! s:AddProblemsFromOtherBuffers(buffer, linters) abort
    let l:filename = expand('#' . a:buffer . ':p')
    let l:loclist = []
    let l:name_map = {}

    " Build a map of the active linters.
    for l:linter in a:linters
        let l:name_map[l:linter.name] = 1
    endfor

    " Find the items from other buffers, for the linters that are enabled.
    for l:info in values(g:ale_buffer_info)
        for l:item in l:info.loclist
            if has_key(l:item, 'filename')
            \&& l:item.filename is# l:filename
            \&& has_key(l:name_map, l:item.linter_name)
                " Copy the items and set the buffer numbers to this one.
                let l:new_item = copy(l:item)
                let l:new_item.bufnr = a:buffer
                call add(l:loclist, l:new_item)
            endif
        endfor
    endfor

    if !empty(l:loclist)
        call sort(l:loclist, function('ale#util#LocItemCompareWithText'))
        call uniq(l:loclist, function('ale#util#LocItemCompareWithText'))

        " Set the loclist variable, used by some parts of ALE.
        let g:ale_buffer_info[a:buffer].loclist = l:loclist
        call ale#engine#SetResults(a:buffer, l:loclist)
    endif
endfunction

function! s:RunIfExecutable(buffer, linter, lint_file, executable) abort
    if ale#command#IsDeferred(a:executable)
        let a:executable.result_callback = {
        \   executable -> s:RunIfExecutable(
        \       a:buffer,
        \       a:linter,
        \       a:lint_file,
        \       executable
        \   )
        \}

        return 1
    endif

    if ale#engine#IsExecutable(a:buffer, a:executable)
        " Use different job types for file or linter jobs.
        let l:job_type = a:lint_file ? 'file_linter' : 'linter'
        call setbufvar(a:buffer, 'ale_job_type', l:job_type)

        " Get the cwd for the linter and set it before we call GetCommand.
        " This will ensure that ale#command#Run uses it by default.
        let l:cwd = ale#linter#GetCwd(a:buffer, a:linter)

        if l:cwd isnot v:null
            call ale#command#SetCwd(a:buffer, l:cwd)
        endif

        let l:command = ale#linter#GetCommand(a:buffer, a:linter)

        if l:cwd isnot v:null
            call ale#command#ResetCwd(a:buffer)
        endif

        let l:options = {
        \   'cwd': l:cwd,
        \   'executable': a:executable,
        \   'buffer': a:buffer,
        \   'linter': a:linter,
        \   'output_stream': get(a:linter, 'output_stream', 'stdout'),
        \   'read_buffer': a:linter.read_buffer,
        \   'lint_file': a:lint_file,
        \}

        return s:RunJob(l:command, l:options)
    endif

    return 0
endfunction

" Run a linter for a buffer.
"
" Returns 1 if the linter was successfully run.
function! s:RunLinter(buffer, linter, lint_file) abort
    if !empty(a:linter.lsp)
        return ale#lsp_linter#CheckWithLSP(a:buffer, a:linter)
    else
        let l:executable = ale#linter#GetExecutable(a:buffer, a:linter)

        return s:RunIfExecutable(a:buffer, a:linter, a:lint_file, l:executable)
    endif

    return 0
endfunction

function! s:GetLintFileSlots(buffer, linters) abort
    let l:linter_slots = []

    for l:linter in a:linters
        let l:LintFile = l:linter.lint_file

        if type(l:LintFile) is v:t_func
            let l:LintFile = l:LintFile(a:buffer)
        endif

        call add(l:linter_slots, [l:LintFile, l:linter])
    endfor

    return l:linter_slots
endfunction

function! s:GetLintFileValues(slots, Callback) abort
    let l:deferred_list = []
    let l:new_slots = []

    for [l:lint_file, l:linter] in a:slots
        while ale#command#IsDeferred(l:lint_file) && has_key(l:lint_file, 'value')
            " If we've already computed the return value, use it.
            let l:lint_file = l:lint_file.value
        endwhile

        if ale#command#IsDeferred(l:lint_file)
            " If we are going to return the result later, wait for it.
            call add(l:deferred_list, l:lint_file)
        else
            " If we have the value now, coerce it to 0 or 1.
            let l:lint_file = l:lint_file is 1
        endif

        call add(l:new_slots, [l:lint_file, l:linter])
    endfor

    if !empty(l:deferred_list)
        for l:deferred in l:deferred_list
            let l:deferred.result_callback =
            \   {-> s:GetLintFileValues(l:new_slots, a:Callback)}
        endfor
    else
        call a:Callback(l:new_slots)
    endif
endfunction

function! s:RunLinters(
\   buffer,
\   linters,
\   slots,
\   should_lint_file,
\   new_buffer,
\) abort
    call s:StopCurrentJobs(a:buffer, a:should_lint_file, a:slots)
    call s:RemoveProblemsForDisabledLinters(a:buffer, a:linters)

    " We can only clear the results if we aren't checking the buffer.
    let l:can_clear_results = !ale#engine#IsCheckingBuffer(a:buffer)

    silent doautocmd <nomodeline> User ALELintPre

    for [l:lint_file, l:linter] in a:slots
        " Only run lint_file linters if we should.
        if !l:lint_file || a:should_lint_file
            if s:RunLinter(a:buffer, l:linter, l:lint_file)
                " If a single linter ran, we shouldn't clear everything.
                let l:can_clear_results = 0
            endif
        else
            " If we skipped running a lint_file linter still in the list,
            " we shouldn't clear everything.
            let l:can_clear_results = 0
        endif
    endfor

    " Clear the results if we can. This needs to be done when linters are
    " disabled, or ALE itself is disabled.
    if l:can_clear_results
        call ale#engine#SetResults(a:buffer, [])
    elseif a:new_buffer
        call s:AddProblemsFromOtherBuffers(
        \   a:buffer,
        \   map(copy(a:slots), 'v:val[1]')
        \)
    endif
endfunction

function! ale#engine#RunLinters(buffer, linters, should_lint_file) abort
    " Initialise the buffer information if needed.
    let l:new_buffer = ale#engine#InitBufferInfo(a:buffer)

    call s:GetLintFileValues(
    \   s:GetLintFileSlots(a:buffer, a:linters),
    \   {
    \       slots -> s:RunLinters(
    \           a:buffer,
    \           a:linters,
    \           slots,
    \           a:should_lint_file,
    \           l:new_buffer,
    \       )
    \   }
    \)
endfunction

" Clean up a buffer.
"
" This function will stop all current jobs for the buffer,
" clear the state of everything, and remove the Dictionary for managing
" the buffer.
function! ale#engine#Cleanup(buffer) abort
    " Don't bother with cleanup code when newer NeoVim versions are exiting.
    if get(v:, 'exiting', v:null) isnot v:null
        return
    endif

    if exists('*ale#lsp#CloseDocument')
        call ale#lsp#CloseDocument(a:buffer)
    endif

    if !has_key(g:ale_buffer_info, a:buffer)
        return
    endif

    call ale#engine#RunLinters(a:buffer, [], 1)

    call remove(g:ale_buffer_info, a:buffer)
endfunction

" Given a buffer number, return the warnings and errors for a given buffer.
function! ale#engine#GetLoclist(buffer) abort
    if !has_key(g:ale_buffer_info, a:buffer)
        return []
    endif

    return g:ale_buffer_info[a:buffer].loclist
endfunction
