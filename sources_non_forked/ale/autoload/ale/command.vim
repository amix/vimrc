" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for formatting command strings, running commands, and
"   managing files during linting and fixing cycles.

" This dictionary holds lists of files and directories to remove later.
if !exists('s:buffer_data')
    let s:buffer_data = {}
endif

" The regular expression used for formatting filenames with modifiers.
let s:path_format_regex = '\v\%s(%(:h|:t|:r|:e)*)'

" Used to get the data in tests.
function! ale#command#GetData() abort
    return deepcopy(s:buffer_data)
endfunction

function! ale#command#ClearData() abort
    let s:buffer_data = {}
endfunction

function! ale#command#InitData(buffer) abort
    if !has_key(s:buffer_data, a:buffer)
        let s:buffer_data[a:buffer] = {
        \   'jobs': {},
        \   'file_list': [],
        \   'directory_list': [],
        \}
    endif
endfunction

" Set the cwd for commands that are about to run.
" Used internally.
function! ale#command#SetCwd(buffer, cwd) abort
    call ale#command#InitData(a:buffer)
    let s:buffer_data[a:buffer].cwd = a:cwd
endfunction

function! ale#command#ResetCwd(buffer) abort
    if has_key(s:buffer_data, a:buffer)
        let s:buffer_data[a:buffer].cwd = v:null
    endif
endfunction

function! ale#command#ManageFile(buffer, file) abort
    call ale#command#InitData(a:buffer)
    call add(s:buffer_data[a:buffer].file_list, a:file)
endfunction

function! ale#command#ManageDirectory(buffer, directory) abort
    call ale#command#InitData(a:buffer)
    call add(s:buffer_data[a:buffer].directory_list, a:directory)
endfunction

function! ale#command#CreateFile(buffer) abort
    " This variable can be set to 1 in tests to stub this out.
    if get(g:, 'ale_create_dummy_temporary_file')
        return 'TEMP'
    endif

    let l:temporary_file = ale#util#Tempname()
    call ale#command#ManageFile(a:buffer, l:temporary_file)

    return l:temporary_file
endfunction

" Create a new temporary directory and manage it in one go.
function! ale#command#CreateDirectory(buffer) abort
    " This variable can be set to 1 in tests to stub this out.
    if get(g:, 'ale_create_dummy_temporary_file')
        return 'TEMP_DIR'
    endif

    let l:temporary_directory = ale#util#Tempname()
    " Create the temporary directory for the file, unreadable by 'other'
    " users.
    call mkdir(l:temporary_directory, '', 0750)
    call ale#command#ManageDirectory(a:buffer, l:temporary_directory)

    return l:temporary_directory
endfunction

function! ale#command#RemoveManagedFiles(buffer) abort
    let l:info = get(s:buffer_data, a:buffer, {})

    if !empty(l:info) && empty(l:info.jobs)
        " We can't delete anything in a sandbox, so wait until we escape from
        " it to delete temporary files and directories.
        if ale#util#InSandbox()
            return
        endif

        " Delete files with a call akin to a plan `rm` command.
        for l:filename in l:info.file_list
            call delete(l:filename)
        endfor

        " Delete directories like `rm -rf`.
        " Directories are handled differently from files, so paths that are
        " intended to be single files can be set up for automatic deletion
        " without accidentally deleting entire directories.
        for l:directory in l:info.directory_list
            call delete(l:directory, 'rf')
        endfor

        call remove(s:buffer_data, a:buffer)
    endif
endfunction

function! ale#command#CreateTempFile(buffer, temporary_file, input) abort
    if empty(a:temporary_file)
        " There is no file, so we didn't create anything.
        return 0
    endif

    " Use an existing list of lines of input if we have it, or get the lines
    " from the file.
    let l:lines = a:input isnot v:null ? a:input : getbufline(a:buffer, 1, '$')

    let l:temporary_directory = fnamemodify(a:temporary_file, ':h')
    " Create the temporary directory for the file, unreadable by 'other'
    " users.
    call mkdir(l:temporary_directory, '', 0750)
    " Automatically delete the directory later.
    call ale#command#ManageDirectory(a:buffer, l:temporary_directory)
    " Write the buffer out to a file.
    call ale#util#Writefile(a:buffer, l:lines, a:temporary_file)

    return 1
endfunction

function! s:TemporaryFilename(buffer) abort
    let l:filename = fnamemodify(bufname(a:buffer), ':t')

    if empty(l:filename)
        " If the buffer's filename is empty, create a dummy filename.
        let l:ft = getbufvar(a:buffer, '&filetype')
        let l:filename = 'file' . ale#filetypes#GuessExtension(l:ft)
    endif

    " Create a temporary filename, <temp_dir>/<original_basename>
    " The file itself will not be created by this function.
    return ale#util#Tempname() . (has('win32') ? '\' : '/') . l:filename
endfunction

" Given part of a command, replace any % with %%, so that no characters in
" the string will be replaced with filenames, etc.
function! ale#command#EscapeCommandPart(command_part) abort
    return substitute(a:command_part, '%', '%%', 'g')
endfunction

" Format a filename, converting it with filename mappings, if non-empty,
" and escaping it for putting into a command string.
"
" The filename can be modified.
function! s:FormatFilename(filename, mappings, modifiers) abort
    let l:filename = a:filename

    if !empty(a:mappings)
        let l:filename = ale#filename_mapping#Map(l:filename, a:mappings)
    endif

    if !empty(a:modifiers)
        let l:filename = fnamemodify(l:filename, a:modifiers)
    endif

    return ale#Escape(l:filename)
endfunction

" Produce a command prefix to check to a particular directory for a command.
" %s format markers with filename-modifiers can be used as the directory, and
" will be returned verbatim for formatting in paths relative to files.
function! ale#command#CdString(directory) abort
    let l:match = matchstrpos(a:directory, s:path_format_regex)
    " Do not escape the directory here if it's a valid format string.
    " This allows us to use sequences like %s:h, %s:h:h, etc.
    let l:directory = l:match[1:] == [0, len(a:directory)]
    \   ? a:directory
    \   : ale#Escape(a:directory)

    if has('win32')
        return 'cd /d ' . l:directory . ' && '
    endif

    return 'cd ' . l:directory . ' && '
endfunction

" Given a command string, replace every...
" %s -> with the current filename
" %t -> with the name of an unused file in a temporary directory
" %% -> with a literal %
function! ale#command#FormatCommand(
\   buffer,
\   executable,
\   command,
\   pipe_file_if_needed,
\   input,
\   cwd,
\   mappings,
\) abort
    let l:temporary_file = ''
    let l:command = a:command

    if !empty(a:cwd)
        let l:command = ale#command#CdString(a:cwd) . l:command
    endif

    " First replace all uses of %%, used for literal percent characters,
    " with an ugly string.
    let l:command = substitute(l:command, '%%', '<<PERCENTS>>', 'g')

    " Replace %e with the escaped executable, if available.
    if !empty(a:executable) && l:command =~# '%e'
        let l:command = substitute(l:command, '%e', '\=ale#Escape(a:executable)', 'g')
    endif

    " Replace all %s occurrences in the string with the name of the current
    " file.
    if l:command =~# '%s'
        let l:filename = fnamemodify(bufname(a:buffer), ':p')
        let l:command = substitute(
        \   l:command,
        \   s:path_format_regex,
        \   '\=s:FormatFilename(l:filename, a:mappings, submatch(1))',
        \   'g'
        \)
    endif

    if a:input isnot v:false && l:command =~# '%t'
        " Create a temporary filename, <temp_dir>/<original_basename>
        " The file itself will not be created by this function.
        let l:temporary_file = s:TemporaryFilename(a:buffer)
        let l:command = substitute(
        \   l:command,
        \   '\v\%t(%(:h|:t|:r|:e)*)',
        \   '\=s:FormatFilename(l:temporary_file, a:mappings, submatch(1))',
        \   'g'
        \)
    endif

    " Finish formatting so %% becomes %.
    let l:command = substitute(l:command, '<<PERCENTS>>', '%', 'g')

    if a:pipe_file_if_needed && empty(l:temporary_file)
        " If we are to send the Vim buffer to a command, we'll do it
        " in the shell. We'll write out the file to a temporary file,
        " and then read it back in, in the shell.
        let l:temporary_file = s:TemporaryFilename(a:buffer)
        let l:command = l:command . ' < ' . ale#Escape(l:temporary_file)
    endif

    let l:file_created = ale#command#CreateTempFile(
    \   a:buffer,
    \   l:temporary_file,
    \   a:input,
    \)

    return [l:temporary_file, l:command, l:file_created]
endfunction

function! ale#command#StopJobs(buffer, job_type) abort
    let l:info = get(s:buffer_data, a:buffer, {})

    if !empty(l:info)
        let l:new_map = {}

        for [l:job_id, l:job_type] in items(l:info.jobs)
            let l:job_id = str2nr(l:job_id)

            if a:job_type is# 'all' || a:job_type is# l:job_type
                call ale#job#Stop(l:job_id)
            else
                let l:new_map[l:job_id] = l:job_type
            endif
        endfor

        let l:info.jobs = l:new_map
    endif
endfunction

function! s:GatherOutput(line_list, job_id, line) abort
    call add(a:line_list, a:line)
endfunction

function! s:ExitCallback(buffer, line_list, Callback, data) abort
    if !has_key(s:buffer_data, a:buffer)
        return
    endif

    let l:jobs = s:buffer_data[a:buffer].jobs

    if !has_key(l:jobs, a:data.job_id)
        return
    endif

    let l:job_type = remove(l:jobs, a:data.job_id)

    if g:ale_history_enabled
        call ale#history#SetExitCode(a:buffer, a:data.job_id, a:data.exit_code)

        " Log the output of the command for ALEInfo if we should.
        if g:ale_history_log_output && a:data.log_output is 1
            call ale#history#RememberOutput(
            \   a:buffer,
            \   a:data.job_id,
            \   a:line_list[:]
            \)
        endif
    endif

    " If the callback starts any new jobs, use the same job type for them.
    call setbufvar(a:buffer, 'ale_job_type', l:job_type)
    let l:value = a:Callback(a:buffer, a:line_list, {
    \   'exit_code': a:data.exit_code,
    \   'temporary_file': a:data.temporary_file,
    \})

    let l:result = a:data.result
    let l:result.value = l:value

    " Set the default cwd for this buffer in this call stack.
    call ale#command#SetCwd(a:buffer, l:result.cwd)

    try
        if get(l:result, 'result_callback', v:null) isnot v:null
            call call(l:result.result_callback, [l:value])
        endif
    finally
        call ale#command#ResetCwd(a:buffer)
    endtry
endfunction

function! ale#command#Run(buffer, command, Callback, ...) abort
    let l:options = get(a:000, 0, {})

    if len(a:000) > 1
        throw 'Too many arguments!'
    endif

    let l:output_stream = get(l:options, 'output_stream', 'stdout')
    let l:line_list = []
    let l:cwd = get(l:options, 'cwd', v:null)

    if l:cwd is v:null
        " Default the working directory to whatever it was for the last
        " command run in the chain.
        let l:cwd = get(get(s:buffer_data, a:buffer, {}), 'cwd', v:null)
    endif

    let [l:temporary_file, l:command, l:file_created] = ale#command#FormatCommand(
    \   a:buffer,
    \   get(l:options, 'executable', ''),
    \   a:command,
    \   get(l:options, 'read_buffer', 0),
    \   get(l:options, 'input', v:null),
    \   l:cwd,
    \   get(l:options, 'filename_mappings', []),
    \)
    let l:command = ale#job#PrepareCommand(a:buffer, l:command)
    let l:job_options = {
    \   'exit_cb': {job_id, exit_code -> s:ExitCallback(
    \       a:buffer,
    \       l:line_list,
    \       a:Callback,
    \       {
    \           'job_id': job_id,
    \           'exit_code': exit_code,
    \           'temporary_file': l:temporary_file,
    \           'log_output': get(l:options, 'log_output', 1),
    \           'result': l:result,
    \       }
    \   )},
    \   'mode': 'nl',
    \}

    if l:output_stream is# 'stdout'
        let l:job_options.out_cb = function('s:GatherOutput', [l:line_list])
    elseif l:output_stream is# 'stderr'
        let l:job_options.err_cb = function('s:GatherOutput', [l:line_list])
    elseif l:output_stream is# 'both'
        let l:job_options.out_cb = function('s:GatherOutput', [l:line_list])
        let l:job_options.err_cb = function('s:GatherOutput', [l:line_list])
    endif

    let l:status = 'failed'

    if get(g:, 'ale_run_synchronously') == 1
        if get(g:, 'ale_emulate_job_failure') == 1
            let l:job_id = 0
        else
            " Generate a fake job ID for tests.
            let s:fake_job_id = get(s:, 'fake_job_id', 0) + 1
            let l:job_id = s:fake_job_id
        endif
    elseif has('win32')
        let l:job_id = ale#job#StartWithCmd(l:command, l:job_options)
    else
        let l:job_id = ale#job#Start(l:command, l:job_options)
    endif

    if l:job_id
        let l:status = 'started'
        let l:job_type = getbufvar(a:buffer, 'ale_job_type', 'all')

        call ale#command#InitData(a:buffer)
        let s:buffer_data[a:buffer].jobs[l:job_id] = l:job_type
    endif

    if g:ale_history_enabled
        call ale#history#Add(a:buffer, l:status, l:job_id, l:command)
    endif

    if !l:job_id
        return 0
    endif

    " We'll return this Dictionary. A `result_callback` can be assigned to it
    " later for capturing the result of a:Callback.
    "
    " The `_deferred_job_id` is used for both checking the type of object, and
    " for checking the job ID and status.
    "
    " The cwd is kept and used as the default value for the next command in
    " the chain.
    "
    " The original command here is used in tests.
    let l:result = {
    \   '_deferred_job_id': l:job_id,
    \   'executable': get(l:options, 'executable', ''),
    \   'cwd': l:cwd,
    \   'command': a:command,
    \}

    if get(g:, 'ale_run_synchronously') == 1 && l:job_id
        if !exists('g:ale_run_synchronously_callbacks')
            let g:ale_run_synchronously_callbacks = []
        endif

        if get(g:, 'ale_run_synchronously_emulate_commands', 0)
            call add(
            \   g:ale_run_synchronously_callbacks,
            \   {exit_code, output -> [
            \       extend(l:line_list, output),
            \       l:job_options.exit_cb(l:job_id, exit_code),
            \   ]}
            \)
        else
            " Run a command synchronously if this test option is set.
            call extend(l:line_list, systemlist(
            \   type(l:command) is v:t_list
            \       ? join(l:command[0:1]) . ' ' . ale#Escape(l:command[2])
            \       : l:command
            \))

            " Don't capture output when the callbacks aren't set.
            if !has_key(l:job_options, 'out_cb')
            \&& !has_key(l:job_options, 'err_cb')
                let l:line_list = []
            endif

            call add(
            \   g:ale_run_synchronously_callbacks,
            \   {-> l:job_options.exit_cb(l:job_id, v:shell_error)}
            \)
        endif
    endif

    return l:result
endfunction

function! ale#command#IsDeferred(value) abort
    return type(a:value) is v:t_dict && has_key(a:value, '_deferred_job_id')
endfunction
