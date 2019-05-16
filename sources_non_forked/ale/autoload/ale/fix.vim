" Apply fixes queued up for buffers which may be hidden.
" Vim doesn't let you modify hidden buffers.
function! ale#fix#ApplyQueuedFixes() abort
    let l:buffer = bufnr('')
    let l:data = get(g:ale_fix_buffer_data, l:buffer, {'done': 0})

    if !l:data.done
        return
    endif

    call remove(g:ale_fix_buffer_data, l:buffer)

    if l:data.changes_made
        let l:start_line = len(l:data.output) + 1
        let l:end_line = len(l:data.lines_before)

        if l:end_line >= l:start_line
            let l:save = winsaveview()
            silent execute l:start_line . ',' . l:end_line . 'd_'
            call winrestview(l:save)
        endif

        " If the file is in DOS mode, we have to remove carriage returns from
        " the ends of lines before calling setline(), or we will see them
        " twice.
        let l:lines_to_set = getbufvar(l:buffer, '&fileformat') is# 'dos'
        \   ? map(copy(l:data.output), 'substitute(v:val, ''\r\+$'', '''', '''')')
        \   : l:data.output

        call setline(1, l:lines_to_set)

        if l:data.should_save
            if empty(&buftype)
                noautocmd :w!
            else
                set nomodified
            endif
        endif
    endif

    if l:data.should_save
        let l:should_lint = g:ale_fix_on_save
    else
        let l:should_lint = l:data.changes_made
    endif

    silent doautocmd <nomodeline> User ALEFixPost

    " If ALE linting is enabled, check for problems with the file again after
    " fixing problems.
    if g:ale_enabled
    \&& l:should_lint
    \&& !ale#events#QuitRecently(l:buffer)
        call ale#Queue(0, l:data.should_save ? 'lint_file' : '')
    endif
endfunction

function! ale#fix#ApplyFixes(buffer, output) abort
    let l:data = g:ale_fix_buffer_data[a:buffer]
    let l:data.output = a:output
    let l:data.changes_made = l:data.lines_before != l:data.output
    let l:data.done = 1

    call ale#command#RemoveManagedFiles(a:buffer)

    if !bufexists(a:buffer)
        " Remove the buffer data when it doesn't exist.
        call remove(g:ale_fix_buffer_data, a:buffer)
    endif

    if l:data.changes_made && bufexists(a:buffer)
        let l:lines = getbufline(a:buffer, 1, '$')

        if l:data.lines_before != l:lines
            call remove(g:ale_fix_buffer_data, a:buffer)
            execute 'echoerr ''The file was changed before fixing finished'''

            return
        endif
    endif

    " We can only change the lines of a buffer which is currently open,
    " so try and apply the fixes to the current buffer.
    call ale#fix#ApplyQueuedFixes()
endfunction

function! s:HandleExit(job_info, buffer, job_output, data) abort
    let l:buffer_info = get(g:ale_fix_buffer_data, a:buffer, {})

    if empty(l:buffer_info)
        return
    endif

    if a:job_info.read_temporary_file
        let l:output = !empty(a:data.temporary_file)
        \   ?  readfile(a:data.temporary_file)
        \   : []
    else
        let l:output = a:job_output
    endif

    let l:ChainCallback = get(a:job_info, 'chain_with', v:null)
    let l:ProcessWith = get(a:job_info, 'process_with', v:null)

    " Post-process the output with a function if we have one.
    if l:ProcessWith isnot v:null
        let l:output = call(l:ProcessWith, [a:buffer, l:output])
    endif

    " Use the output of the job for changing the file if it isn't empty,
    " otherwise skip this job and use the input from before.
    "
    " We'll use the input from before for chained commands.
    if l:ChainCallback is v:null && !empty(split(join(l:output)))
        let l:input = l:output
    else
        let l:input = a:job_info.input
    endif

    let l:next_index = l:ChainCallback is v:null
    \   ? a:job_info.callback_index + 1
    \   : a:job_info.callback_index

    call s:RunFixer({
    \   'buffer': a:buffer,
    \   'input': l:input,
    \   'output': l:output,
    \   'callback_list': a:job_info.callback_list,
    \   'callback_index': l:next_index,
    \   'chain_callback': l:ChainCallback,
    \})
endfunction

function! s:RunJob(result, options) abort
    if ale#command#IsDeferred(a:result)
        let a:result.result_callback = {x -> s:RunJob(x, a:options)}

        return
    endif

    let l:buffer = a:options.buffer
    let l:input = a:options.input

    if a:result is 0 || type(a:result) is v:t_list
        if type(a:result) is v:t_list
            let l:input = a:result
        endif

        call s:RunFixer({
        \   'buffer': l:buffer,
        \   'input': l:input,
        \   'callback_index': a:options.callback_index + 1,
        \   'callback_list': a:options.callback_list,
        \})

        return
    endif

    let l:command = get(a:result, 'command', '')
    let l:ChainWith = get(a:result, 'chain_with', v:null)

    if empty(l:command)
        " If the command is empty, skip to the next item, or call the
        " chain_with function.
        call s:RunFixer({
        \   'buffer': l:buffer,
        \   'input': l:input,
        \   'callback_index': a:options.callback_index + (l:ChainWith is v:null),
        \   'callback_list': a:options.callback_list,
        \   'chain_callback': l:ChainWith,
        \   'output': [],
        \})

        return
    endif

    let l:read_temporary_file = get(a:result, 'read_temporary_file', 0)
    " Default to piping the buffer for the last fixer in the chain.
    let l:read_buffer = get(a:result, 'read_buffer', l:ChainWith is v:null)
    let l:output_stream = get(a:result, 'output_stream', 'stdout')

    if l:read_temporary_file
        let l:output_stream = 'none'
    endif

    let l:Callback = function('s:HandleExit', [{
    \   'input': l:input,
    \   'chain_with': l:ChainWith,
    \   'callback_index': a:options.callback_index,
    \   'callback_list': a:options.callback_list,
    \   'process_with': get(a:result, 'process_with', v:null),
    \   'read_temporary_file': l:read_temporary_file,
    \}])
    let l:run_result = ale#command#Run(l:buffer, l:command, l:Callback, {
    \   'output_stream': l:output_stream,
    \   'executable': '',
    \   'read_buffer': l:read_buffer,
    \   'input': l:input,
    \   'log_output': 0,
    \})

    if empty(l:run_result)
        call s:RunFixer({
        \   'buffer': l:buffer,
        \   'input': l:input,
        \   'callback_index': a:options.callback_index + 1,
        \   'callback_list': a:options.callback_list,
        \})
    endif
endfunction

function! s:RunFixer(options) abort
    let l:buffer = a:options.buffer
    let l:input = a:options.input
    let l:index = a:options.callback_index

    if len(a:options.callback_list) <= l:index
        call ale#fix#ApplyFixes(l:buffer, l:input)

        return
    endif

    let l:ChainCallback = get(a:options, 'chain_callback', v:null)

    let l:Function = l:ChainCallback isnot v:null
    \   ? ale#util#GetFunction(l:ChainCallback)
    \   : a:options.callback_list[l:index]

    " Record new jobs started as fixer jobs.
    call setbufvar(l:buffer, 'ale_job_type', 'fixer')

    if l:ChainCallback isnot v:null
        " Chained commands accept (buffer, output, [input])
        let l:result = ale#util#FunctionArgCount(l:Function) == 2
        \   ? call(l:Function, [l:buffer, a:options.output])
        \   : call(l:Function, [l:buffer, a:options.output, copy(l:input)])
    else
        " Regular fixer commands accept (buffer, [input])
        let l:result = ale#util#FunctionArgCount(l:Function) == 1
        \   ? call(l:Function, [l:buffer])
        \   : call(l:Function, [l:buffer, copy(l:input)])
    endif

    call s:RunJob(l:result, {
    \   'buffer': l:buffer,
    \   'input': l:input,
    \   'callback_list': a:options.callback_list,
    \   'callback_index': l:index,
    \})
endfunction

function! s:AddSubCallbacks(full_list, callbacks) abort
    if type(a:callbacks) is v:t_string
        call add(a:full_list, a:callbacks)
    elseif type(a:callbacks) is v:t_list
        call extend(a:full_list, a:callbacks)
    else
        return 0
    endif

    return 1
endfunction

function! s:GetCallbacks(buffer, fixers) abort
    if len(a:fixers)
        let l:callback_list = a:fixers
    elseif type(get(b:, 'ale_fixers')) is v:t_list
        " Lists can be used for buffer-local variables only
        let l:callback_list = b:ale_fixers
    else
        " buffer and global options can use dictionaries mapping filetypes to
        " callbacks to run.
        let l:fixers = ale#Var(a:buffer, 'fixers')
        let l:callback_list = []
        let l:matched = 0

        for l:sub_type in split(&filetype, '\.')
            if s:AddSubCallbacks(l:callback_list, get(l:fixers, l:sub_type))
                let l:matched = 1
            endif
        endfor

        " If we couldn't find fixers for a filetype, default to '*' fixers.
        if !l:matched
            call s:AddSubCallbacks(l:callback_list, get(l:fixers, '*'))
        endif
    endif

    if empty(l:callback_list)
        return []
    endif

    let l:corrected_list = []

    " Variables with capital characters are needed, or Vim will complain about
    " funcref variables.
    for l:Item in l:callback_list
        if type(l:Item) is v:t_string
            let l:Func = ale#fix#registry#GetFunc(l:Item)

            if !empty(l:Func)
                let l:Item = l:Func
            endif
        endif

        try
            call add(l:corrected_list, ale#util#GetFunction(l:Item))
        catch /E475/
            " Rethrow exceptions for failing to get a function so we can print
            " a friendly message about it.
            throw 'BADNAME ' . v:exception
        endtry
    endfor

    return l:corrected_list
endfunction

function! ale#fix#InitBufferData(buffer, fixing_flag) abort
    " The 'done' flag tells the function for applying changes when fixing
    " is complete.
    let g:ale_fix_buffer_data[a:buffer] = {
    \   'lines_before': getbufline(a:buffer, 1, '$'),
    \   'done': 0,
    \   'should_save': a:fixing_flag is# 'save_file',
    \   'temporary_directory_list': [],
    \}
endfunction

" Accepts an optional argument for what to do when fixing.
"
" Returns 0 if no fixes can be applied, and 1 if fixing can be done.
function! ale#fix#Fix(buffer, fixing_flag, ...) abort
    if a:fixing_flag isnot# '' && a:fixing_flag isnot# 'save_file'
        throw "fixing_flag must be either '' or 'save_file'"
    endif

    try
        let l:callback_list = s:GetCallbacks(a:buffer, a:000)
    catch /E700\|BADNAME/
        let l:function_name = join(split(split(v:exception, ':')[3]))
        let l:echo_message = printf(
        \   'There is no fixer named `%s`. Check :ALEFixSuggest',
        \   l:function_name,
        \)
        execute 'echom l:echo_message'

        return 0
    endtry

    if empty(l:callback_list)
        if a:fixing_flag is# ''
            execute 'echom ''No fixers have been defined. Try :ALEFixSuggest'''
        endif

        return 0
    endif

    call ale#command#StopJobs(a:buffer, 'fixer')
    " Clean up any files we might have left behind from a previous run.
    call ale#command#RemoveManagedFiles(a:buffer)
    call ale#fix#InitBufferData(a:buffer, a:fixing_flag)

    silent doautocmd <nomodeline> User ALEFixPre

    call s:RunFixer({
    \   'buffer': a:buffer,
    \   'input': g:ale_fix_buffer_data[a:buffer].lines_before,
    \   'callback_index': 0,
    \   'callback_list': l:callback_list,
    \})

    return 1
endfunction

" Set up an autocmd command to try and apply buffer fixes when available.
augroup ALEBufferFixGroup
    autocmd!
    autocmd BufEnter * call ale#fix#ApplyQueuedFixes()
augroup END
