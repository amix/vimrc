call ale#Set('cs_mcsc_options', '')
call ale#Set('cs_mcsc_source', '')
call ale#Set('cs_mcsc_assembly_path', [])
call ale#Set('cs_mcsc_assemblies', [])

function! s:GetWorkingDirectory(buffer) abort
    let l:working_directory = ale#Var(a:buffer, 'cs_mcsc_source')

    if !empty(l:working_directory)
        return l:working_directory
    endif

    return expand('#' . a:buffer . ':p:h')
endfunction

function! ale_linters#cs#mcsc#GetCommand(buffer) abort
    " Pass assembly paths via the -lib: parameter.
    let l:path_list = ale#Var(a:buffer, 'cs_mcsc_assembly_path')

    let l:lib_option = !empty(l:path_list)
    \   ? '-lib:' . join(map(copy(l:path_list), 'ale#Escape(v:val)'), ',')
    \   : ''

    " Pass paths to DLL files via the -r: parameter.
    let l:assembly_list = ale#Var(a:buffer, 'cs_mcsc_assemblies')

    let l:r_option = !empty(l:assembly_list)
    \   ? '-r:' . join(map(copy(l:assembly_list), 'ale#Escape(v:val)'), ',')
    \   : ''

    " register temporary module target file with ale
    " register temporary module target file with ALE.
    let l:out = ale#engine#CreateFile(a:buffer)

    " The code is compiled as a module and the output is redirected to a
    " temporary file.
    return ale#path#CdString(s:GetWorkingDirectory(a:buffer))
    \    . 'mcs -unsafe'
    \    . ale#Pad(ale#Var(a:buffer, 'cs_mcsc_options'))
    \    . ale#Pad(l:lib_option)
    \    . ale#Pad(l:r_option)
    \    . ' -out:' . l:out
    \    . ' -t:module'
    \    . ' -recurse:' . ale#Escape('*.cs')
endfunction

function! ale_linters#cs#mcsc#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " Tests.cs(12,29): error CSXXXX: ; expected
    "
    " NOTE: pattern also captures file name as linter compiles all
    " files within the source tree rooted at the specified source
    " path and not just the file loaded in the buffer
    let l:pattern = '^\v(.+\.cs)\((\d+),(\d+)\)\: ([^ ]+) ([^ ]+): (.+)$'
    let l:output = []

    let l:dir = s:GetWorkingDirectory(a:buffer)

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'type': l:match[4] is# 'error' ? 'E' : 'W',
        \   'code': l:match[5],
        \   'text': l:match[6],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('cs',{
\   'name': 'mcsc',
\   'output_stream': 'stderr',
\   'executable': 'mcs',
\   'command_callback': 'ale_linters#cs#mcsc#GetCommand',
\   'callback': 'ale_linters#cs#mcsc#Handle',
\   'lint_file': 1
\})
