call ale#Set('cs_csc_options', '')
call ale#Set('cs_csc_source', '')
call ale#Set('cs_csc_assembly_path', [])
call ale#Set('cs_csc_assemblies', [])

function! s:GetWorkingDirectory(buffer) abort
    let l:working_directory = ale#Var(a:buffer, 'cs_csc_source')

    if !empty(l:working_directory)
        return l:working_directory
    endif

    return expand('#' . a:buffer . ':p:h')
endfunction

function! ale_linters#cs#csc#GetCommand(buffer) abort
    " Pass assembly paths via the -lib: parameter.
    let l:path_list = ale#Var(a:buffer, 'cs_csc_assembly_path')

    let l:lib_option = !empty(l:path_list)
    \   ? '/lib:' . join(map(copy(l:path_list), 'ale#Escape(v:val)'), ',')
    \   : ''

    " Pass paths to DLL files via the -r: parameter.
    let l:assembly_list = ale#Var(a:buffer, 'cs_csc_assemblies')

    let l:r_option = !empty(l:assembly_list)
    \   ? '/r:' . join(map(copy(l:assembly_list), 'ale#Escape(v:val)'), ',')
    \   : ''

    " register temporary module target file with ale
    " register temporary module target file with ALE.
    let l:out = ale#command#CreateFile(a:buffer)

    " The code is compiled as a module and the output is redirected to a
    " temporary file.
    return ale#path#CdString(s:GetWorkingDirectory(a:buffer))
    \    . 'csc /unsafe'
    \    . ale#Pad(ale#Var(a:buffer, 'cs_csc_options'))
    \    . ale#Pad(l:lib_option)
    \    . ale#Pad(l:r_option)
    \    . ' /out:' . l:out
    \    . ' /t:module'
    \    . ' /recurse:' . ale#Escape('*.cs')
endfunction

function! ale_linters#cs#csc#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " Tests.cs(12,29): error CSXXXX: ; expected
    "
    " NOTE: pattern also captures file name as linter compiles all
    " files within the source tree rooted at the specified source
    " path and not just the file loaded in the buffer
    let l:patterns = [
    \    '^\v(.+\.cs)\((\d+),(\d+)\)\:\s+([^ ]+)\s+([cC][sS][^ ]+):\s(.+)$',
    \    '^\v([^ ]+)\s+([Cc][sS][^ ]+):\s+(.+)$',
    \]
    let l:output = []

    let l:dir = s:GetWorkingDirectory(a:buffer)

    for l:match in ale#util#GetMatches(a:lines, l:patterns)
        if len(l:match) > 6 && strlen(l:match[5]) > 2 && l:match[5][:1] is? 'CS'
            call add(l:output, {
            \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
            \   'lnum': l:match[2] + 0,
            \   'col': l:match[3] + 0,
            \   'type': l:match[4] is# 'error' ? 'E' : 'W',
            \   'code': l:match[5],
            \   'text': l:match[6] ,
            \})
        elseif strlen(l:match[2]) > 2 && l:match[2][:1] is? 'CS'
            call add(l:output, {
            \   'filename':'<csc>',
            \   'lnum': -1,
            \   'col': -1,
            \   'type': l:match[1] is# 'error' ? 'E' : 'W',
            \   'code': l:match[2],
            \   'text': l:match[3],
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('cs',{
\   'name': 'csc',
\   'output_stream': 'stdout',
\   'executable': 'csc',
\   'command': function('ale_linters#cs#csc#GetCommand'),
\   'callback': 'ale_linters#cs#csc#Handle',
\   'lint_file': 1
\})
