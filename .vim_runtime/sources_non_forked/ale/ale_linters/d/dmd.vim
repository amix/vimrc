" Author: w0rp <devw0rp@gmail.com>
" Description: "dmd for D files"

function! s:GetDUBCommand(buffer) abort
    " If we can't run dub, then skip this command.
    if executable('dub')
        " Returning an empty string skips to the DMD command.
        let l:config = ale#d#FindDUBConfig(a:buffer)

        " To support older dub versions, we just change the directory to the
        " directory where we found the dub config, and then run `dub describe`
        " from that directory.
        if !empty(l:config)
            return [fnamemodify(l:config, ':h'), 'dub describe --data-list
            \ --data=import-paths
            \ --data=string-import-paths
            \ --data=versions
            \ --data=debug-versions
            \']
        endif
    endif

    return ['', '']
endfunction

function! ale_linters#d#dmd#RunDUBCommand(buffer) abort
    let [l:cwd, l:command] = s:GetDUBCommand(a:buffer)

    if empty(l:command)
        " If we can't run DUB, just run DMD.
        return ale_linters#d#dmd#DMDCommand(a:buffer, [], {})
    endif

    return ale#command#Run(
    \   a:buffer,
    \   l:command,
    \   function('ale_linters#d#dmd#DMDCommand'),
    \   {'cwd': l:cwd},
    \)
endfunction

function! ale_linters#d#dmd#DMDCommand(buffer, dub_output, meta) abort
    let l:import_list = []
    let l:str_import_list = []
    let l:versions_list = []
    let l:deb_versions_list = []
    let l:list_ind = 1
    let l:seen_line = 0

    " Build a list of options generated from DUB, if available.
    " DUB output each path or version on a single line.
    " Each list is separated by a blank line.
    " Empty list are represented by a blank line (followed and/or
    " preceded by a separation blank line)
    for l:line in a:dub_output
        " line still has end of line char on windows
        let l:line = substitute(l:line, '[\r\n]*$', '', '')

        if !empty(l:line)
            if l:list_ind == 1
                call add(l:import_list, '-I' . ale#Escape(l:line))
            elseif l:list_ind == 2
                call add(l:str_import_list, '-J' . ale#Escape(l:line))
            elseif l:list_ind == 3
                call add(l:versions_list, '-version=' . ale#Escape(l:line))
            elseif l:list_ind == 4
                call add(l:deb_versions_list, '-debug=' . ale#Escape(l:line))
            endif

            let l:seen_line = 1
        elseif !l:seen_line
            " if list is empty must skip one empty line
            let l:seen_line = 1
        else
            let l:seen_line = 0
            let l:list_ind += 1
        endif
    endfor

    return 'dmd ' . join(l:import_list) . ' ' .
    \   join(l:str_import_list) . ' ' .
    \   join(l:versions_list) . ' ' .
    \   join(l:deb_versions_list) . ' -o- -wi -vcolumns -c %t'
endfunction

function! ale_linters#d#dmd#Handle(buffer, lines) abort
    " Matches patterns lines like the following:
    " /tmp/tmp.qclsa7qLP7/file.d(1): Error: function declaration without return type. (Note that constructors are always named 'this')
    " /tmp/tmp.G1L5xIizvB.d(8,8): Error: module weak_reference is in file 'dstruct/weak_reference.d' which cannot be read
    let l:pattern = '\v^(\f+)\((\d+)(,(\d+))?\): (\w+): (.+)$'
    let l:output = []
    let l:dir = expand('#' . a:buffer . ':p:h')

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        " If dmd was invoked with relative path, match[1] is relative, otherwise it is absolute.
        " As we invoke dmd with the buffer path (in /tmp), this will generally be absolute already
        let l:fname = ale#path#GetAbsPath(l:dir, l:match[1])
        call add(l:output, {
        \   'filename': l:fname,
        \   'lnum': l:match[2],
        \   'col': l:match[4],
        \   'type': l:match[5] is# 'Warning' || l:match[5] is# 'Deprecation' ? 'W' : 'E',
        \   'text': l:match[6],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('d', {
\   'name': 'dmd',
\   'executable': 'dmd',
\   'command': function('ale_linters#d#dmd#RunDUBCommand'),
\   'callback': 'ale_linters#d#dmd#Handle',
\   'output_stream': 'stderr',
\})
