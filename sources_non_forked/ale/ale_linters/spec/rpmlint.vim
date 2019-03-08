" Author: Jason Tibbitts <tibbs@math.uh.edu>
" Description: Adds support for checking RPM spec files with rpmlint

" rpmlint will produce varions types of output:
"
" Lines like the following are output when the file is simply not able to be
" parsed by rpmspec -P:
"   apcupsd.spec: E: specfile-error warning: bogus date in %changelog: Mon Oct 1 2005 - Foo
"   apcupsd.spec: E: specfile-error error: %changelog not in descending chronological order
" They do not contain a line number, and there's not a whole lot that can be
" done to locate them besides grep for them.  rpmlint is just passing the
" output from rpm along with the filename, an error indicator, and an error
" type.
"
" Lines like the following:
"   cyrus-imapd.spec:23: W: macro-in-comment %version
"   cyrus-imapd.spec:18: E: hardcoded-library-path in %_prefix/lib/%name
" indicate warnings and errors, respectively.  No column numbers are provided
"
" Lines like:
"   apcupsd.spec: I: checking
"   apcupsd.spec: I: checking-url https://downloads.sourceforge.net/apcupsd/apcupsd-3.14.14.tar.gz (timeout 10 seconds)
" are merely informational and are only output when -v is passed.  But they
" may be useful in a log to know why things are taking so long.
"
" And this is always output at the end and should just be ignored:
"   0 packages and 1 specfiles checked; 4 errors, 0 warnings.

call ale#Set('spec_rpmlint_executable', 'rpmlint')
call ale#Set('spec_rpmlint_options', '')

function! ale_linters#spec#rpmlint#GetCommand(buffer) abort
    return '%e'
    \   . ale#Pad(ale#Var(a:buffer, 'spec_rpmlint_options'))
    \   . ' -o "NetworkEnabled False"'
    \   . ' -v'
    \   . ' %t'
endfunction

function! ale_linters#spec#rpmlint#Handle(buffer, lines) abort
    " let l:pat_inform = '^.\+: I: \(.+\)'
    let l:pat_errwarn = '^.\+:\(\d\+\): \([EW]\): \(.\+\)'
    let l:pat_baderr = '^.\+: E: \(.\+\)'
    let l:output = []

    for l:line in a:lines
        let l:match_errwarn = matchlist(l:line, l:pat_errwarn)
        let l:match_baderr = matchlist(l:line, l:pat_baderr)

        if len(l:match_errwarn) > 0
            let l:text = l:match_errwarn[3]
            let l:type = l:match_errwarn[2]
            let l:lnum = l:match_errwarn[1] + 0
        elseif len(l:match_baderr) > 0
            let l:text = l:match_baderr[1]
            let l:type = 'E'
            let l:lnum = 1
        else
            continue
        endif

        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': l:lnum,
        \   'text': l:text,
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('spec', {
\   'name': 'rpmlint',
\   'executable': {b -> ale#Var(b, 'spec_rpmlint_executable')},
\   'command': function('ale_linters#spec#rpmlint#GetCommand'),
\   'callback': 'ale_linters#spec#rpmlint#Handle',
\})
