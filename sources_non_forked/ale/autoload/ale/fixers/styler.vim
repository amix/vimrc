" Author: tvatter <thibault.vatter@gmail.com>
" Description: Fixing R files with styler.

call ale#Set('r_styler_executable', 'Rscript')
call ale#Set('r_styler_options', 'tidyverse_style')

function! ale#fixers#styler#Fix(buffer) abort
    return {
    \   'command': 'Rscript --vanilla -e '
    \       . '"suppressPackageStartupMessages(library(styler));'
    \       . 'style_file(commandArgs(TRUE), style = '
    \       . ale#Var(a:buffer, 'r_styler_options') . ')"'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
