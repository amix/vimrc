" vint: -ProhibitSetNoCompatible
set nocompatible nomore shellslash encoding=utf-8 shortmess+=WIF
lang mess C

if $RUNBENCH_SETTINGS is? 'all'
  let $RUNBENCH_SETTINGS = join(['array_whitespace_error', 'build_constraints',
        \ 'chan_whitespace_error', 'extra_types', 'fields', 'format_strings',
        \ 'function_arguments', 'function_calls', 'functions', 'generate_tags',
        \ 'operators', 'space_tab_error', 'string_spellcheck',
        \ 'trailing_whitespace_error', 'types', 'variable_assignments',
        \ 'variable_declarations'], ' ')
endif

for s:s in split($RUNBENCH_SETTINGS, ' ')
  call execute('let g:go_highlight_' . s:s . ' = 1')
endfor

filetype plugin indent on
syntax on

syntime on
redraw!
let s:report = execute('syntime report')
execute ':e ' . fnameescape($RUNBENCH_OUT)
call setline('.', split(s:report, '\n'))
wq
