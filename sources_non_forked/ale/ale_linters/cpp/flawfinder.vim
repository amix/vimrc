" Author: Christian Gibbons <cgibbons@gmu.edu>
" Description: flawfinder linter for c++ files

call ale#Set('cpp_flawfinder_executable', 'flawfinder')
call ale#Set('cpp_flawfinder_options', '')
call ale#Set('cpp_flawfinder_minlevel', 1)
call ale#Set('c_flawfinder_error_severity', 6)

function! ale_linters#cpp#flawfinder#GetExecutable(buffer) abort
   return ale#Var(a:buffer, 'cpp_flawfinder_executable')
endfunction

function! ale_linters#cpp#flawfinder#GetCommand(buffer) abort

   " Set the minimum vulnerability level for flawfinder to bother with
   let l:minlevel = ' --minlevel=' . ale#Var(a:buffer, 'cpp_flawfinder_minlevel')

   return ale#Escape(ale_linters#cpp#flawfinder#GetExecutable(a:buffer))
   \  . ' -CDQS'
   \  . ale#Var(a:buffer, 'cpp_flawfinder_options')
   \  . l:minlevel
   \  . ' %t'
endfunction

call ale#linter#Define('cpp', {
\  'name': 'flawfinder',
\  'output_stream': 'stdout',
\  'executable_callback': 'ale_linters#cpp#flawfinder#GetExecutable',
\  'command_callback': 'ale_linters#cpp#flawfinder#GetCommand',
\  'callback': 'ale#handlers#flawfinder#HandleFlawfinderFormat',
\})
