" Author: Brandon Roehl - https://github.com/BrandonRoehl
" Description: Ruby MRI for Ruby files

call ale#Set('ruby_ruby_executable', 'ruby')

call ale#linter#Define('ruby', {
\   'name': 'ruby',
\   'executable': {b -> ale#Var(b, 'ruby_ruby_executable')},
\   'command': '%e -w -c %t',
\   'output_stream': 'stderr',
\   'callback': 'ale#handlers#ruby#HandleSyntaxErrors',
\})
