" Author: Leo <thinkabit.ukim@gmail.com>
" Description: secfixes-check from atools linter for APKBUILDs

call ale#Set('apkbuild_secfixes_check_executable', 'secfixes-check')

call ale#linter#Define('apkbuild', {
\   'name': 'secfixes_check',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'apkbuild_secfixes_check_executable')},
\   'command': '%e %t',
\   'callback': 'ale#handlers#atools#Handle',
\})
