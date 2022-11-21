" Author: Leo <thinkabit.ukim@gmail.com>
" Description: apkbuild-lint from atools linter for APKBUILDs

call ale#Set('apkbuild_apkbuild_lint_executable', 'apkbuild-lint')

call ale#linter#Define('apkbuild', {
\   'name': 'apkbuild_lint',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'apkbuild_apkbuild_lint_executable')},
\   'command': '%e %t',
\   'callback': 'ale#handlers#atools#Handle',
\})
