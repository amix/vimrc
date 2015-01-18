if exists("g:go_loaded_commands")
    finish
endif
let g:go_loaded_commands = 1


" Some handy plug mappings
nnoremap <silent> <Plug>(go-run) :<C-u>call go#cmd#Run(expand('%'))<CR>
nnoremap <silent> <Plug>(go-build) :<C-u>call go#cmd#Build('')<CR>
nnoremap <silent> <Plug>(go-install) :<C-u>call go#cmd#Install()<CR>
nnoremap <silent> <Plug>(go-test) :<C-u>call go#cmd#Test('')<CR>
nnoremap <silent> <Plug>(go-coverage) :<C-u>call go#cmd#Coverage('')<CR>
nnoremap <silent> <Plug>(go-vet) :<C-u>call go#cmd#Vet()<CR>
nnoremap <silent> <Plug>(go-files) :<C-u>call go#tool#Files()<CR>
nnoremap <silent> <Plug>(go-deps) :<C-u>call go#tool#Deps()<CR>
nnoremap <silent> <Plug>(go-info) :<C-u>call go#complete#Info()<CR>
nnoremap <silent> <Plug>(go-import) :<C-u>call go#import#SwitchImport(1, '', expand('<cword>'))<CR>

nnoremap <silent> <Plug>(go-implements) :<C-u>call go#oracle#Implements(-1)<CR>
nnoremap <silent> <Plug>(go-callees) :<C-u>call go#oracle#Callees(-1)<CR>

nnoremap <silent> <Plug>(go-rename) :<C-u>call go#rename#Rename()<CR>

nnoremap <silent> <Plug>(go-def) :<C-u>call go#def#Jump()<CR>
nnoremap <silent> <Plug>(go-def-vertical) :<C-u>call go#def#JumpMode("vsplit")<CR>
nnoremap <silent> <Plug>(go-def-split) :<C-u>call go#def#JumpMode("split")<CR>
nnoremap <silent> <Plug>(go-def-tab) :<C-u>call go#def#JumpMode("tab")<CR>

nnoremap <silent> <Plug>(go-doc) :<C-u>call go#doc#Open("new", "split")<CR>
nnoremap <silent> <Plug>(go-doc-tab) :<C-u>call go#doc#Open("tabnew", "tabe")<CR>
nnoremap <silent> <Plug>(go-doc-vertical) :<C-u>call go#doc#Open("vnew", "vsplit")<CR>
nnoremap <silent> <Plug>(go-doc-split) :<C-u>call go#doc#Open("new", "split")<CR>
nnoremap <silent> <Plug>(go-doc-browser) :<C-u>call go#doc#OpenBrowser()<CR>


" gorename
command! -nargs=? GoRename call go#rename#Rename(<f-args>)

" oracle
command! -range=% GoImplements call go#oracle#Implements(<count>)
command! -range=% GoCallees call go#oracle#Callees(<count>)

" tool
command! -nargs=0 GoFiles echo go#tool#Files()
command! -nargs=0 GoDeps echo go#tool#Deps()
command! -nargs=* GoInfo call go#complete#Info()

" cmd
command! -nargs=* -bang GoRun call go#cmd#Run(<bang>0,<f-args>)
command! -nargs=* -bang GoBuild call go#cmd#Build(<bang>0,<f-args>)
command! -nargs=* GoInstall call go#cmd#Install(<f-args>)
command! -nargs=* GoTest call go#cmd#Test(<f-args>)
command! -nargs=* GoCoverage call go#cmd#Coverage(<f-args>)
command! -nargs=0 GoVet call go#cmd#Vet()

" -- play
command! -nargs=0 -range=% GoPlay call go#play#Share(<count>, <line1>, <line2>)

" -- def
command! -nargs=* -range GoDef :call go#def#Jump(<f-args>)

" -- doc
command! -nargs=* -range -complete=customlist,go#package#Complete GoDoc call go#doc#Open('new', 'split', <f-args>)
command! -nargs=* -range -complete=customlist,go#package#Complete GoDocBrowser call go#doc#OpenBrowser(<f-args>)

" -- fmt
command! -nargs=0 GoFmt call go#fmt#Format(-1)
command! -nargs=0 GoImports call go#fmt#Format(1)

" -- import
command! -nargs=? -complete=customlist,go#package#Complete GoDrop call go#import#SwitchImport(0, '', <f-args>)
command! -nargs=1 -complete=customlist,go#package#Complete GoImport call go#import#SwitchImport(1, '', <f-args>)
command! -nargs=* -complete=customlist,go#package#Complete GoImportAs call go#import#SwitchImport(1, <f-args>)

" -- lint
command! GoLint call go#lint#Run()

" -- errcheck
command! -nargs=? -complete=customlist,go#package#Complete GoErrCheck call go#errcheck#Run(<f-args>)

" Disable all commands until they are fully integrated.
"
" command! -range=% GoOracleDescribe call go#oracle#Describe(<count>)
" command! -range=% GoOracleCallers call go#oracle#Callers(<count>)
" command! -range=% GoOracleCallgraph call go#oracle#Callgraph(<count>)
" command! -range=% GoOracleCallstack call go#oracle#Callstack(<count>)
" command! -range=% GoOracleFreevars call go#oracle#Freevars(<count>)
" command! -range=% GoOraclePeers call go#oracle#Peers(<count>)
" command! -range=% GoOracleReferrers call go#oracle#Referrers(<count>)

" vim:ts=4:sw=4:et
"

