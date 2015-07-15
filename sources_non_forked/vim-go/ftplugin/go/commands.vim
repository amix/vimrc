if exists("g:go_loaded_commands")
    finish
endif
let g:go_loaded_commands = 1

" go_jump_to_error defines whether we should pass the bang attribute to the
" command or not. This is only used for mappings, because the user can't pass
" the bang attribute to the plug mappings below. So instead of hardcoding it
" as 0 (no '!' attribute) or 1 (with '!' attribute) we pass the user setting,
" which by default is enabled. For commands the user has the ability to pass
" the '!', such as :GoBuild or :GoBuild!
if !exists("g:go_jump_to_error")
    let g:go_jump_to_error = 1
endif


" Some handy plug mappings
nnoremap <silent> <Plug>(go-run) :<C-u>call go#cmd#Run(!g:go_jump_to_error,expand('%'))<CR>
nnoremap <silent> <Plug>(go-build) :<C-u>call go#cmd#Build(!g:go_jump_to_error,'')<CR>
nnoremap <silent> <Plug>(go-generate) :<C-u>call go#cmd#Generate(!g:go_jump_to_error,'')<CR>
nnoremap <silent> <Plug>(go-install) :<C-u>call go#cmd#Install(!g:go_jump_to_error)<CR>
nnoremap <silent> <Plug>(go-test) :<C-u>call go#cmd#Test(!g:go_jump_to_error, 0, '')<CR>
nnoremap <silent> <Plug>(go-test-func) :<C-u>call go#cmd#TestFunc(!g:go_jump_to_error, '')<CR>
nnoremap <silent> <Plug>(go-test-compile) :<C-u>call go#cmd#Test(!g:go_jump_to_error, 1, '')<CR>
nnoremap <silent> <Plug>(go-coverage) :<C-u>call go#cmd#Coverage(!g:go_jump_to_error, '')<CR>
nnoremap <silent> <Plug>(go-vet) :<C-u>call go#cmd#Vet(!g:go_jump_to_error)<CR>

nnoremap <silent> <Plug>(go-files) :<C-u>call go#tool#Files()<CR>
nnoremap <silent> <Plug>(go-deps) :<C-u>call go#tool#Deps()<CR>
nnoremap <silent> <Plug>(go-info) :<C-u>call go#complete#Info()<CR>
nnoremap <silent> <Plug>(go-import) :<C-u>call go#import#SwitchImport(1, '', expand('<cword>'))<CR>

nnoremap <silent> <Plug>(go-implements) :<C-u>call go#oracle#Implements(-1)<CR>
nnoremap <silent> <Plug>(go-callees) :<C-u>call go#oracle#Callees(-1)<CR>
nnoremap <silent> <Plug>(go-callers) :<C-u>call go#oracle#Callers(-1)<CR>
nnoremap <silent> <Plug>(go-describe) :<C-u>call go#oracle#Describe(-1)<CR>
nnoremap <silent> <Plug>(go-callstack) :<C-u>call go#oracle#Callstack(-1)<CR>
nnoremap <silent> <Plug>(go-freevars) :<C-u>call go#oracle#Freevars(-1)<CR>
nnoremap <silent> <Plug>(go-channelpeers) :<C-u>call go#oracle#ChannelPeers(-1)<CR>
nnoremap <silent> <Plug>(go-referrers) :<C-u>call go#oracle#Referrers(-1)<CR>

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
command! -range=% GoDescribe call go#oracle#Describe(<count>)
command! -range=% GoCallers call go#oracle#Callers(<count>)
command! -range=% GoCallstack call go#oracle#Callstack(<count>)
command! -range=% GoFreevars call go#oracle#Freevars(<count>)
command! -range=% GoChannelPeers call go#oracle#ChannelPeers(<count>)
command! -range=% GoReferrers call go#oracle#Referrers(<count>)

command! -nargs=* -complete=customlist,go#package#Complete GoOracleScope call go#oracle#Scope(<f-args>)

" tool
command! -nargs=0 GoFiles echo go#tool#Files()
command! -nargs=0 GoDeps echo go#tool#Deps()
command! -nargs=* GoInfo call go#complete#Info()

" cmd
command! -nargs=* -bang GoBuild call go#cmd#Build(<bang>0,<f-args>)
command! -nargs=* -bang GoGenerate call go#cmd#Generate(<bang>0,<f-args>)
command! -nargs=* -bang GoRun call go#cmd#Run(<bang>0,<f-args>)
command! -nargs=* -bang GoInstall call go#cmd#Install(<bang>0, <f-args>)
command! -nargs=* -bang GoTest call go#cmd#Test(<bang>0, 0, <f-args>)
command! -nargs=* -bang GoTestFunc call go#cmd#TestFunc(<bang>0, <f-args>)
command! -nargs=* -bang GoTestCompile call go#cmd#Test(<bang>0, 1, <f-args>)
command! -nargs=* -bang GoCoverage call go#cmd#Coverage(<bang>0, <f-args>)
command! -nargs=0 -bang GoVet call go#cmd#Vet(<bang>0)

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

" vim:ts=4:sw=4:et
