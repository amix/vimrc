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
nnoremap <silent> <Plug>(go-run) :<C-u>call go#cmd#Run(!g:go_jump_to_error, '%')<CR>

if has("nvim")
	nnoremap <silent> <Plug>(go-run-vertical) :<C-u>call go#cmd#RunTerm('vsplit')<CR>
	nnoremap <silent> <Plug>(go-run-split) :<C-u>call go#cmd#RunTerm('split')<CR>
	nnoremap <silent> <Plug>(go-run-tab) :<C-u>call go#cmd#RunTerm('tab')<CR>
endif

nnoremap <silent> <Plug>(go-build) :<C-u>call go#cmd#Build(!g:go_jump_to_error)<CR>
nnoremap <silent> <Plug>(go-generate) :<C-u>call go#cmd#Generate(!g:go_jump_to_error)<CR>
nnoremap <silent> <Plug>(go-install) :<C-u>call go#cmd#Install(!g:go_jump_to_error)<CR>
nnoremap <silent> <Plug>(go-test) :<C-u>call go#cmd#Test(!g:go_jump_to_error, 0)<CR>
nnoremap <silent> <Plug>(go-test-func) :<C-u>call go#cmd#TestFunc(!g:go_jump_to_error)<CR>
nnoremap <silent> <Plug>(go-test-compile) :<C-u>call go#cmd#Test(!g:go_jump_to_error, 1)<CR>
nnoremap <silent> <Plug>(go-coverage) :<C-u>call go#cmd#Coverage(!g:go_jump_to_error)<CR>

nnoremap <silent> <Plug>(go-files) :<C-u>call go#tool#Files()<CR>
nnoremap <silent> <Plug>(go-deps) :<C-u>call go#tool#Deps()<CR>
nnoremap <silent> <Plug>(go-info) :<C-u>call go#complete#Info()<CR>
nnoremap <silent> <Plug>(go-import) :<C-u>call go#import#SwitchImport(1, '', expand('<cword>'), '')<CR>

nnoremap <silent> <Plug>(go-implements) :<C-u>call go#oracle#Implements(-1)<CR>
nnoremap <silent> <Plug>(go-callees) :<C-u>call go#oracle#Callees(-1)<CR>
nnoremap <silent> <Plug>(go-callers) :<C-u>call go#oracle#Callers(-1)<CR>
nnoremap <silent> <Plug>(go-describe) :<C-u>call go#oracle#Describe(-1)<CR>
nnoremap <silent> <Plug>(go-callstack) :<C-u>call go#oracle#Callstack(-1)<CR>
nnoremap <silent> <Plug>(go-freevars) :<C-u>call go#oracle#Freevars(-1)<CR>
nnoremap <silent> <Plug>(go-channelpeers) :<C-u>call go#oracle#ChannelPeers(-1)<CR>
nnoremap <silent> <Plug>(go-referrers) :<C-u>call go#oracle#Referrers(-1)<CR>

nnoremap <silent> <Plug>(go-rename) :<C-u>call go#rename#Rename(!g:go_jump_to_error)<CR>

nnoremap <silent> <Plug>(go-def) :<C-u>call go#def#Jump()<CR>
nnoremap <silent> <Plug>(go-def-vertical) :<C-u>call go#def#JumpMode("vsplit")<CR>
nnoremap <silent> <Plug>(go-def-split) :<C-u>call go#def#JumpMode("split")<CR>
nnoremap <silent> <Plug>(go-def-tab) :<C-u>call go#def#JumpMode("tab")<CR>

nnoremap <silent> <Plug>(go-doc) :<C-u>call go#doc#Open("new", "split")<CR>
nnoremap <silent> <Plug>(go-doc-tab) :<C-u>call go#doc#Open("tabnew", "tabe")<CR>
nnoremap <silent> <Plug>(go-doc-vertical) :<C-u>call go#doc#Open("vnew", "vsplit")<CR>
nnoremap <silent> <Plug>(go-doc-split) :<C-u>call go#doc#Open("new", "split")<CR>
nnoremap <silent> <Plug>(go-doc-browser) :<C-u>call go#doc#OpenBrowser()<CR>

nnoremap <silent> <Plug>(go-metalinter) :<C-u>call go#lint#Gometa(0)<CR>
nnoremap <silent> <Plug>(go-vet) :<C-u>call go#lint#Vet(!g:go_jump_to_error)<CR>


