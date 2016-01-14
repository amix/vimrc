" gorename
command! -nargs=? GoRename call go#rename#Rename(<bang>0,<f-args>)

" oracle
command! -nargs=* -complete=customlist,go#package#Complete GoOracleScope call go#oracle#Scope(<f-args>)
command! -range=% GoImplements call go#oracle#Implements(<count>)
command! -range=% GoCallees call go#oracle#Callees(<count>)
command! -range=% GoDescribe call go#oracle#Describe(<count>)
command! -range=% GoCallers call go#oracle#Callers(<count>)
command! -range=% GoCallstack call go#oracle#Callstack(<count>)
command! -range=% GoFreevars call go#oracle#Freevars(<count>)
command! -range=% GoChannelPeers call go#oracle#ChannelPeers(<count>)
command! -range=% GoReferrers call go#oracle#Referrers(<count>)
command! -nargs=? GoOracleTags call go#oracle#Tags(<f-args>)

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
command! -nargs=? -complete=customlist,go#package#Complete GoDrop call go#import#SwitchImport(0, '', <f-args>, '')
command! -nargs=1 -bang -complete=customlist,go#package#Complete GoImport call go#import#SwitchImport(1, '', <f-args>, '<bang>')
command! -nargs=* -bang -complete=customlist,go#package#Complete GoImportAs call go#import#SwitchImport(1, <f-args>, '<bang>')

" -- linters
command! -nargs=* GoMetaLinter call go#lint#Gometa(0, <f-args>)
command! -nargs=* GoLint call go#lint#Golint(<f-args>)
command! -nargs=* -bang GoVet call go#lint#Vet(<bang>0, <f-args>)
command! -nargs=* -complete=customlist,go#package#Complete GoErrCheck call go#lint#Errcheck(<f-args>)

" vim:ts=4:sw=4:et
