"" syntax coloring for javadoc comments (HTML)
syntax region jsComment    matchgroup=jsComment start="/\*\s*"  end="\*/" contains=jsDocTags,jsCommentTodo,jsCvsTag,@jsHtml,@Spell fold

" tags containing a param
syntax match  jsDocTags         contained "@\(alias\|api\|augments\|borrows\|class\|constructs\|default\|defaultvalue\|emits\|exception\|exports\|extends\|fires\|kind\|link\|listens\|member\|member[oO]f\|mixes\|module\|name\|namespace\|requires\|template\|throws\|var\|variation\|version\)\>" skipwhite nextgroup=jsDocParam
" tags containing type and param
syntax match  jsDocTags         contained "@\(arg\|argument\|cfg\|param\|property\|prop\|typedef\)\>" skipwhite nextgroup=jsDocType
" tags containing type but no param
syntax match  jsDocTags         contained "@\(callback\|define\|enum\|external\|implements\|this\|type\|return\|returns\|yields\)\>" skipwhite nextgroup=jsDocTypeNoParam
" tags containing references
syntax match  jsDocTags         contained "@\(lends\|see\|tutorial\)\>" skipwhite nextgroup=jsDocSeeTag
" other tags (no extra syntax)
syntax match  jsDocTags         contained "@\(abstract\|access\|accessor\|async\|author\|classdesc\|constant\|const\|constructor\|copyright\|deprecated\|desc\|description\|dict\|event\|example\|file\|file[oO]verview\|final\|function\|global\|ignore\|inherit[dD]oc\|inner\|instance\|interface\|license\|localdoc\|method\|mixin\|nosideeffects\|override\|overview\|preserve\|private\|protected\|public\|readonly\|since\|static\|struct\|todo\|summary\|undocumented\|virtual\)\>"

syntax region jsDocType         contained matchgroup=jsDocTypeBrackets start="{" end="}" contains=jsDocTypeRecord oneline skipwhite nextgroup=jsDocParam
syntax match  jsDocType         contained "\%(#\|\"\|\w\|\.\|:\|\/\)\+" skipwhite nextgroup=jsDocParam
syntax region jsDocTypeRecord   contained start=/{/ end=/}/ contains=jsDocTypeRecord extend
syntax region jsDocTypeRecord   contained start=/\[/ end=/\]/ contains=jsDocTypeRecord extend
syntax region jsDocTypeNoParam  contained start="{" end="}" oneline
syntax match  jsDocTypeNoParam  contained "\%(#\|\"\|\w\|\.\|:\|\/\)\+"
syntax match  jsDocParam        contained "\%(#\|\$\|-\|'\|\"\|{.\{-}}\|\w\|\.\|:\|\/\|\[.\{-}]\|=\)\+"
syntax region jsDocSeeTag       contained matchgroup=jsDocSeeTag start="{" end="}" contains=jsDocTags

if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink jsDocTags              Special
  HiLink jsDocSeeTag            Function
  HiLink jsDocType              Type
  HiLink jsDocTypeBrackets      jsDocType
  HiLink jsDocTypeRecord        jsDocType
  HiLink jsDocTypeNoParam       Type
  HiLink jsDocParam             Label
  delcommand HiLink
endif
