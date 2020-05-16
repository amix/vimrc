syntax region  jsFlowDefinition     contained                        start=/:/    end=/\%(\s*[,=;)\n]\)\@=/ contains=@jsFlowCluster containedin=jsParen
syntax region  jsFlowArgumentDef    contained                        start=/:/    end=/\%(\s*[,)]\|=>\@!\)\@=/ contains=@jsFlowCluster
syntax region  jsFlowArray          contained matchgroup=jsFlowNoise start=/\[/   end=/\]/        contains=@jsFlowCluster,jsComment fold
syntax region  jsFlowObject         contained matchgroup=jsFlowNoise start=/{/    end=/}/         contains=@jsFlowCluster,jsComment fold
syntax region  jsFlowExactObject    contained matchgroup=jsFlowNoise start=/{|/   end=/|}/       contains=@jsFlowCluster,jsComment fold
syntax region  jsFlowParens         contained matchgroup=jsFlowNoise start=/(/  end=/)/ contains=@jsFlowCluster nextgroup=jsFlowArrow skipwhite keepend extend fold
syntax match   jsFlowNoise          contained /[:;,<>]/
syntax keyword jsFlowType           contained boolean number string null void any mixed JSON array Function object array bool class
syntax keyword jsFlowTypeof         contained typeof skipempty skipwhite nextgroup=jsFlowTypeCustom,jsFlowType
syntax match   jsFlowTypeCustom     contained /[0-9a-zA-Z_.]*/ skipwhite skipempty nextgroup=jsFlowGeneric
syntax region  jsFlowGeneric                  matchgroup=jsFlowNoise start=/\k\@<=</ end=/>/ contains=@jsFlowCluster containedin=@jsExpression,jsFlowDeclareBlock
syntax region  jsFlowGeneric        contained matchgroup=jsFlowNoise start=/</ end=/>(\@=/ oneline contains=@jsFlowCluster containedin=@jsExpression,jsFlowDeclareBlock
syntax region  jsFlowObjectGeneric  contained matchgroup=jsFlowNoise start=/\k\@<=</ end=/>/ contains=@jsFlowCluster nextgroup=jsFuncArgs
syntax match   jsFlowArrow          contained /=>/ skipwhite skipempty nextgroup=jsFlowType,jsFlowTypeCustom,jsFlowParens
syntax match   jsFlowObjectKey      contained /[0-9a-zA-Z_$?]*\(\s*:\)\@=/ contains=jsFunctionKey,jsFlowMaybe skipwhite skipempty nextgroup=jsObjectValue containedin=jsObject
syntax match   jsFlowOrOperator     contained /|/ skipwhite skipempty nextgroup=@jsFlowCluster
syntax keyword jsFlowImportType     contained type typeof skipwhite skipempty nextgroup=jsModuleAsterisk,jsModuleKeyword,jsModuleGroup
syntax match   jsFlowWildcard       contained /*/

syntax match   jsFlowReturn         contained /:\s*/ contains=jsFlowNoise skipwhite skipempty nextgroup=@jsFlowReturnCluster,jsFlowArrow,jsFlowReturnParens
syntax region  jsFlowReturnObject   contained matchgroup=jsFlowNoise start=/{/    end=/}/  contains=@jsFlowCluster skipwhite skipempty nextgroup=jsFuncBlock,jsFlowReturnOrOp extend fold
syntax region  jsFlowReturnArray    contained matchgroup=jsFlowNoise start=/\[/   end=/\]/ contains=@jsFlowCluster skipwhite skipempty nextgroup=jsFuncBlock,jsFlowReturnOrOp fold
syntax region  jsFlowReturnParens   contained matchgroup=jsFlowNoise start=/(/    end=/)/  contains=@jsFlowCluster skipwhite skipempty nextgroup=jsFuncBlock,jsFlowReturnOrOp,jsFlowReturnArrow fold
syntax match   jsFlowReturnArrow    contained /=>/ skipwhite skipempty nextgroup=@jsFlowReturnCluster
syntax match   jsFlowReturnKeyword  contained /\k\+/ contains=jsFlowType,jsFlowTypeCustom skipwhite skipempty nextgroup=jsFlowReturnGroup,jsFuncBlock,jsFlowReturnOrOp,jsFlowReturnArray
syntax match   jsFlowReturnMaybe    contained /?/ skipwhite skipempty nextgroup=jsFlowReturnKeyword,jsFlowReturnObject,jsFlowReturnParens
syntax region  jsFlowReturnGroup    contained matchgroup=jsFlowNoise start=/</ end=/>/ contains=@jsFlowCluster skipwhite skipempty nextgroup=jsFuncBlock,jsFlowReturnOrOp
syntax match   jsFlowReturnOrOp     contained /\s*|\s*/ skipwhite skipempty nextgroup=@jsFlowReturnCluster
syntax match   jsFlowWildcardReturn contained /*/ skipwhite skipempty nextgroup=jsFuncBlock
syntax keyword jsFlowTypeofReturn   contained typeof skipempty skipwhite nextgroup=@jsFlowReturnCluster

syntax region  jsFlowFunctionGroup      contained matchgroup=jsFlowNoise start=/</ end=/>/ contains=@jsFlowCluster skipwhite skipempty nextgroup=jsFuncArgs
syntax region  jsFlowClassGroup         contained matchgroup=jsFlowNoise start=/</ end=/>/ contains=@jsFlowCluster skipwhite skipempty nextgroup=jsClassBlock
syntax region  jsFlowClassFunctionGroup contained matchgroup=jsFlowNoise start=/</ end=/>/ contains=@jsFlowCluster skipwhite skipempty nextgroup=jsFuncArgs
syntax match   jsFlowObjectFuncName contained /\<\K\k*<\@=/ skipwhite skipempty nextgroup=jsFlowObjectGeneric containedin=jsObject

syntax region  jsFlowTypeStatement                                   start=/\(opaque\s\+\)\?type\%(\s\+\k\)\@=/    end=/=\@=/ contains=jsFlowTypeOperator oneline skipwhite skipempty nextgroup=jsFlowTypeValue keepend
syntax region  jsFlowTypeValue      contained     matchgroup=jsFlowNoise start=/=/ end=/\%(;\|\n\%(\s*|\)\@!\)/ contains=@jsFlowCluster,jsFlowGeneric,jsFlowMaybe
syntax match   jsFlowTypeOperator   contained /=/ containedin=jsFlowTypeValue
syntax match   jsFlowTypeOperator   contained /=/
syntax keyword jsFlowTypeKeyword    contained type

syntax keyword jsFlowDeclare                  declare skipwhite skipempty nextgroup=jsFlowTypeStatement,jsClassDefinition,jsStorageClass,jsFlowModule,jsFlowInterface
syntax match   jsFlowClassProperty  contained /\<[0-9a-zA-Z_$]*\>:\@=/ skipwhite skipempty nextgroup=jsFlowClassDef containedin=jsClassBlock
syntax region  jsFlowClassDef       contained start=/:/    end=/\%(\s*[,=;)\n]\)\@=/ contains=@jsFlowCluster skipwhite skipempty nextgroup=jsClassValue

syntax region  jsFlowModule         contained start=/module/ end=/\%({\|:\)\@=/ skipempty skipwhite nextgroup=jsFlowDeclareBlock contains=jsString
syntax region  jsFlowInterface      contained start=/interface/ end=/{\@=/ skipempty skipwhite nextgroup=jsFlowInterfaceBlock contains=@jsFlowCluster
syntax region  jsFlowDeclareBlock   contained matchgroup=jsFlowNoise start=/{/ end=/}/ contains=jsFlowDeclare,jsFlowNoise fold

syntax match   jsFlowMaybe          contained /?/
syntax region  jsFlowInterfaceBlock contained matchgroup=jsFlowNoise start=/{/ end=/}/ contains=jsObjectKey,jsObjectKeyString,jsObjectKeyComputed,jsObjectSeparator,jsObjectFuncName,jsFlowObjectFuncName,jsObjectMethodType,jsGenerator,jsComment,jsObjectStringKey,jsSpreadExpression,jsFlowNoise,jsFlowParens,jsFlowGeneric keepend fold

syntax region  jsFlowParenAnnotation contained start=/:/ end=/[,=)]\@=/ containedin=jsParen contains=@jsFlowCluster

syntax cluster jsFlowReturnCluster            contains=jsFlowNoise,jsFlowReturnObject,jsFlowReturnArray,jsFlowReturnKeyword,jsFlowReturnGroup,jsFlowReturnMaybe,jsFlowReturnOrOp,jsFlowWildcardReturn,jsFlowReturnArrow,jsFlowTypeofReturn
syntax cluster jsFlowCluster                  contains=jsFlowArray,jsFlowObject,jsFlowExactObject,jsFlowNoise,jsFlowTypeof,jsFlowType,jsFlowGeneric,jsFlowMaybe,jsFlowParens,jsFlowOrOperator,jsFlowWildcard

if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink jsFlowDefinition         PreProc
  HiLink jsFlowClassDef           jsFlowDefinition
  HiLink jsFlowArgumentDef        jsFlowDefinition
  HiLink jsFlowType               Type
  HiLink jsFlowTypeCustom         PreProc
  HiLink jsFlowTypeof             PreProc
  HiLink jsFlowTypeofReturn       PreProc
  HiLink jsFlowArray              PreProc
  HiLink jsFlowObject             PreProc
  HiLink jsFlowExactObject        PreProc
  HiLink jsFlowParens             PreProc
  HiLink jsFlowGeneric            PreProc
  HiLink jsFlowObjectGeneric      jsFlowGeneric
  HiLink jsFlowReturn             PreProc
  HiLink jsFlowParenAnnotation    PreProc
  HiLink jsFlowReturnObject       jsFlowReturn
  HiLink jsFlowReturnArray        jsFlowArray
  HiLink jsFlowReturnParens       jsFlowParens
  HiLink jsFlowReturnGroup        jsFlowGeneric
  HiLink jsFlowFunctionGroup      PreProc
  HiLink jsFlowClassGroup         PreProc
  HiLink jsFlowClassFunctionGroup PreProc
  HiLink jsFlowArrow              PreProc
  HiLink jsFlowReturnArrow        PreProc
  HiLink jsFlowTypeStatement      PreProc
  HiLink jsFlowTypeKeyword        PreProc
  HiLink jsFlowTypeOperator       Operator
  HiLink jsFlowMaybe              PreProc
  HiLink jsFlowReturnMaybe        PreProc
  HiLink jsFlowClassProperty      jsClassProperty
  HiLink jsFlowDeclare            PreProc
  HiLink jsFlowModule             PreProc
  HiLink jsFlowInterface          PreProc
  HiLink jsFlowNoise              Noise
  HiLink jsFlowObjectKey          jsObjectKey
  HiLink jsFlowOrOperator         jsOperator
  HiLink jsFlowReturnOrOp         jsFlowOrOperator
  HiLink jsFlowWildcard           PreProc
  HiLink jsFlowWildcardReturn     PreProc
  HiLink jsFlowImportType         PreProc
  HiLink jsFlowTypeValue          PreProc
  HiLink jsFlowObjectFuncName     jsObjectFuncName
  delcommand HiLink
endif
