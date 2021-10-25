map <c-]> g<c-]>
set completeopt=menu,menuone  
let OmniCpp_MayCompleteDot=1    "  打开  . 操作符
let OmniCpp_MayCompleteArrow=1  " 打开 -> 操作符
let OmniCpp_MayCompleteScope=1  " 打开 :: 操作符
let OmniCpp_NamespaceSearch=1   " 打开命名空间
let OmniCpp_GlobalScopeSearch=1  
let OmniCpp_DefaultNamespace=["std"]  
let OmniCpp_ShowPrototypeInAbbr=1  " 打开显示函数原型
let OmniCpp_SelectFirstItem = 2 " 自动弹出时自动跳至第一个
autocmd BufRead scp://* :set bt=acwrite

" 添加自动补全字典
au FileType php setlocal dict+=~/.vim_runtime/dictionary/php_keywords_list.txt
au FileType cpp setlocal dict+=~/.vim_runtime/dictionary/cpp_keywords_list.txt
au FileType java setlocal dict+=~/.vim_runtime/dictionary/java_keywords_list.txt
" au FileType markdown setlocal dict+=~/.vim/dictionary/words.txt
