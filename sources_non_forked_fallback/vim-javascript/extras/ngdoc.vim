syntax match  jsDocTags         contained /@\(link\|method[oO]f\|ngdoc\|ng[iI]nject\|restrict\)/ nextgroup=jsDocParam skipwhite
syntax match  jsDocType         contained "\%(#\|\$\|\w\|\"\|-\|\.\|:\|\/\)\+" nextgroup=jsDocParam skipwhite
syntax match  jsDocParam        contained "\%(#\|\$\|\w\|\"\|-\|\.\|:\|{\|}\|\/\|\[\|]\|=\)\+"
