if exists("b:did_ftplugin") || !exists("*FugitiveGitDir")
  finish
endif
let b:did_ftplugin = 1

call fugitive#BlameFileType()
