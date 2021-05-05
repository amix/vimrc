if exists("b:did_ftplugin") || !exists('*fugitive#BlameFileType')
  finish
endif
let b:did_ftplugin = 1

call fugitive#BlameFileType()
