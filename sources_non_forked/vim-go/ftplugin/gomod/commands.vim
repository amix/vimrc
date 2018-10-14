command! -nargs=0 -range GoModFmt call go#mod#Format()

command! -nargs=0 GoModFmtAutoSaveToggle call go#mod#ToggleModFmtAutoSave()
