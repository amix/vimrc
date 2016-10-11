all : emmet-vim.zip

remove-zip:
	-rm doc/tags
	-rm emmet-vim.zip

emmet-vim.zip: remove-zip
	zip -r emmet-vim.zip autoload plugin doc

release: emmet-vim.zip
	vimup update-script emmet.vim
