all : gist-vim.zip

remove-zip:
	-rm -f doc/tags
	-rm -f gist-vim.zip

gist-vim.zip: remove-zip
	zip -r gist-vim.zip autoload plugin doc README.mkd

release: gist-vim.zip
	vimup update-script gist.vim
