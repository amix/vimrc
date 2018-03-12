VIMDIR=$(DESTDIR)/usr/share/vim
ADDONS=${VIMDIR}/addons
REGISTRY=${VIMDIR}/registry

all:

install:
	mkdir -pv ${ADDONS}/ftdetect
	cp -v ftdetect/markdown.vim ${ADDONS}/ftdetect/markdown.vim
	mkdir -pv ${ADDONS}/ftplugin
	cp -v ftplugin/markdown.vim ${ADDONS}/ftplugin/markdown.vim
	mkdir -pv ${ADDONS}/syntax
	cp -v syntax/markdown.vim ${ADDONS}/syntax/markdown.vim
	mkdir -pv ${ADDONS}/after/ftplugin
	cp -v after/ftplugin/markdown.vim ${ADDONS}/after/ftplugin/markdown.vim
	mkdir -pv ${ADDONS}/indent
	cp -v indent/markdown.vim ${ADDONS}/indent/markdown.vim
	mkdir -pv ${ADDONS}/doc
	cp -v doc/vim-markdown.txt ${ADDONS}/doc/vim-markdown.txt
	mkdir -pv ${REGISTRY}
	cp -v registry/markdown.yaml ${REGISTRY}/markdown.yaml

test: build/tabular build/vim-toml build/vim-json build/vader.vim
	test/run-tests.sh
.PHONY: test

update: build/tabular build/vim-toml build/vim-json build/vader.vim
	cd build/tabular && git pull
	cd build/vim-toml && git pull
	cd build/vim-json && git pull
	cd build/vader.vim && git pull
.PHONY: update

build/tabular: | build
	git clone https://github.com/godlygeek/tabular build/tabular

build/vim-toml: | build
	git clone https://github.com/cespare/vim-toml build/vim-toml

build/vim-json: | build
	git clone https://github.com/elzr/vim-json build/vim-json

build/vader.vim: | build
	git clone https://github.com/junegunn/vader.vim build/vader.vim

build:
	mkdir build

doc: build/html2vimdoc build/vim-tools
	sed -e '/^\[!\[Build Status\]/d' \
	    -e '/^1\. \[/d' README.md > doc/tmp.md # remove table of contents
	build/html2vimdoc/bin/python build/vim-tools/html2vimdoc.py -f vim-markdown \
		doc/tmp.md | \
		sed -E -e "s/[[:space:]]*$$//" -e "# remove trailing spaces" \
		    -e "/^.{79,}\|$$/ {" -e "# wrap table of contents over 79" \
		    -e "h" -e "# save the matched line to the hold space" \
		    -e "s/^(.*) (\|[^|]*\|)$$/\1/" -e "# make content title" \
		    -e "p" -e "# print title" \
		    -e "g" -e "# restore the matched line" \
		    -e "s/^.* (\|[^|]*\|)$$/ \1/" -e "# make link" \
		    -e ":c" -e "s/^(.{1,78})$$/ \1/" -e "tc" -e "# align right" \
		    -e "}" \
		    -e "/^- '[^']*':( |$$)/ {" \
		    -e "h" -e "# save the matched line to the hold space" \
		    -e "s/^- '([^']{3,})':.*/ \*\1\*/" -e "# make command reference" \
		    -e "s/^- '([^']{1,2})':.*/ \*vim-markdown-\1\*/" -e "# short command" \
		    -e ":a" -e "s/^(.{1,78})$$/ \1/" -e "ta" -e "# align right" \
		    -e "G" -e "# append the matched line after the command reference" \
		    -e "}" > doc/vim-markdown.txt && rm -f doc/tmp.md

.PHONY: doc

# Prerequire Python and virtualenv.
# $ sudo pip install virtualenv
# Create the virtual environment.
# Install the dependencies.
build/html2vimdoc: | build
	virtualenv build/html2vimdoc
	build/html2vimdoc/bin/pip install beautifulsoup coloredlogs==4.0 markdown

build/vim-tools: | build
	git clone https://github.com/xolox/vim-tools.git build/vim-tools
