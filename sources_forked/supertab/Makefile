SHELL=/bin/bash

all: dist

dist:
	@rm supertab.vmb 2> /dev/null || true
	@vim -c 'r! git ls-files doc plugin' \
		-c '$$,$$d _' -c '%MkVimball supertab .' -c 'q!'

clean:
	@rm -R build 2> /dev/null || true

install: supertab.vmb
	vim $< -c 'so %' -c 'q'

uninstall:
	vim -c 'RmVimball supertab.vmb' -c 'q'
