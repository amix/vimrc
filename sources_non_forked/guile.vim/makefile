VIM = nvim

.PHONY: check

check:
	@VADER_OUTPUT_FILE=/dev/stdout $(VIM) --headless -c 'Vader! test/*.vader'
