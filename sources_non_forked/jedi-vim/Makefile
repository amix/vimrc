BUILD_VIRTUAL_ENV:=build/venv

test:
	pytest

test_nvim:
	VSPEC_VIM=nvim pytest

test_coverage: export PYTEST_ADDOPTS:=--cov pythonx --cov test --cov-report=term-missing:skip-covered
test_coverage: test_nvim

$(dir $(BUILD_VIRTUAL_ENV)):
	mkdir -p $@

$(BUILD_VIRTUAL_ENV): | $(dir $(BUILD_VIRTUAL_ENV))
	python -m venv $@

$(BUILD_VIRTUAL_ENV)/bin/vint: | $(BUILD_VIRTUAL_ENV)
	$|/bin/python -m pip install vim-vint==0.3.21

$(BUILD_VIRTUAL_ENV)/bin/flake8: | $(BUILD_VIRTUAL_ENV)
	$|/bin/python -m pip install -q flake8==3.7.8

vint: $(BUILD_VIRTUAL_ENV)/bin/vint
	$(BUILD_VIRTUAL_ENV)/bin/vint after autoload ftplugin plugin

flake8: $(BUILD_VIRTUAL_ENV)/bin/flake8
	$(BUILD_VIRTUAL_ENV)/bin/flake8 pythonx/jedi_*.py

check: vint flake8

clean:
	rm -rf build

.PHONY: test check clean vint flake8
