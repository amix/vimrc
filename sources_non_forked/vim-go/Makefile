all: install test lint

install:
	@echo "==> Installing Vims"
	@./scripts/install-vim vim-7.4
	@./scripts/install-vim vim-8.0
	@./scripts/install-vim nvim

test:
	@echo "==> Running tests"
	@./scripts/test vim-7.4
	@./scripts/test vim-8.0
	@./scripts/test nvim

lint:
	@echo "==> Running linting tools"
	@./scripts/lint vim-8.0

docker:
	@echo "==> Building/starting Docker container"
	@./scripts/docker-test

clean:
	@echo "==> Cleaning /tmp/vim-go-test"
	@rm -rf /tmp/vim-go-test


.PHONY: all test install clean lint docker
