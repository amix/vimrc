VIMS ?= vim-7.4 vim-8.0 nvim

all: install test lint

install:
	@echo "==> Installing Vims: $(VIMS)"
	@for vim in $(VIMS); do \
		./scripts/install-vim $$vim; \
	done

test:
	@echo "==> Running tests for $(VIMS)"
	@for vim in $(VIMS); do \
		./scripts/test $$vim; \
	done

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
