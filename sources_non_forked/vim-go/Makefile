VIMS ?= vim-8.0 vim-8.2 nvim
TEST_FLAGS ?=

all: install lint test

install:
	@echo "==> Installing Vims: $(VIMS)"
	@for vim in $(VIMS); do \
		./scripts/install-vim $$vim; \
		./scripts/install-tools $$vim; \
	done

test:
	@echo "==> Running tests for $(VIMS)"
	@for vim in $(VIMS); do \
		./scripts/test $(TEST_FLAGS) $$vim; \
	done

lint:
	@echo "==> Running linting tools"
	@./scripts/lint vim-8.2

docker:
	@echo "==> Building/starting Docker container"
	@./scripts/docker-test

clean:
	@echo "==> Cleaning /tmp/vim-go-test"
	@rm -rf /tmp/vim-go-test

.PHONY: all test install clean lint docker
