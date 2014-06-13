NAME = node
TITLE = Node.vim
VERSION = 0.8.1
ID = 4674
TEST_OPTS =

love:
	@echo "Feel like makin' love."

test: spec
autotest: autospec

spec: $(shell find . -name "*_test.rb")
	@ruby -rbundler/setup $(addprefix -r./,$^) -e "" -- $(TEST_OPTS)

autospec:
	@bundle exec guard start --no-interactions

pack:
	rm -rf "$(NAME)-$(VERSION).zip"
	zip -r "$(NAME)-$(VERSION).zip" * -x @.packignore

publish:
	open "http://www.vim.org/scripts/add_script_version.php?script_id=$(ID)"

tag:
	git tag "v$(VERSION)"

node.tar.gz:
	wget -c "https://github.com/joyent/node/archive/master.tar.gz" -O node.tar.gz

list-core-modules: node.tar.gz
	tar tf node.tar.gz |\
	egrep "^node[^/]*/lib/.+" |\
	xargs -n1 basename -s .js |\
	{ cat; echo node; } | sort
	
.PHONY: love
.PHONY: spec autospec
.PHONY: pack publish tag
