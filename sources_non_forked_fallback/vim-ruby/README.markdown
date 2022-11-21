## Vim-ruby

This project contains Vim's runtime files for ruby support. This includes syntax
highlighting, indentation, omnicompletion, and various useful tools and mappings.

## Installation

See the file [INSTALL.markdown](./INSTALL.markdown) for instructions.

You might also find useful setup tips in the github wiki:
https://github.com/vim-ruby/vim-ruby/wiki/VimRubySupport

## Usage

Ideally, vim-ruby should work "correctly" for you out of the box. However, ruby
developers have varying preferences, so there are settings that control some of
the details. You can get more information on these by using the native `:help`
command:

- [`:help vim-ruby-plugin`](./doc/ft-ruby-plugin.txt): Filetype settings and custom mappings
- [`:help vim-ruby-indent`](./doc/ft-ruby-indent.txt): Indentation settings
- [`:help vim-ruby-syntax`](./doc/ft-ruby-syntax.txt): Syntax-related tweaks
- [`:help vim-ruby-omni`](./doc/ft-ruby-omni.txt): Information and settings for omni completion

## Issues

If you have an issue or a feature request, it's recommended to use the github
issue tracker: https://github.com/vim-ruby/vim-ruby/issues. Try the search box
to look for an existing issue -- it might have already been reported.

If you don't have a github account or would rather contact us in a different
way, you can find emails for individual maintainers in the
[CONTRIBUTORS](./CONTRIBUTORS) file. They're also in the comment headers of the
project's Vimscript files (`syntax/ruby.vim`, `indent/ruby.vim`, etc) under the
label "Maintainer".

If you're not sure who the most relevant person to contact is for your
particular issue, you can send an email to the release coordinator, Doug Kearns
(dougkearns at gmail.com).

## Contributing

Vim-ruby is a mature project, which is one way of saying it moves slowly and it
can be a bit difficult to modify. It's far from impossible, but be warned that
issues and PRs may take time to be handled. Partly, it's because we don't want
to risk breaking Vim's core ruby support, partly because it takes a lot of time
and energy to debug and fix things.

Contributing a fix for an issue would be very appreciated, even if it's a
proof-of-concept to start a conversation. Be warned that we're definitely going
to be conservative when considering changes to vim-ruby.

The code is tested using [RSpec](https://rspec.info/) and
[Vimrunner](https://github.com/AndrewRadev/vimrunner). The tests are not
exhaustive, but they should cover a wide variety of cases.

## Project history

This project began in July 2003, when the current version of Vim was 6.2. It
was migrated from CVS in August, 2008.

If you're curious about individual pre-git changes, you can read some of them
in the (unmaintained) [NEWS](./NEWS) and/or [ChangeLog](./ChangeLog) files.
