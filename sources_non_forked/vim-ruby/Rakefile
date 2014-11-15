require 'rubygems'
require 'rake/gempackagetask'

PACKAGE_NAME = 'vim-ruby'
RELEASE_FILES = FileList[
  'ChangeLog', 'CONTRIBUTORS', 'FAQ', 'INSTALL', 'NEWS', 'README', 'bin/*.rb',
  'doc/*.txt','{autoload,compiler,ftdetect,ftplugin,indent,syntax}/*.vim'
]
PACKAGE_VERSION = Time.now.gmtime.strftime('%Y.%m.%d')

desc "Build all the packages"
task :default => :package


def gemspec
  Gem::Specification.new do |s|
    s.name                  = PACKAGE_NAME
    s.version               = PACKAGE_VERSION
    s.files                 = RELEASE_FILES.to_a
    s.summary               = "Ruby configuration files for Vim.  Run 'vim-ruby-install.rb' to complete installation."
    s.description           = s.summary + "\n\nThis package doesn't contain a Ruby library."
    s.requirements          << 'RubyGems 0.8+' << 'Vim 6.0+'
    s.required_ruby_version = '>= 1.8.0'
    s.require_path          = '.'
    s.bindir                = 'bin'
    s.executables           = ['vim-ruby-install.rb']
    s.author                = 'Gavin Sinclair et al.'
    s.email                 = 'gsinclair@soyabean.com.au'
    s.homepage              = 'https://github.com/vim-ruby/vim-ruby'
    s.has_rdoc              = false
  end
end

Rake::GemPackageTask.new(gemspec) do |t|
  t.package_dir = 'etc/package'
  t.need_tar = true
  t.need_zip = true
end

# Supporting methods

# vim: nowrap sw=2 sts=2 ts=8 ff=unix ft=ruby:
