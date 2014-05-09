#require 'fileutils'
#include FileUtils

namespace :snippets_dir do
  task :find do
    vim_dir = File.join(ENV['VIMFILES'] || ENV['HOME'] || ENV['USERPROFILE'], RUBY_PLATFORM =~ /mswin|msys|mingw32/ ? "vimfiles" : ".vim")
    pathogen_dir = File.join(vim_dir, "bundle")
    @snippets_dir = File.directory?(pathogen_dir) ? File.join(pathogen_dir, "snipmate", "snippets") : File.join(vim_dir, "snippets")
  end

  desc "Purge the contents of the vim snippets directory"
  task :purge => ["snippets_dir:find"] do
    rm_rf @snippets_dir, :verbose => true if File.directory? @snippets_dir
    mkdir @snippets_dir, :verbose => true
  end
end

desc "Copy the snippets directories into ~/.vim/snippets"
task :deploy_local => ["snippets_dir:purge"] do
  Dir.foreach(".") do |f|
    cp_r f, @snippets_dir, :verbose => true if File.directory?(f) && f =~ /^[^\.]/
  end
  cp "support_functions.vim", @snippets_dir, :verbose => true
end
