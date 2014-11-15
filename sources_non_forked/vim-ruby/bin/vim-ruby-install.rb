#!/usr/bin/env ruby

# vim-ruby-install: install the Vim config files for Ruby editing
#
#  * scope out the target directory and get user to confirm
#    * if no directory found, ask user
#    * allow user to force a search for a Windows gvim installation
#  * find source files from gem or from top level directory
#  * copy to target directory, taking account of
#    * line endings (NL for Unix-ish; CRLF for Windows)
#    * permissions (755 for directories; 644 for files)
#

require 'rbconfig'
include RbConfig
require 'fileutils'
require 'optparse'
require 'pathname'

SOURCE_FILES = %w{
  autoload/rubycomplete.vim
  compiler/eruby.vim
  compiler/rspec.vim
  compiler/ruby.vim
  compiler/rubyunit.vim
  ftdetect/ruby.vim
  ftplugin/eruby.vim
  ftplugin/ruby.vim
  indent/eruby.vim
  indent/ruby.vim
  syntax/eruby.vim
  syntax/ruby.vim
}

#
# Miscellaneous functions in the user's environment.
#
class Env
  #
  # Returns :UNIX or :WINDOWS, according to CONFIG['host_os'] and $options[:windows].
  #
  def Env.determine_target_os
    os = CONFIG['host_os']
    if os =~ /mswin/ or $options[:windows]
      return :WINDOWS
    else
      return :UNIX
    end
  end

  #
  # Returns the path to the directory where the vim configuration files will be copied from.
  # The first preference is the directory above this script.    If that fails, we look for the
  # RubyGems package 'vim-ruby'.  Failing that, we return +nil+.
  #
  def Env.determine_source_directory
    # 1. Try the directory above this installation script.
    vim_ruby_source_dir = File.expand_path(File.join(File.dirname($0), '..'))
    return vim_ruby_source_dir if _valid_vim_ruby_dir(vim_ruby_source_dir)
    # 2. Try the gem 'vim-ruby'.
    begin
      require 'rubygems'
      raise "Need RubyGems 0.8+" if Gem::RubyGemsPackageVersion < '0.8'
    rescue LoadError
      return nil
    end
    #vim_ruby_gem_dir = Gem.latest_load_paths.grep(%r{gems/vim-ruby-\d{4}\.\d{2}\.\d{2}}).last
    vim_ruby_gem_dir = Gem.all_load_paths.grep(%r{gems/vim-ruby-\d{4}\.\d{2}\.\d{2}}).sort.last
    if vim_ruby_gem_dir and _valid_vim_ruby_dir(vim_ruby_gem_dir)
      return vim_ruby_gem_dir
    end
    return nil
  end

  # Returns the Vim installation directory ($VIM).
  # TODO: print warning if vim command not in PATH or appropriate key not in registry?
  def Env.determine_vim_dir
    installation_dir = ENV['VIM'] ||
      case Env.determine_target_os
      when :UNIX
        IO.popen('vim --version 2>/dev/null') do |version|
          dir = version.read[/fall-back for \$VIM: "(.*)"/, 1]
        end
      when :WINDOWS
        begin
          require 'win32/registry'
          Win32::Registry::HKEY_LOCAL_MACHINE.open('SOFTWARE\Vim\Gvim') do |reg|
            path = reg['path', Win32::Registry::REG_SZ]
            dir = path.sub(/\\vim\d\d\\gvim.exe/i, '')
          end
        rescue Win32::Registry::Error
          nil
        end
      end
    return installation_dir
  end

  def Env.determine_home_dir
    home_dir = ENV['HOME'] ||
      case Env.determine_target_os
      when :WINDOWS
        ENV['HOMEDRIVE'] + ENV['HOMEPATH'] if ENV['HOMEDRIVE'] and ENV['HOMEPATH']
      end
    return home_dir
  end

  def Env.ask_user(message)
    print message
    gets.strip
  end

  private_class_method

  def Env._valid_vim_ruby_dir(dir)
    Dir.chdir(dir) do
      return SOURCE_FILES.all? { |path| FileTest.file?(path) }
    end
  end

end  # class Env


#
# A FileWriter writes files with pre-selected line endings and permissions.
#
#   writer = FileWriter.new(:UNIX, 0664)
#   writer.copy(source, target)
#
class FileWriter
  LINE_ENDINGS = { :UNIX => "\n", :WINDOWS => "\r\n" }

  def initialize(ending, file_permissions=0644, directory_permissions=0755)
    @ending = LINE_ENDINGS[ending] or raise "No/invalid line ending given: #{ending}"
    @permissions = {
      :file => file_permissions,
      :dir  => directory_permissions
    }
  end

  # Source and target paths assumed to be Pathname objects.  Copy the source to the target,
  # ensuring the right line endings.
  def copy(source_path, target_path)
    _ensure_directory_exists(target_path)
    target_path.open('wb', @permissions[:file]) do |io|
      lines = source_path.read.split("\n")
      lines.each do |line|
        io.write(line.chomp + @ending)
      end
    end
    puts "#{source_path.to_s.ljust(25)} -> #{target_path}"
  end

  # Create the given directory with the correct directory permissions.
  def mkpath(directory)
    FileUtils.mkdir_p(directory.to_s, :mode => @permissions[:dir], :verbose => true)
  end

  def _ensure_directory_exists(path)
    dir = path.dirname
    unless dir.directory?
      # <XXX> FileUtils.mkdir_p already checks if it exists and is a
      # directory.  What if it exists as a file? (HGS)</XXX>
      mkpath(dir)
    end
  end
end  # class FileWriter

#
# Represents the target base directory for installs.  Handles writing the files through a
# given FileWriter.
#
class TargetDirectory
  def self.finder
    TargetDirectory::Finder.new
  end

  def initialize(directory, writer)
    @directory = Pathname.new(directory)
    @writer    = writer     # FileWriter
  end

    # Copies the given relative path from the current directory to the target.
  def copy(path)
    source_path = Pathname.new(path)
    target_path = @directory + path
    @writer.copy(source_path, target_path)
  end

  def [](path)
    @directory + path
  end

  def path
    @directory
  end
end  # class TargetDirectory

#
# Represents the target directory.  Can find candidates, based on the operating system and
# user options; but is ultimately created with one in mind.
#
class TargetDirectory::Finder
  # Guides the user through a selection process, ending in a chosen directory.
  def find_target_directory
      # 1. Was a directory specified using the --directory option?
    if option_dir = $options[:target_dir]
      return option_dir
    end
      # 2. Try the potentials (if there are any).
    if dirs = _potential_directories and not dirs.empty?
      puts
      puts "Possible Vim installation directories:"
      dirs.each_with_index do |dir, idx|
        puts "  #{idx+1}) #{dir}"
      end
      puts
      r = Env.ask_user "Please select one (or anything else to specify another directory): "
      if (1..dirs.size).include? r.to_i
        chosen_directory = dirs[r.to_i - 1]
        return chosen_directory
      end
    end
      # 3. We didn't find any, or the user wants to enter another.
    if dirs.empty?
      puts
      puts "Couldn't find any Vim installation directories."
    end
    entered_directory = Env.ask_user "Please enter the full path to your Vim installation directory: "
    entered_directory = File.expand_path(entered_directory)
    return entered_directory
  end

  private

  # Return an array of _potential_ directories (i.e. they exist).  Take the options into
  # account.
  def _potential_directories
    dirs = []
    dirs << _vim_user_dir
    dirs << _vim_system_dir
    return dirs.compact.map { |dir| File.expand_path(dir) }
  end

  # Return the Vim system preferences directory
  def _vim_system_dir
    vim_dir = Env.determine_vim_dir
    system_dir = vim_dir + "/vimfiles" if vim_dir
    return system_dir
  end

  # Return the Vim user preferences directory
  def _vim_user_dir
    platform_dir = { :UNIX => "/.vim", :WINDOWS => "/vimfiles" }
    home_dir = Env.determine_home_dir
    user_dir = home_dir + platform_dir[Env.determine_target_os] if home_dir
    return user_dir
  end
end  # class TargetDirectory::Finder

#
# VimRubyInstaller is the class that copies the files from the source directory to the target
# directory, both of which are provided.
#
class VimRubyInstaller
  # +source+ and +target+ are the base directories from and to which the configuration files
  # will be copied.  Both are strings.
  def initialize(source, target)
    unless FileTest.directory?(source)
      raise "Automatically determined source directory ('#{source}') doesn't exist"
    end
    unless FileTest.directory?(target)
      raise "Chosen target directory ('#{target}') doesn't exist"
    end
    @source_dir = source
    file_writer = FileWriter.new(Env.determine_target_os)
    @target_dir = TargetDirectory.new(target, file_writer)
  end

  # Since we know the source and target directories, all we have to do is copy the files
  # across.  If the --backup option was specified or the target file is
  # _newer_ than the source file, we make a backup of it and report that to
  # the user.
  def install
    backupdir = BackupDir.new("./vim-ruby-backup.#{Process.pid}")
    Dir.chdir(@source_dir) do
      SOURCE_FILES.each do |path|
        source_path = Pathname.new(path)
        target_path = @target_dir[path]
        # FIXME: Backup everything for now
        if $options[:backup] and target_path.file?
            backupdir.backup(@target_dir, path)
        elsif target_path.file? and target_path.mtime > source_path.mtime
            # We're going to overwrite a newer file; back it up, unless they're the same.
          unless _same_contents?(target_path, source_path)
            backupdir.backup(@target_dir, path)
          end
        end
        @target_dir.copy(path)
      end
    end
    backups = backupdir.contents
    unless backups.empty?
      puts
      puts "The following backups were made:"
      backups.each do |path|
        puts " * #{path}"
      end
      puts
      puts "These backups are located in this directory: #{backupdir.path}"
    end
  end

  private

  # Test two files for equality of contents, ignoring line endings.
  def _same_contents?(p1, p2)
    contents1 = p1.read.split("\n").map { |line| line.chomp }
    contents2 = p2.read.split("\n").map { |line| line.chomp }
    contents1 == contents2
  end

  # A directory for holding backups of configuration files.
  class BackupDir
    def initialize(path)
      @base = Pathname.new(path).expand_path
    end

    # Copy basedir/path to @path/path.
    def backup(basedir, path)
      @base.mkpath unless @base.directory?
      source = basedir.path + path
      target = @base + path
      target.dirname.mkpath
      FileUtils.cp(source.to_s, target.dirname.to_s, :verbose => true)
    end

    def [](path)
      @base + path
    end

    def contents
      return [] unless @base.directory?
      results = []
      Dir.chdir(@base) do
        Pathname.new('.').find do |path|
          results << path if path.file?
        end
      end
      results
    end

    def path
      @base
    end
  end  # class VimRubyInstaller::BackupDir
end  # class VimRubyInstaller

#
#  * * *  M A I N  * * *
#

begin
  $options = {
    :backup     => false,
    :target_dir => nil,
    :windows    => false
  }

  op = OptionParser.new do |p|
    p.banner = %{
       vim-ruby-install.rb: Install the vim-ruby configuration files

        About:
          * Detects the Vim user and system-wide preferences directories
            * User to confirm before proceeding
            * User may specify other directory
          * Takes config files from current directory or from vim-ruby gem
          * Writes files with correct permissions and line endings

        Usage:
          direct:   ruby bin/vim-ruby-install.rb [options]
          gem:      vim-ruby-install.rb [options]

        Options:
    }.gsub(/^    /, '')
    p.on('-b', '--backup', 'Backup existing runtime files') do |value|
      $options[:backup] = value
    end
    p.on('-d DIR', '--directory', 'Install into given directory') do |dir|
      $options[:target_dir] = dir
    end
    p.on('-w', '--windows', 'Install into Windows directories') do |value|
      $options[:windows] = value
    end
    p.on('-h', '--help', 'Show this message') do
      puts p
      exit
    end
    p.on_tail %{
        Notes:

          * "Direct" usage means unpacking a vim-ruby tarball and running this
            program from the vim-ruby directory.

          * The convenient alternative is to use RubyGems:
              gem install vim-ruby
              vim-ruby-install.rb

          * The --windows option is designed for forcing an install into the
            Windows (gvim) configuration directory; useful when running from
            Cygwin or MinGW.

          * This installer is quite new (2004-09-20).  Please report bugs to
            gsinclair@soyabean.com.au.
    }.gsub(/^    /, '')
  end
  op.parse!(ARGV)

  if not ARGV.empty?
    raise "invalid argument: #{ARGV[0]}"
  end

  source_dir = Env.determine_source_directory
  if source_dir.nil?
    raise "Can't find source directory."
  end

  target_dir = TargetDirectory.finder.find_target_directory
  if not File.directory?(target_dir)
    puts
    puts "Target directory '#{target_dir}' does not exist."
    response = Env.ask_user "Do you want to create it? [Yn] "
    response = "y" if response.empty?
    if response.strip =~ /^y(es)?$/i
      FileUtils.mkdir_p(target_dir, :verbose => true)
    else
      puts
      puts "Installation aborted."
      exit
    end
  end

  VimRubyInstaller.new(source_dir, target_dir).install

rescue

  raise if $DEBUG
  $stderr.puts
  $stderr.puts $!.message
  $stderr.puts "Try 'ruby #{$0} --help' for detailed usage."
  exit 1

end

# vim: nowrap sw=2 sts=2 ts=8 ff=unix ft=ruby:
