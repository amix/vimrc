require_relative "./helper"

describe "Plugin" do
  include WithTemporaryDirectory

  describe "b:node_root" do
    it "must be set when in same directory with package.json" do
      FileUtils.touch File.join(@dir, "package.json")
      $vim.edit File.join(@dir, "index.js")
      $vim.echo("b:node_root").must_equal @dir
    end

    it "must be set when in same directory with node_modules" do
      Dir.mkdir File.join(@dir, "node_modules")
      $vim.edit File.join(@dir, "index.js")
      $vim.echo("b:node_root").must_equal @dir
    end

    it "must be set when ancestor directory has package.json" do
      FileUtils.touch File.join(@dir, "package.json")

      nested = File.join(@dir, "lib", "awesomeness")
      FileUtils.mkdir_p nested
      $vim.edit File.join(nested, "index.js")
      $vim.echo("b:node_root").must_equal @dir
    end

    it "must be set when ancestor directory has node_modules" do
      Dir.mkdir File.join(@dir, "node_modules")

      nested = File.join(@dir, "lib", "awesomeness")
      FileUtils.mkdir_p nested
      $vim.edit File.join(nested, "index.js")
      $vim.echo("b:node_root").must_equal @dir
    end

    it "must be set also for other filetypes" do
      FileUtils.touch File.join(@dir, "package.json")

      $vim.edit File.join(@dir, "README.txt")
      $vim.echo("b:node_root").must_equal @dir
    end

    it "must be set in nested Node projects" do
      nested = File.join(@dir, "node_modules", "require-guard")
      FileUtils.mkdir_p nested
      FileUtils.touch File.join(nested, "package.json")

      test = File.join(nested, "test")
      FileUtils.mkdir_p test
      $vim.edit File.join(test, "index_test.js")
      $vim.echo("b:node_root").must_equal nested
    end

    it "must not be set when no ancestor has one" do
      $vim.edit File.join(@dir, "index_test.js")
      $vim.echo(%(exists("b:node_root"))).must_equal "0"
    end

    it "must be set from file, not working directory" do
      $vim.command "cd #{@dir}"
      FileUtils.touch File.join(@dir, "package.json")

      nested = File.join(@dir, "node_modules", "require-guard")
      FileUtils.mkdir_p nested
      FileUtils.touch File.join(nested, "package.json")

      $vim.edit File.join(nested, "index_test.js")
      $vim.echo("b:node_root").must_equal nested
    end

    it "must detect directory as Node's when opening Vim" do
      begin
        Dir.chdir @dir
        FileUtils.touch File.join(@dir, "package.json")

        vim = Vimrunner::Server.new(:vimrc => $vimrc).start
        vim.command("pwd").must_equal @dir
        vim.echo("b:node_root").must_equal @dir
      ensure
        vim.kill if vim
      end
    end
  end
end
