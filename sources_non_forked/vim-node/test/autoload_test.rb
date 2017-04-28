require_relative "./helper"
require "json"

describe "Autoloaded" do
  include WithTemporaryDirectory

  before do
    FileUtils.touch File.join(@dir, "package.json")
  end

  after do
    $vim.command("windo wincmd c")
  end

  describe "Autocommand" do
    it "must fire user autcommand \"Node\"" do
      $vim.command "au User Node let node_autocommand = 1337"
      $vim.edit File.join(@dir, "other.js")
      $vim.echo(%(g:node_autocommand)).must_equal "1337"
    end
  end

  describe "Goto file" do
    it "must define plug mapping in non-JavaScript files" do
      $vim.edit File.join(@dir, "README")
      $vim.echo(%(maparg("<Plug>NodeGotoFile", "n"))).wont_equal ""
    end

    it "must not be available in non-JavaScript files" do
      $vim.edit File.join(@dir, "README")
      $vim.echo(%(hasmapto("<Plug>NodeGotoFile"))).must_equal "0"
    end

    it "must edit README.txt" do
      touch File.join(@dir, "index.js"), %(// Please read README.txt)
      touch File.join(@dir, "README.txt")

      $vim.edit File.join(@dir, "index.js")
      $vim.feedkeys "$gf"
      $vim.echo(%(bufname("%"))).must_equal File.join(@dir, "README.txt")
    end

    it "must edit README before README.js" do
      touch File.join(@dir, "index.js"), "// Please read README"
      touch File.join(@dir, "README")
      touch File.join(@dir, "README.js")

      $vim.edit File.join(@dir, "index.js")
      $vim.feedkeys "$gf"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "README")
    end

    it "must edit ./README.txt relative to file" do
      touch File.join(@dir, "foo", "index.js"), %(// Please read ./README.txt)
      touch File.join(@dir, "foo", "README.txt")

      $vim.edit File.join(@dir, "foo", "index.js")
      $vim.feedkeys "$gf"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "foo", "README.txt")
    end

    it "must edit /.../README.txt" do
      touch File.join(@dir, "index.js"), %(// Read #@dir/lib/README.txt)
      touch File.join(@dir, "lib", "README.txt")

      $vim.edit File.join(@dir, "index.js")
      $vim.feedkeys "$gf"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "lib", "README.txt")
    end

    it "must open ./other.js relative to file" do
      touch File.join(@dir, "foo", "index.js"), %(require("./other")) 
      touch File.join(@dir, "foo", "other.js")
 
      $vim.edit File.join(@dir, "foo", "index.js")
      $vim.feedkeys "f.gf"
 
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "foo", "other.js")
    end

    it "must edit ./index.js given ." do
      touch File.join(@dir, "other.js"), %(require(".")) 
      touch File.join(@dir, "index.js")

      $vim.edit File.join(@dir, "other.js")
      $vim.feedkeys "f.gf"

      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must edit ./index.js given ./" do
      touch File.join(@dir, "other.js"), %(require("./")) 
      touch File.join(@dir, "index.js")

      $vim.edit File.join(@dir, "other.js")
      $vim.feedkeys "f.gf"

      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must edit ../index.js given .." do
      touch File.join(@dir, "foo", "other.js"), %(require("..")) 
      touch File.join(@dir, "index.js")

      $vim.edit File.join(@dir, "foo", "other.js")
      $vim.feedkeys "f.gf"

      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must edit ../index.js given ../" do
      touch File.join(@dir, "foo", "other.js"), %(require("../")) 
      touch File.join(@dir, "index.js")

      $vim.edit File.join(@dir, "foo", "other.js")
      $vim.feedkeys "f.gf"

      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must open ./node_modules/foo/index.js given foo" do
      touch File.join(@dir, "index.js"), %(require("foo")) 
      index = File.join(@dir, "node_modules", "foo", "index.js")
      touch index
 
      $vim.edit File.join(@dir, "index.js")
      $vim.feedkeys "$hhgf"
      $vim.echo(%(bufname("%"))).must_equal index
    end

    it "must not show an error when opening nothing" do
      touch File.join(@dir, "index.js"), %("")

      $vim.edit File.join(@dir, "index.js")
      $vim.command(%(let v:errmsg = ""))
      $vim.feedkeys "gf"

      error = $vim.command("let v:errmsg").sub(/^\S+\s*/, "")
      error.must_equal ""
    end

    it "must show error when opening a non-existent file" do
      touch File.join(@dir, "index.js"), %(require("new"))

      $vim.edit File.join(@dir, "index.js")
      $vim.command(%(let v:errmsg = ""))
      $vim.feedkeys "$hhgf"

      error = $vim.command("let v:errmsg").sub(/^\S+\s*/, "")
      error.must_equal %(E447: Can't find file "new" in path)
    end

    it "must find also when filetype is JSON" do
      $vim.command("au BufReadPre package set ft=json")
      touch File.join(@dir, "package"), %({"dependencies": {"foo": "1.x"}})
      index = File.join(@dir, "node_modules", "foo", "index.js")
      touch index

      $vim.edit File.join(@dir, "package")
      $vim.echo("&filetype").must_equal "json"
      $vim.command("au! BufReadPre package set ft=json")

      $vim.feedkeys "/foo\\<CR>gf"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal index
    end
  end

  describe "Goto file with split" do
    it "must edit file in a new split" do
      touch File.join(@dir, "index.js"), %(require("./other")) 
      touch File.join(@dir, "other.js")

      $vim.edit File.join(@dir, "index.js")
      $vim.feedkeys "f.\\<C-w>f"

      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "other.js")
      $vim.echo(%(winnr("$"))).must_equal "2"
    end
  end

  describe "Goto file with tab" do
    it "must edit file in a new tab" do
      touch File.join(@dir, "index.js"), %(require("./other")) 
      touch File.join(@dir, "other.js")

      $vim.edit File.join(@dir, "index.js")
      $vim.feedkeys "f.\\<C-w>gf"

      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "other.js")
      $vim.echo(%(tabpagenr("$"))).must_equal "2"
    end
  end

  describe "Include file search pattern" do
    it "must find matches given a require" do
      touch File.join(@dir, "index.js"), <<-end.gsub(/^\s+/, "")
        var awesome = require("foo")
        awesome()
      end

      definition = %(module.exports = function awesome() { return 1337 })
      touch File.join(@dir, "node_modules", "foo", "index.js"), definition

      $vim.edit File.join(@dir, "index.js")
      $vim.command("normal G[i").must_equal definition
    end

    it "must find matches given a relative require" do
      touch File.join(@dir, "index.js"), <<-end.gsub(/^\s+/, "")
        var awesome = require("./other")
        awesome()
      end

      definition = %(module.exports = function awesome() { return 1337 })
      touch File.join(@dir, "other.js"), definition

      $vim.edit File.join(@dir, "index.js")
      $vim.command("normal G[i").must_equal definition
    end

    it "must find matches given a relative require in another directory" do
      touch File.join(@dir, "foo", "index.js"), <<-end.gsub(/^\s+/, "")
        var awesome = require("./other")
        awesome()
      end

      definition = %(module.exports = function awesome() { return 1337 })
      touch File.join(@dir, "foo", "other.js"), definition

      $vim.edit File.join(@dir, "foo", "index.js")
      $vim.command("normal G[i").must_equal definition
    end
  end

  describe ":Nedit" do
    # NOTE: Test from a non-JavaScript file everywhere to make sure there are
    # no dependencies on JavaScript specific settings.
    FULL_COMMAND_MATCH = "2"

    it "must be available in non-JavaScript files" do
      $vim.edit File.join(@dir, "README.txt")
      $vim.echo("exists(':Nedit')").must_equal FULL_COMMAND_MATCH
    end

    it "must be available in JavaScript files" do
      $vim.edit File.join(@dir, "index.js")
      $vim.echo("exists(':Nedit')").must_equal FULL_COMMAND_MATCH
    end

    it "must edit ./README.txt" do
      touch File.join(@dir, "README.txt")
      $vim.edit File.join(@dir, "CHANGELOG.txt")
      $vim.command "Nedit ./README.txt"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "README.txt")
    end

    it "must edit ./README.txt relative to node_root" do
      touch File.join(@dir, "README.txt")
      Dir.mkdir File.join(@dir, "lib")
      $vim.edit File.join(@dir, "lib", "CHANGELOG.txt")
      $vim.command "Nedit ./README.txt"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "README.txt")
    end

    it "must edit /.../README.txt" do
      touch File.join(@dir, "lib", "README.txt")
      $vim.edit File.join(@dir, "CHANGELOG.txt")
      $vim.command "Nedit #@dir/lib/README.txt"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "lib", "README.txt")
    end

    it "must edit ./other.js relative to node_root" do
      touch File.join(@dir, "other.js")
      Dir.mkdir File.join(@dir, "lib")
      $vim.edit File.join(@dir, "lib", "CHANGELOG.txt")
      $vim.command "Nedit ./other"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "other.js")
    end

    it "must edit ./index.js given ." do
      touch File.join(@dir, "index.js")
      $vim.edit File.join(@dir, "CHANGELOG.txt")
      $vim.command "Nedit ."
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must edit ./index.js given . relative to node_root" do
      touch File.join(@dir, "index.js")
      Dir.mkdir File.join(@dir, "lib")
      $vim.edit File.join(@dir, "lib", "CHANGELOG.txt")
      $vim.command "Nedit ."
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must edit ./index.js given ./" do
      touch File.join(@dir, "index.js")
      $vim.edit File.join(@dir, "CHANGELOG.txt")
      $vim.command "Nedit ./"
      bufname = File.realpath($vim.echo(%(bufname("%"))))
      bufname.must_equal File.join(@dir, "index.js")
    end

    it "must edit /node_modules/foo/index.js given foo" do
      index = File.join(@dir, "node_modules", "foo", "index.js")
      touch index

      $vim.edit File.join(@dir, "README.txt")
      $vim.command("Nedit foo")
      $vim.echo(%(bufname("%"))).must_equal index
    end

    describe "completion" do
      after do
        $vim.command("set wildignorecase&")
        $vim.command("set fileignorecase&")
      end

      def complete(cmd)
        cmdline = $vim.command(%(silent! normal! :#{cmd}e))
        cmdline.sub(/^:\w+\s+/, "")
      end

      public_core_modules = CORE_MODULES.select {|m| m[0] != "_" }
      private_core_modules = CORE_MODULES.select {|m| m[0] == "_" }

      it "must return files, directories, modules" do
        Dir.mkdir File.join(@dir, "node_modules")
        Dir.mkdir File.join(@dir, "node_modules", "require-guard")
        Dir.mkdir File.join(@dir, "node_modules", "export")
        Dir.mkdir File.join(@dir, "node_modules", "soul")
        touch File.join(@dir, "index.js")

        $vim.edit File.join(@dir, "README.txt")
        files = %w[export/ require-guard/ soul/ ./index.js ./package.json]
        all = public_core_modules + files
        complete("Nedit ").split.sort.must_equal all.sort
      end

      it "must return only public core modules" do
        $vim.edit File.join(@dir, "README.txt")
        modules = public_core_modules + ["./package.json"]
        complete("Nedit ").must_equal modules.join(" ")
      end

      it "must return private core modules if explicitly asked" do
        $vim.edit File.join(@dir, "README.txt")
        complete("Nedit _").must_equal private_core_modules.join(" ")
      end

      it "must return only matching modules" do
        Dir.mkdir File.join(@dir, "node_modules")
        Dir.mkdir File.join(@dir, "node_modules", "export")
        Dir.mkdir File.join(@dir, "node_modules", "soul")
        Dir.mkdir File.join(@dir, "node_modules", "soulstash")

        $vim.edit File.join(@dir, "README.txt")
        modules = "smalloc stream string_decoder sys soul/ soulstash/"
        complete("Nedit s").must_equal modules
      end

      it "must not return modules with matching bit in the middle" do
        Dir.mkdir File.join(@dir, "node_modules")
        Dir.mkdir File.join(@dir, "node_modules", "soul")
        Dir.mkdir File.join(@dir, "node_modules", "soulstash")
        Dir.mkdir File.join(@dir, "node_modules", "asoul")

        $vim.edit File.join(@dir, "README.txt")
        complete("Nedit sou").must_equal "soul/ soulstash/"
      end

      it "must return files and directories in module's directory" do
        touch File.join(@dir, "node_modules", "soul", "index.js")
        touch File.join(@dir, "node_modules", "soul", "test", "test.js")
        $vim.edit File.join(@dir, "README.txt")
        complete("Nedit soul/").must_equal "soul/index.js soul/test/"
      end

      it "must return files and directories given a double slash" do
        touch File.join(@dir, "node_modules", "soul", "index.js")
        touch File.join(@dir, "node_modules", "soul", "test", "test.js")
        $vim.edit File.join(@dir, "README.txt")
        complete("Nedit soul//").must_equal "soul//index.js soul//test/"
      end

      it "must return files case-insensitively given &fileignorecase" do
        skip if $vim.echo(%(exists("&fileignorecase"))) != "1"

        $vim.command("set fileignorecase")
        $vim.command("set nowildignorecase")

        touch File.join(@dir, "node_modules", "soul", "index.js")
        touch File.join(@dir, "node_modules", "soul", "CHANGELOG")
        $vim.edit File.join(@dir, "README.txt")
        complete("Nedit soul/chan").must_equal "soul/CHANGELOG"
      end

      it "must return files case-insensitively given only &wildignorecase" do
        skip if $vim.echo(%(exists("&wildignorecase"))) != "1"

        $vim.command("set nofileignorecase")
        $vim.command("set wildignorecase")

        touch File.join(@dir, "node_modules", "soul", "index.js")
        touch File.join(@dir, "node_modules", "soul", "CHANGELOG")
        $vim.edit File.join(@dir, "README.txt")
        complete("Nedit soul/chan").must_equal "soul/CHANGELOG"
      end
    end
  end

  describe ":Nopen" do
    it "must edit and lcd to module's directory" do
      touch File.join(@dir, "node_modules", "foo", "package.json")
      touch File.join(@dir, "node_modules", "foo", "index.js")

      $vim.edit File.join(@dir, "README.txt")
      $vim.command("vsplit")

      $vim.command("Nopen foo")
      $vim.echo(%(bufname("%"))).must_equal "index.js"
      $vim.command("pwd").must_equal File.join(@dir, "node_modules", "foo")

      $vim.command("wincmd p")
      $vim.command("pwd").must_equal Dir.pwd
    end

    it "must edit and lcd to module's root directory" do
      touch File.join(@dir, "node_modules", "foo", "package.json")
      utils = File.join(@dir, "node_modules", "foo", "lib", "utils.js")
      touch utils

      $vim.edit File.join(@dir, "README.txt")
      $vim.command("vsplit")

      $vim.command("Nopen foo/lib/utils")
      $vim.echo(%(bufname("%"))).must_equal "lib/utils.js"
      $vim.command("pwd").must_equal File.join(@dir, "node_modules", "foo")

      $vim.command("wincmd p")
      $vim.command("pwd").must_equal Dir.pwd
    end
  end
end
