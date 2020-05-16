require "spec_helper"

describe "vim for cython" do
  before(:all) {
    vim.command "new"
    vim.command "set ft=cython"
    vim.command("set indentexpr?").should include "GetPythonPEPIndent("
  }
  before(:each) {
    # clear buffer
    vim.normal 'gg"_dG'

    # Insert two blank lines.
    # The first line is a corner case in this plugin that would shadow the
    # correct behaviour of other tests. Thus we explicitly jump to the first
    # line when we require so.
    vim.feedkeys 'i\<CR>\<CR>\<ESC>'
  }
  after(:all) {
    vim.command "bwipe!"
  }

  describe "when using a cdef function definition" do
      it "indents shiftwidth spaces" do
          vim.feedkeys 'icdef long_function_name(\<CR>arg'
          indent.should == shiftwidth
      end
  end

  describe "when using a cpdef function definition" do
      it "indents shiftwidth spaces" do
          vim.feedkeys 'icpdef long_function_name(\<CR>arg'
          indent.should == shiftwidth
      end
  end
end
