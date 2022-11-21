require 'spec_helper'

describe "Syntax highlighting" do
  # See issue #171
  specify "ambiguous / at end of line is not a regexp" do
    vim.command 'let g:ruby_operators = 1'
    assert_correct_highlighting <<~'EOF', '/', 'rubyArithmeticOperator'
      a = calculate(90).and_some_long_expression /
          and_long_expression_here
      puts a
    EOF
    vim.command 'unlet g:ruby_operators'
  end

  # See issue #63
  specify "interpolated regexp in a host regexp" do
    assert_correct_highlighting <<~'EOF', '/$', 'rubyRegexpDelimiter'
      /#{foo.sub(/bar/, 'baz')}/
    EOF
  end
end
