require 'spec_helper'

describe "Default argument syntax" do
  it "default argument" do
    <<-'EOF'
      def foo(bar \\ :baz)
    EOF
    .should include_elixir_syntax('elixirOperator', '\\')

    <<-EOF
      def foo(bar // :baz)
    EOF
    .should include_elixir_syntax('elixirOperator', '\/')
  end
end
