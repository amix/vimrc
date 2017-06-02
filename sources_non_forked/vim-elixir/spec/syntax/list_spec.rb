require 'spec_helper'

describe "List syntax" do
  it 'should properly handle "\\\\" inside' do
    syntax = <<-EOF
      '"\\\\'
      var = 1
    EOF
    syntax.should include_elixir_syntax('elixirId', 'var')
    syntax.should_not include_elixir_syntax('elixirString', 'var')
  end
end
