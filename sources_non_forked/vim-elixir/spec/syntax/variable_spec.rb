require 'spec_helper'

describe "Variable syntax" do
  it "unused" do
    <<-EOF
      def handle_call(:pop, _from, [h|stack]) do
        { :reply, h, stack }
      end
    EOF
    .should include_elixir_syntax('elixirUnusedVariable', '_from')
  end
end
