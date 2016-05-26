require 'spec_helper'

describe "Indenting" do
  it "conditional" do
    <<-EOF
      cond do
        foo -> 1
        bar -> 2
      end
    EOF
    .should be_elixir_indentation
  end
end
