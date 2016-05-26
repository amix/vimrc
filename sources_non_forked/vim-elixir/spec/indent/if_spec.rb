require 'spec_helper'

describe "Indenting" do
  it "if-clauses" do
    <<-EOF
      if foo do
        bar
      end
    EOF
    .should be_elixir_indentation
  end

  it "if-else-clauses" do
    <<-EOF
      if foo do
        bar
      else
        baz
      end
    EOF
     .should be_elixir_indentation
  end
end
