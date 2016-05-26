require 'spec_helper'

describe "Indenting" do
  specify "multiline tuple" do
    <<-EOF
    def xpto do
      { :a,
        :b,
        :c }
    end
    EOF
    .should be_elixir_indentation
  end

  specify "tuples with break line after square brackets" do
    <<-EOF
    def method do
      {
        :bar,
        path: "deps/umbrella/apps/bar"
      }
    end
    EOF
    .should be_elixir_indentation
  end
end
