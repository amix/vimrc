require 'spec_helper'

describe "Indenting" do
  context "documentation" do
    it "with end keyword" do
      <<-EOF
        defmodule Test do
          @doc """
          end
          """
        end
      EOF
      .should be_elixir_indentation
    end
  end
end
