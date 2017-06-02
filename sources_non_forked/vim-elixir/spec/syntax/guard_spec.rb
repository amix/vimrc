require 'spec_helper'

describe "Guard syntax" do
  it "guard in function" do
    <<-EOF
    def fun(a) when is_atom(a) do
    end
    EOF
      .should include_elixir_syntax('elixirKeyword', 'is_atom')
  end

  it "guard in case" do
    <<-EOF
    case
      a when is_atom(a) -> {:ok, a}
    end
    EOF
      .should include_elixir_syntax('elixirKeyword', 'is_atom')
  end

  it "does not highlight outside guards" do
    <<-EOF
      if is_atom(a) do
        {:ok, a}
      end
    EOF
      .should_not include_elixir_syntax('elixirKeyword', 'is_atom')
  end
end
