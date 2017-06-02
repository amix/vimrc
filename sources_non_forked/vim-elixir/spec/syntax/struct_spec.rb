require 'spec_helper'

describe "Struct syntax" do
  it "without defaults" do
    <<-EOF
      defstruct [:name, :age]
    EOF
   .should include_elixir_syntax('elixirAtom', ':name')
  end

  it "with defaults" do
    <<-EOF
      defstruct name: "john", age: 27
    EOF
    .should include_elixir_syntax('elixirAtom', 'name:')
  end
end
