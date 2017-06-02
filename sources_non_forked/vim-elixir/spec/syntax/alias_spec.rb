require 'spec_helper'

describe "Alias syntax" do
  it "colorize only module alias" do
    <<-EOF
      Enum.empty?(...)
    EOF
    .should include_elixir_syntax('elixirAlias', 'Enum')
  end

  it "colorize the module alias even if it starts with `!`" do
    <<-EOF
      !Enum.empty?(...)
    EOF
    .should include_elixir_syntax('elixirAlias', 'aEnum')
  end

  it "does not colorize words starting with lowercase letters" do
    <<-EOF
      aEnum.empty?(...)
    EOF
    .should_not include_elixir_syntax('elixirAlias', 'aEnum')
  end
end
