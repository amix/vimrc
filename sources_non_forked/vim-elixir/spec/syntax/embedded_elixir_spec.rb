require 'spec_helper'

describe "Embedded Elixir syntax" do
  it "elixir" do
    '<%= if true do %>'.should include_eelixir_syntax('elixirKeyword', 'if')
    '<%= if true do %>'.should include_eelixir_syntax('elixirBoolean', 'true')
  end

  it "expression" do
    '<%= if true do %>'.should include_eelixir_syntax('eelixirExpression', 'if')
    '<% end %>'.should include_eelixir_syntax('eelixirExpression', 'end')
  end

  it "quote" do
    '<%% def f %>'.should include_eelixir_syntax('eelixirQuote', 'def')
  end

  it "comment" do
    '<%# foo bar baz %>'.should include_eelixir_syntax('eelixirComment', 'foo')
  end

  it "delimiters" do
    '<% end %>'.should include_eelixir_syntax('eelixirDelimiter', '<%')
    '<% end %>'.should include_eelixir_syntax('eelixirDelimiter', '%>')
  end
end
