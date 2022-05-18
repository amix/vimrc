# frozen_string_literal: true

require 'spec_helper'

describe 'Embedded Elixir syntax' do
  it 'elixir' do
    expect('<%= if true do %>').to include_eelixir_syntax('elixirKeyword', 'if')
    expect('<%= if true do %>').to include_eelixir_syntax('elixirBoolean', 'true')
  end

  it 'expression' do
    expect('<%= if true do %>').to include_eelixir_syntax('eelixirExpression', 'if')
    expect('<% end %>').to include_eelixir_syntax('eelixirExpression', 'end')
  end

  it 'quote' do
    expect('<%% def f %>').to include_eelixir_syntax('eelixirQuote', 'def')
  end

  it 'comment' do
    expect('<%# foo bar baz %>').to include_eelixir_syntax('eelixirComment', 'foo')
  end

  it 'delimiters' do
    expect('<% end %>').to include_eelixir_syntax('eelixirDelimiter', '<%')
    expect('<% end %>').to include_eelixir_syntax('eelixirDelimiter', '%>')
  end
end

describe 'Embedded Live Elixir syntax' do
  it 'elixir' do
    expect('<%= if true do %>').to include_leelixir_syntax('elixirKeyword', 'if')
    expect('<%= if true do %>').to include_leelixir_syntax('elixirBoolean', 'true')
  end

  it 'expression' do
    expect('<%= if true do %>').to include_leelixir_syntax('eelixirExpression', 'if')
    expect('<% end %>').to include_leelixir_syntax('eelixirExpression', 'end')
  end

  it 'quote' do
    expect('<%% def f %>').to include_leelixir_syntax('eelixirQuote', 'def')
  end

  it 'comment' do
    expect('<%# foo bar baz %>').to include_leelixir_syntax('eelixirComment', 'foo')
  end

  it 'delimiters' do
    expect('<% end %>').to include_leelixir_syntax('eelixirDelimiter', '<%')
    expect('<% end %>').to include_leelixir_syntax('eelixirDelimiter', '%>')
  end
end

describe 'Embedded Surface syntax' do
  it 'elixir' do
    expect('{{ @foo }}').to include_surface_syntax('elixirVariable', 'foo')
    expect('{{ @foo }}').to include_surface_syntax('surfaceDelimiter', '{{')
    expect('{{ @foo }}').to include_surface_syntax('surfaceDelimiter', '}}')
  end
end
