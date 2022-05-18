# frozen_string_literal: true

require 'spec_helper'

describe 'Struct syntax' do
  it 'without defaults' do
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', ':name')
      defstruct [:name, :age]
    EOF
  end

  it 'with defaults' do
    expect(<<~EOF).to include_elixir_syntax('elixirAtom', 'name:')
      defstruct name: "john", age: 27
    EOF
  end

  it 'structs' do
    str = %q(%MyStruct{name: "one"})

    expect(str).to include_elixir_syntax('elixirStructDelimiter', '%')
    expect(str).to include_elixir_syntax('elixirStruct', '%')

    expect(str).to include_elixir_syntax('elixirAlias', 'MyStruct')
    expect(str).to include_elixir_syntax('elixirStruct', 'MyStruct')

    expect(str).to include_elixir_syntax('elixirStructDelimiter', '{')
    expect(str).to include_elixir_syntax('elixirStruct', '{')

    expect(str).to include_elixir_syntax('elixirAtom', 'name:')
    expect(str).to include_elixir_syntax('elixirStruct', 'name:')

    expect(str).to include_elixir_syntax('elixirStructDelimiter', '}')
  end

  it 'properly closes strings in structs' do
    str = <<~'EOF'
    %MyStruct{url: "http://127.0.0.1:#{port}"} # anchor
    # this should not be a string still
    EOF

    expect(str).to include_elixir_syntax('elixirStruct', '{url')

    expect(str).to include_elixir_syntax('elixirStringDelimiter', '"http')
    expect(str).to include_elixir_syntax('elixirStruct', '"http')

    expect(str).to include_elixir_syntax('elixirInterpolationDelimiter', '#{')
    expect(str).to include_elixir_syntax('elixirStruct', '#{')

    expect(str).to include_elixir_syntax('elixirInterpolationDelimiter', '}"}')
    expect(str).to include_elixir_syntax('elixirStruct', '}"}')
    expect(str).not_to include_elixir_syntax('elixirStructDelimiter', '}"}')

    expect(str).to include_elixir_syntax('elixirStringDelimiter', '"}')
    expect(str).to include_elixir_syntax('elixirStruct', '"}')

    expect(str).to include_elixir_syntax('elixirStringDelimiter', '"}')
    expect(str).to include_elixir_syntax('elixirStruct', '"}')

    expect(str).to include_elixir_syntax('elixirStructDelimiter', '} #')

    expect(str).to include_elixir_syntax('elixirComment', '# this should not be a string still')
  end
end
