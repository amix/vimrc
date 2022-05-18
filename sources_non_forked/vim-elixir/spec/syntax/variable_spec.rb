# frozen_string_literal: true

require 'spec_helper'

describe 'Variable syntax' do
  it 'unused' do
    expect(<<~EOF).to include_elixir_syntax('elixirUnusedVariable', '_from')
      def handle_call(:pop, _from, [h|stack]) do
        { :reply, h, stack }
      end
    EOF
  end

  it 'unused in function body' do
    expect(<<~EOF).not_to include_elixir_syntax('elixirUnusedVariable', '_from')
    def handle_call(:pop)
      Hello._from
    end
    EOF
  end

  it 'unused, multiple lines' do
    expect(<<~EOF).to include_elixir_syntax('elixirUnusedVariable', '_from')
      def handle_call(:pop,
                      _from,
                      [h|stack]) do
        { :reply, h, stack }
      end
    EOF
  end

  it 'unused, single char' do
    expect(<<~EOF).to include_elixir_syntax('elixirUnusedVariable', '_')
      def call(:pop, _, [h|stack]) do
        { :reply, h, stack }
      end
    EOF
  end

  it 'unused in pattern_match' do
    str = <<~EOF
    def sign_in(conn, %{
      "data" => %{
        "type" => "doctor",
        "attributes" => %{
          "institution_code" => institution_code,
          "password" => password,
          "email_or_phone" => email_or_phone}}}, _user, _claims) do
      :ok
    end
    EOF
    expect(str).to include_elixir_syntax('elixirUnusedVariable', '_user')
    expect(str).to include_elixir_syntax('elixirUnusedVariable', '_claims')
  end

  it 'unused, in anonymous function, inline' do
    expect(<<~EOF).to include_elixir_syntax('elixirUnusedVariable', '_unused')
      fun = fn _unused -> false end
    EOF
  end

  it 'unused, in anonymous function, multiple lines' do
    expect(<<~EOF).to include_elixir_syntax('elixirUnusedVariable', '_unused')
      fun  = fn
        ([], _unused) -> true
      end
    EOF
  end

  it 'unused, in pattern matching' do
    expect(<<~EOF).to include_elixir_syntax('elixirUnusedVariable', '_unused')
      _unused = false
    EOF
  end
end
