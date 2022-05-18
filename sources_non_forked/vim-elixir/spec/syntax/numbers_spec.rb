# frozen_string_literal: true

require 'spec_helper'

describe 'Numbers syntax' do
  describe 'decimal' do
    it 'positive is colorized' do
      expect('123').to include_elixir_syntax('elixirNumber', '123')
    end

    it 'negative is colorized' do
      expect('-123').to include_elixir_syntax('elixirNumber', '123')
    end
  end

  describe 'hexadecimal' do
    it 'positive is colorized' do
      expect('0xdeadbeaf').to include_elixir_syntax('elixirNumber', '0xdeadbeaf')
    end

    it 'negative is colorized' do
      expect('-0xdeadbeaf').to include_elixir_syntax('elixirNumber', '0xdeadbeaf')
    end
  end

  describe 'octal' do
    it 'positive is colorized' do
      expect('0o777').to include_elixir_syntax('elixirNumber', '0o777')
    end

    it 'negative is colorized' do
      expect('-0o777').to include_elixir_syntax('elixirNumber', '0o777')
    end
  end

  describe 'binary' do
    it 'positive is colorized' do
      expect('0b1011').to include_elixir_syntax('elixirNumber', '0b1011')
    end

    it 'negative is colorized' do
      expect('-0b1011').to include_elixir_syntax('elixirNumber', '0b1011')
    end
  end
end
