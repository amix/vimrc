# frozen_string_literal: true

require 'spec_helper'

describe 'ExUnit syntax' do
  it 'test macro' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitMacro', 'test')
    test 'that stuff works' do
      assert true
    end
    EOF
  end

  it 'describe macro' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitMacro', 'describe')
    describe 'some_function/1' do
      test 'that stuff works' do
        assert true
      end
    end
    EOF
  end

  it 'setup macro' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitMacro', 'setup')
    setup do
      IO.puts "hi mom"
    end

    test 'that stuff works' do
      assert true
    end
    EOF
  end

  it 'setup_all macro' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitMacro', 'setup_all')
    setup_all do
      IO.puts "hi mom"
    end

    test 'that stuff works' do
      assert true
    end
    EOF
  end

  it 'on_exit macro' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitMacro', 'on_exit')
    setup_all do
      IO.puts "hi mom"
      on_exit fn() ->
        do_something
      end
    end

    test 'that stuff works' do
      assert true
    end
    EOF
  end

  it 'assert' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'assert')
    test 'that stuff works' do
      assert true
    end
    EOF
  end

  it 'assert_in_delta' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'assert_in_delta')
    test 'that stuff works' do
      assert_in_delta true
    end
    EOF
  end

  it 'assert_raise' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'assert_raise')
    test 'that stuff works' do
      assert_raise true
    end
    EOF
  end

  it 'assert_receive' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'assert_receive')
    test 'that stuff works' do
      assert_receive true
    end
    EOF
  end

  it 'assert_received' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'assert_received')
    test 'that stuff works' do
      assert_received true
    end
    EOF
  end

  it 'catch_error' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'catch_error')
    test 'that stuff works' do
      catch_error true
    end
    EOF
  end

  it 'catch_exit' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'catch_exit')
    test 'that stuff works' do
      catch_exit true
    end
    EOF
  end

  it 'catch_throw' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'catch_throw')
    test 'that stuff works' do
      catch_throw true
    end
    EOF
  end

  it 'flunk' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'flunk')
    test 'that stuff works' do
      flunk true
    end
    EOF
  end

  it 'refute' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'refute')
    test 'that stuff works' do
      refute true
    end
    EOF
  end

  it 'refute_in_delta' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'refute_in_delta')
    test 'that stuff works' do
      refute_in_delta true
    end
    EOF
  end

  it 'refute_receive' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'refute_receive')
    test 'that stuff works' do
      refute_receive true
    end
    EOF
  end

  it 'refute_received' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitAssert', 'refute_received')
    test 'that stuff works' do
      refute_received true
    end
    EOF
  end

  it 'doctest' do
    expect(<<~EOF).to include_elixir_syntax('elixirExUnitMacro', 'doctest')
    module MyTest do
      doctest MyModule
    end
    EOF
  end
end
