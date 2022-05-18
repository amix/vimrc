require 'spec_helper'

describe 'Macros' do
  i <<~EOF
  defmodule DeadboltTest do
    use ExUnit.Case
    doctest Deadbolt

    hello

  end
  EOF
end
