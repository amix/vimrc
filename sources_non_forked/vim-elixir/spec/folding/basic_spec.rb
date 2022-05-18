# frozen_string_literal: true

require 'spec_helper'

describe 'Basic folding' do
  def self.fold(content)
    it("properly folds \n#{content}") do
      expect(content).to fold_lines
    end
  end

  fold <<~EOF
  defmodule M do # fold
  end # fold
  "not in fold"
  EOF

  fold <<~EOF
  defmodule M do # fold
    def some_func do # fold
    end # fold
  end # fold
  "not in fold"
  EOF

  fold <<~EOF
  defmodule M do
    def some_func do # fold
    end # fold
  end
  "not in fold"
  EOF

  fold <<~EOF
  if true do # fold
  end # fold
  "not in fold"
  EOF

  fold <<~EOF
  if true do # fold
    nil # fold
  else # fold
    nil # fold
  end # fold
  "not in fold"
  EOF

  fold <<~EOF
  defmodule M do
    def some_func do
      [ # fold
        :hello, # fold
        :world # fold
      ] # fold
      :hello_world
    end
  end
  EOF

  fold <<~EOF
  defmodule M do
    def some_func do
      { # fold
        :hello, # fold
        :world # fold
      } # fold
      :hello_world
    end
  end
  EOF

  fold <<~EOF
  defmodule M do
    def some_func do
      %{ # fold
        hello: "a", # fold
        world: "b" # fold
      } # fold
      :hello_world
    end
  end
  EOF

  fold <<~EOF
  defmodule M do
    def some_func do
      %User{ # fold
        hello: "a", # fold
        world: "b" # fold
      } # fold
      :hello_world
    end
  end
  EOF
end
