# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting if clauses' do
  i <<~EOF
  if foo do
    bar
  end
  EOF

  i <<~EOF
  if foo do
    bar
  else
    baz
  end
  EOF

  i <<~EOF
  def test do
    "else"
  end
  EOF

  i <<~EOF
  if true do
  else
  end
  EOF

  i <<~EOF
  def exec(command, progress_func \\ fn(_, state) -> state end, key \\ nil, output \\ nil) do
    if key do
      with_cache(key, output, fn -> do_exec(command, progress_func) end)
    else
      do_exec(command, progress_func)
    end
  end
  EOF
end
