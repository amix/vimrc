# frozen_string_literal: true

require 'spec_helper'

describe 'Binary operators' do
  i <<~EOF
  word =
    "h"
    <> "e"
    <> "l"
    <> "l"
    <> "o"

  IO.puts word
  EOF

  i <<~EOF
  def hello do
    expected = "hello"
               <> "world"
    IO.puts expected
  end
  EOF

  i <<~EOF
  def hello do
    expected =
      "hello"
      <> "world"
    IO.puts expected
  end
  EOF

  i <<~EOF
  alias Rumbl.Repo
  alias Rumbl.Category

  for category <- ~w(Action Drama Romance Comedy Sci-fi) do
    Repo.get_by(Category, name: category) ||
      Repo.insert!(%Category{name: category})
  end
  EOF

  i <<~EOF
  data = [
    "blah",
    "blah2", # *
    "blah3"
  ]
  EOF

  i <<~EOF
  data = [
    "blah",
    # +
    "blah2",
    "blah3"
  ]
  EOF
end
