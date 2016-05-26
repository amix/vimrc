# encoding: utf-8
require 'spec_helper'

describe "Record syntax" do
  it "private record symbol" do
    <<-EOF
      defrecordp :user, name: "José", age: 25
    EOF
    .should include_elixir_syntax('elixirAtom', ':user')
  end
end
