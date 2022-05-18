require 'spec_helper'

describe 'exunit' do
  i <<~EOF
    test "test" do
      Mod.fun(fn ->
        map = %Mod.Map{
          id: "abc123",
          state: "processing",
          submod: %Mod.Submod{
            options: %{}
          }
        }
  EOF

  i <<~EOF
    test "test" do
      Mod.fun(fn ->
        map = %Mod.Map{
          id: "abc123",
          fun: fn ->
            IO.inspect :hello
            IO.inspect %{
              this_is: :a_map
            }
          end,
          submod: %Mod.Submod{
            options: %{}
          }
        }
  EOF
end
