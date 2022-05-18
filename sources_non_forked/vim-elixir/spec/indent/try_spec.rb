require 'spec_helper'

describe 'try indent' do
  i <<~EOF
  try do
  rescue
  end
  EOF

  i <<~EOF
  try do
  catch
  end
  EOF

  i <<~EOF
  try do
  after
  end
  EOF

  i <<~EOF
  test "it proceses the command" do
    out = "testfile"
    try do
      cmd = "thing \#{@test_file} \#{out}"
      {:ok, 0, _} = Thing.exec(cmd)
    after
      File.rm!(out)
    end
  end
  EOF

  i <<~EOF
  try do
    foo()
  else
    value -> value
  end
  EOF
end
