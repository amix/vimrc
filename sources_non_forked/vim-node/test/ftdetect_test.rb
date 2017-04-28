require_relative "./helper"

describe "Ftdetect" do
  [
    "#!/usr/bin/env node",
    "#!/usr/bin/env node --harmony-generators",
    "#!/usr/local/bin/env node",
    "#!/usr/local/bin/env node --harmony-generators",
    "#!/usr/bin/node",
    "#!/usr/bin/node --harmony-generators",
    "#!/usr/local/bin/node",
    "#!/usr/local/bin/node --harmony-generators",

  ].each do |shebang|
    it %(must detect a file with "#{shebang}" shebang as JavaScript) do
      file = Tempfile.new("bang")
      file.write shebang + $/
      file.close
      $vim.edit file.path
      $vim.echo("&ft").must_equal "javascript"
    end
  end

  [
    "#!/usr/bin/env noder",
    "#!/usr/bin/noder",

  ].each do |shebang|
    it %(must not detect a file with "#{shebang}" shebang as JavaScript) do
      file = Tempfile.new("bang")
      file.write shebang + $/
      file.close
      $vim.edit file.path
      $vim.echo("&ft").wont_equal "javascript"
    end
  end

  it "must not detect a .c file as JavaScript even with Node's shebang" do
    file = Tempfile.new(%w[tea .c])
    file.write "#!/usr/bin/node" + $/
    file.close
    $vim.edit file.path
    $vim.echo("&ft").wont_equal "javascript"
  end
end
