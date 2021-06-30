require 'spec_helper'

describe "Maxmempattern limit" do
  specify "maxmempattern=1000 is enough even for long strings" do
    str = <<~'EOF'
      hash = {
        "A-NOT-Managed-Strings" => "ABCDEfghe910dmckamks019292djdjOOOjjjd/cr3wdCA+1n/xHfHMgG+cC0EoUNngcBjgWvBMEF1CurBwTtDswJjQYa5wYRAQEBAQECCwGwAQEvI50CnwMNAwRrAQYBr9PPAoK7sQMBAQMCBAkICAQIAwEBAwYBAQQFFQEBAhQDAwMDCwEBAQUBAQHGAQEWBAEBDecBfS8CHQEKkAEMMxcMCQoUDwYHIjd3DQ4MFk0JWGYALSKLAQOLAYEBFBAjCBGDAQICAgMANjsZAg9fCxkCgLZKAwSEAQIBiwEZGAsrBCgFMmUEJShyFSfRBQEOSQY62AG0AVlCrQ",
      }
    EOF
    assert_correct_highlighting str, %w[ABCDE], 'rubyString'
  end
end
