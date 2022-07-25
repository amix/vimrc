require 'spec_helper'

describe "Indenting" do
  specify "indented blocks with expression style" do
    vim.command 'let g:ruby_indent_block_style = "expression"'

    assert_correct_indenting <<~EOF
      a.
        b.
        c do |x|
        something
      end

      next_line
    EOF

    assert_correct_indenting <<~EOF
      a.
        b.
        c { |x|
        something
      }

      next_line
    EOF
  end

  specify "indented blocks with do style" do
    vim.command 'let g:ruby_indent_block_style = "do"'

    assert_correct_indenting <<~EOF
      a.
        b.
        c do |x|
          something
        end

      next_line
    EOF

    # Check that "do" style indentation does not mess up indentation
    # following the bock.
    assert_correct_indenting <<~EOF
      a.
        b.
        c do |x|
          something
        end

      next_line
    EOF

    # Check that "do" style indenting works properly for brace blocks.
    assert_correct_indenting <<~EOF
      a.
        b.
        c { |x|
          something
        }

      next_line
    EOF
  end

  specify "'do' indenting" do
    assert_correct_indenting <<~EOF
      do
        something
      end
    EOF

    assert_correct_indenting <<~EOF
      def foo
        a_hash = {:do => 'bar'}
      end
    EOF

    assert_correct_indenting <<~EOF
      def foo(job)
        job.do!
      end
    EOF
  end

  specify "blocks with assignment on the previous line" do
    assert_correct_indenting <<~EOF
      foo =
        something do
          "other"
        end
    EOF

    assert_correct_indenting <<~EOF
      @foo ||=
        something do
          "other"
        end
    EOF
  end

  specify "blocks with multiline parameters" do
    vim.command 'let g:ruby_indent_block_style = "expression"'
    assert_correct_indenting <<~EOF
      def foo
        opts.on('--coordinator host=HOST[,port=PORT]',
                'Specify the HOST and the PORT of the coordinator') do |str|
          h = sub_opts_to_hash(str)
          puts h
        end
      end
    EOF
  end

  specify "case-insensitive matching" do
    vim.set 'ignorecase'
    assert_correct_indenting <<~EOF
      module X
        Class.new do
        end
      end
    EOF
    vim.set 'ignorecase&'
  end

  specify "blocks with tuple arguments" do
    assert_correct_indenting <<~EOF
      proc do |(a, b)|
        puts a
        puts b
      end
    EOF

    assert_correct_indenting <<~EOF
      proc do |foo, (a, b), bar|
        puts a
        puts b
      end
    EOF

    assert_correct_indenting <<~EOF
      proc do |(a, (b, c)), d|
        puts a, b
        puts c, d
      end
    EOF
  end

  specify "blocks with default arguments" do
    assert_correct_indenting <<~EOF
      proc do |a = 1|
        puts a
      end
    EOF

    # See https://github.com/vim-ruby/vim-ruby/issues/304
    assert_correct_indenting <<~EOF
      proc do |a: "asdf", b:|
        proc do
          puts a, b
        end
      end
    EOF
  end
end
