require 'spec_helper'

describe "Indenting" do
  specify "method chaining" do
    assert_correct_indenting <<~EOF
      some_object.
        method_one.
        method_two.
        method_three
    EOF

    assert_correct_indenting <<~EOF
      some_object
        .method_one
        .method_two
        .method_three
    EOF

    assert_correct_indenting <<~EOF
      some_object&.
        method_one&.
        method_two&.
        method_three
    EOF

    assert_correct_indenting <<~EOF
      some_object
        &.method_one
        &.method_two
        &.method_three
    EOF
  end

  specify "arrays" do
    assert_correct_indenting <<~EOF
      foo = [one,
             two,
             three]
    EOF
  end

  specify "tricky string interpolation" do
    # See https://github.com/vim-ruby/vim-ruby/issues/75 for details
    assert_correct_indenting <<~EOF
      puts %{\#{}}
      puts "OK"
    EOF

    assert_correct_indenting <<~EOF
      while true
        begin
          puts %{\#{x}}
        rescue ArgumentError
        end
      end
    EOF
  end

  specify "continuations after round braces" do
    vim.command 'let g:ruby_indent_block_style = "expression"'
    assert_correct_indenting <<~EOF
      opts.on('--coordinator host=HOST[,port=PORT]',
              'Specify the HOST and the PORT of the coordinator') do |str|
        h = sub_opts_to_hash(str)
        puts h
      end
    EOF
  end

  describe "assignments" do
    specify "continuations after assignment" do
      assert_correct_indenting <<~EOF
        variable =
          if condition?
            1
          else
            2
          end
      EOF

      assert_correct_indenting <<~EOF
        variable = # evil comment
          case something
          when 'something'
            something_else
          else
            other
          end
      EOF

      assert_correct_indenting <<~EOF
        variable = case something
                   when 'something'
                     something_else
                   else
                     other
                   end
      EOF

      assert_correct_indenting <<~EOF
        variable = if something == something_else
                     something_else
                   elsif other == none
                     none
                   else
                     other
                   end
      EOF

      assert_correct_indenting <<~EOF
        variable = while
                     break something
                   end
      EOF

      assert_correct_indenting <<~EOF
        variable = if [].
                       map { |x| x * 2 }.
                       filter { |x| x % 3 == 0 }.
                       empty?
                     something
                   end
      EOF

      vim.command 'let g:ruby_indent_assignment_style = "variable"'

      assert_correct_indenting <<~EOF
        variable = case something # evil comment
        when 'something'
          something_else
        else
          other
        end
      EOF

      assert_correct_indenting <<~EOF
        variable = if something == something_else
          something_else
        elsif other == none
          none
        else
          other
        end
      EOF

      assert_correct_indenting <<~EOF
        variable = while
          break something
        end
      EOF

      assert_correct_indenting <<~EOF
        variable = if [].
            map { |x| x * 2 }.
            filter { |x| x % 3 == 0 }.
            empty?
          something
        end
      EOF
    end
  end

  specify "continuations after hanging comma" do
    assert_correct_indenting <<~EOF
      array = [
        :one,
      ].each do |x|
        puts x.to_s
      end
    EOF
  end

  specify "string interpolation" do
    # For details, see:
    #
    #   https://github.com/vim-ruby/vim-ruby/issues/93
    #   https://github.com/vim-ruby/vim-ruby/issues/160
    #
    assert_correct_indenting <<~EOF
      command = %|\#{file}|
      settings.log.info("Returning: \#{command}")
    EOF

    assert_correct_indenting <<~EOF
      {
        thing: "[\#{}]",
        thong: "b"
      }
    EOF

    assert_correct_indenting <<~EOF
      {
        a: "(\#{a})",
        b: "(\#{b})",
        c: "(c)",
        d: "(d)",
        e: "(e)",
      }
    EOF
  end

  specify "closing bracket not on its own line" do
    # See https://github.com/vim-ruby/vim-ruby/issues/81 for details
    assert_correct_indenting <<~EOF
      one { two >>
            three }
      four
    EOF
  end

  specify "lonesome single parenthesis in a method definition" do
    # See https://github.com/vim-ruby/vim-ruby/issues/130 for details
    assert_correct_indenting <<~EOF
      def bar(
        baz
      )
        return baz+1
      end
    EOF
  end

  specify "brackets on their own line, followed by a comma" do
    # See https://github.com/vim-ruby/vim-ruby/issues/124 for details
    assert_correct_indenting <<~EOF
      bla = {
        :one => [
          {:bla => :blub}
        ],
        :two => (
          {:blub => :abc}
        ),
        :three => {
          :blub => :abc
        },
        :four => 'five'
      }
    EOF
  end

  specify "string with an and#" do
    # See https://github.com/vim-ruby/vim-ruby/issues/108 for details
    assert_correct_indenting <<~EOF
      outside_block "and#" do
        inside_block do
        end
      end
    EOF
  end

  specify "continuation with a symbol at the end" do
    # See https://github.com/vim-ruby/vim-ruby/issues/132 for details
    assert_correct_indenting <<~EOF
      foo = :+
      # Next indents correctly
    EOF
  end

  specify "continuation with a hanging comma" do
    # See https://github.com/vim-ruby/vim-ruby/issues/139 for details
    assert_correct_indenting <<~EOF
      thing :foo
      thing 'a',
        'b'
    EOF
  end

  specify "continuations in an if-clause condition" do
    # See https://github.com/vim-ruby/vim-ruby/issues/215 for details
    assert_correct_indenting <<~EOF
      if foo || bar ||
          bong &&
          baz || bing
        puts "foo"
      end
    EOF
  end

  specify "continuations with round brackets" do
    # See https://github.com/vim-ruby/vim-ruby/issues/17 for details
    assert_correct_indenting <<~EOF
      foo and
        (bar and
         baz) and
        bing
    EOF
  end

  specify "block within an argument list" do
    # See https://github.com/vim-ruby/vim-ruby/issues/312 for details
    assert_correct_indenting <<~EOF
      foo(
        x: 1,
        y: [1, 2, 3].map { |i|
          i + 1
        }
      )
    EOF
  end

  specify "backslashes" do
    # See https://github.com/vim-ruby/vim-ruby/issues/311 for details
    assert_correct_indenting <<~EOF
      def foo
        x = 1

        string = ". \#{x}" \\
          "xyz"

        puts string
        puts string
      end
    EOF
  end

  specify "wrong continuation within regex character class" do
    # See https://github.com/vim-ruby/vim-ruby/issues/405 for details

    assert_correct_indenting <<~EOF
      extname = file.extname(url).split(/[?#]/).first
      target_file = tempfile.new()
    EOF
  end
end
