require 'spec_helper'

describe "Indenting" do
  specify "'do' indenting" do
    assert_correct_indenting <<-EOF
      do
        something
      end
    EOF

    assert_correct_indenting <<-EOF
      def foo
        a_hash = {:do => 'bar'}
      end
    EOF

    assert_correct_indenting <<-EOF
      def foo(job)
        job.do!
      end
    EOF
  end

  specify "blocks with multiline parameters" do
    assert_correct_indenting <<-EOF
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
    @vim.set 'ignorecase'
    assert_correct_indenting <<-EOF
      module X
        Class.new do
        end
      end
    EOF
    @vim.set 'ignorecase&'
  end

  specify "blocks with tuple arguments" do
    assert_correct_indenting <<-EOF
      proc do |(a, b)|
        puts a
        puts b
      end
    EOF

    assert_correct_indenting <<-EOF
      proc do |foo, (a, b), bar|
        puts a
        puts b
      end
    EOF

    assert_correct_indenting <<-EOF
      proc do |(a, (b, c)), d|
        puts a, b
        puts c, d
      end
    EOF
  end
end
