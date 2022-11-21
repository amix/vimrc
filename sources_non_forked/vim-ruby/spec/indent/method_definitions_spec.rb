require 'spec_helper'

describe "Indenting" do
  specify "method definitions prefixed with access modifiers" do
    assert_correct_indenting <<~EOF
      class Foo
        public def one(x)
        end

        private def two(y)
          code
        end
      end
    EOF
  end

  specify "method definitions prefixed with any method call" do
    assert_correct_indenting <<~EOF
      class Foo
        foobar def one(x)
        end
        foobar? def one(x)
        end
        foobar! def one(x)
        end

        фубар def one(x)
        end

        foobar
        def one(x)
        end

        FooBar1 def two(y)
          code
        end
      end
    EOF
  end

  specify "endless methods" do
    # Note: A case that doesn't work at this time:
    #
    #   def foo()
    #     = 42
    #
    assert_correct_indenting <<~EOF
      indented_block do
        def foo(bar) = puts(bar)
        def foo!(bar) = puts(bar)
        def foo?(bar) = puts(bar)

        def foo(bar)=puts(bar)

        def foo(bar) = bar + 1

        def foo() = 1 + 1
        def foo = 1 + 1

        private def foo(bar) = bar + 1

        def foo(bar) =
          bar + 1

        def foo(bar = default_function()) = puts(bar)

        def foo(bar = default_function()) =
          puts(bar)

        def foo(
          bar
        ) = puts(bar)

        # Reference: https://github.com/vim-ruby/vim-ruby/issues/450
        def self.foo = puts(bar)
        def bar.foo = puts(baz)
      end
    EOF
  end
end
