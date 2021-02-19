require 'spec_helper'

describe "Indenting" do
  specify "default indented access modifiers" do
    assert_correct_indenting <<~EOF
      class OuterClass

        private :method
        protected :method
        def method; end
        protected
        def method; end
        private
        def method; end
        public
        def method; end

        class InnerClass

          private :method
          protected :method
          def method; end
          protected
          def method; end
          private
          def method; end
          public
          def method; end

        end

        private :method
        protected :method
        def method; end
        protected
        def method; end
        private
        def method; end
        public
        def method; end

      end
    EOF
  end

  specify "indented access modifiers" do
    vim.command 'let g:ruby_indent_access_modifier_style = "indent"'

    assert_correct_indenting <<~EOF
      class OuterClass

        private :method
        protected :method
        def method; end
        protected
          def method; end
        private
          def method; end
        public
        def method; end

        class InnerClass

          private :method
          protected :method
          def method; end
          protected
            def method; end
          private
            def method; end
          public
          def method; end

        end

        private :method
        protected :method
        def method; end
        protected
          def method; end
        private
          def method; end
        public
        def method; end

      end
    EOF
  end

  specify "outdented access modifiers" do
    vim.command 'let g:ruby_indent_access_modifier_style = "outdent"'

    assert_correct_indenting <<~EOF
      class OuterClass

        private :method
        protected :method
        def method; end
      protected
        def method; end
      private
        def method; end
      public
        def method; end

        class InnerClass

          private :method
          protected :method
          def method; end
        protected
          def method; end
        private
          def method; end
        public
          def method; end

        end

        private :method
        protected :method
        def method; end
      protected
        def method; end
      private
        def method; end
      public
        def method; end

      end
    EOF
  end
end
