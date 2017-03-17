require "spec_helper"

describe "Yankstack" do
  let(:vim) { Vimbot::Driver.new }

  before(:all) do
    vim.start

    vim.set "visualbell"
    vim.set "noerrorbells"
    vim.set "macmeta"

    vim.set "runtimepath+=#{PLUGIN_ROOT}"
    vim.runtime "plugin/yankstack.vim"

    vim.source VIM_REPEAT_PATH
  end

  after(:all)   { vim.stop }
  before(:each) { vim.clear_buffer }

  shared_examples "yanking and pasting" do
    let(:yank_keys) { "yw" }

    before do
      vim.insert "first_line<CR>", "second_line<CR>", "third_line<CR>", "fourth_line"
      vim.normal "gg"
      vim.normal yank_keys, 'j', yank_keys, 'j', yank_keys, 'j', yank_keys
    end

    it "pushes every yanked string to the :Yanks stack" do
      yank_entries[0].should match /0\s+fourth_line/
      yank_entries[1].should match /1\s+third_line/
      yank_entries[2].should match /2\s+second_line/
      yank_entries[3].should match /3\s+first_line/
    end

    describe "yanking with different keys" do
      before do
        vim.normal "A", "<CR>", "line to delete", "<Esc>", "^"
      end

      keys_that_change_register = [
        'cc', 'C',
        'dd', 'D',
        's', 'S',
        'x', 'X',
        'yy', 'Y'
      ]

      keys_that_change_register.each do |key|
        it "pushes to the stack when deleting text with '#{key}'" do
          vim.normal key
          yank_entries[1].should match /1\s+fourth_line/
        end
      end

      it "pushes to the stack when overwriting text in select mode" do
        vim.type "V"
        vim.type "<c-g>", "this overwrites the last line"
        yank_entries[0].should include "line to delete"
        yank_entries[1].should include "fourth_line"
      end
    end

    context "in normal mode" do
      before { vim.normal "o", "<Esc>" }

      describe "pasting a string with 'p'" do
        before { vim.normal "p" }

        it "pastes the most recently yanked string" do
          vim.line_number.should == 5
          vim.line.should == "fourth_line"
        end

        describe "pressing the repeat key with '.'" do
          it "pastes again" do
            pending unless File.exists?(VIM_REPEAT_PATH)

            vim.type "."
            vim.line.should == "fourth_linefourth_line"
          end
        end

        describe "typing the 'cycle paste' key" do
          before { vim.normal "<M-p>" }

          it "replaces the pasted string with the previously yanked text" do
            vim.line.should == "third_line"
          end

          it "rotates the previously yanked text to the top of the yank stack" do
            yank_entries[0].should include 'third_line'
            yank_entries[1].should include 'second_line'
            yank_entries[2].should include 'first_line'
            yank_entries[-1].should include 'fourth_line'
          end

          it "rotates through the yanks when pressed multiple times" do
            vim.normal "<M-p>"
            vim.line.should == "second_line"
            vim.normal "<M-p>"
            vim.line.should == "first_line"

            vim.normal "<M-P>"
            vim.line.should == "second_line"
            vim.normal "<M-P>"
            vim.line.should == "third_line"
            vim.normal "<M-P>"
            vim.line.should == "fourth_line"
          end
        end
      end

      describe "typing the `substitute_older_paste` key without pasting first" do
        before { vim.type "<M-p>" }

        it "pastes the most recently yanked string" do
          vim.line_number.should == 5
          vim.line.should == "fourth_line"
        end

        describe "typing the 'cycle paste' key" do
          before { vim.normal "<M-p>" }

          it "replaces the pasted text with the previously yanked text" do
            vim.line.should == "third_line"
          end
        end
      end

      describe "typing the `substitute_newer_paste` key without pasting first" do
        before { vim.type "<M-P>" }

        it "pastes the most recently yanked string" do
          vim.line_number.should == 5
          vim.line.should == "fourth_line"
        end

        describe "typing the 'cycle paste' key" do
          before { vim.normal "<M-p>" }

          it "replaces the pasted text with the previously yanked text" do
            vim.line.should == "third_line"
          end
        end
      end

      it "allows pasting from a non-default register" do
        reg = 'a'
        vim.normal "gg"
        vim.normal %("#{reg}y$)
        vim.normal "G"

        vim.normal %("#{reg}p)
        vim.line.should == "first_line"
      end

      it "allows pasting with a count" do
        vim.normal "3p"
        vim.line_number.should == 5
        vim.line.should == "fourth_line" * 3
      end
    end

    context "in visual mode, with text highlighted" do
      before do
        vim.normal "A<CR>", "line to overwrite"
        vim.normal "V"
      end

      describe "pasting a string with 'p'" do
        before do
          vim.type "p"
        end

        it "overwrites the selection with the most recently yanked string" do
          vim.line.should == "fourth_line"
        end

        it "moves the the overwritten text to the bottom of the stack" do
          yank_entries[0].should include "fourth_line"
          yank_entries[1].should include "third_line"
          yank_entries[2].should include "second_line"
          yank_entries[-1].should include "line to overwrite"
        end

        describe "typing the 'cycle older paste' key" do
          before { vim.normal "<M-p>" }

          it "replaces the pasted text with the previously yanked text" do
            vim.line.should == "third_line"
          end

          it "moves the previously yanked text to the top of the stack" do
            yank_entries[0].should include "third_line"
            yank_entries[1].should include "second_line"
            yank_entries[2].should include "first_line"
            yank_entries[-2].should include "line to overwrite"
            yank_entries[-1].should include "fourth_line"
          end

          describe "typing the 'cycle newer paste' key" do
            before { vim.normal "<M-P>" }

            it "replaces the pasted text with the previously yanked text" do
              vim.line.should == "fourth_line"
            end

            it "moves the previously yanked text to the top of the stack" do
              yank_entries[0].should include "fourth_line"
              yank_entries[1].should include "third_line"
              yank_entries[2].should include "second_line"
              yank_entries[3].should include "first_line"
              yank_entries[-1].should include "line to overwrite"
            end
          end
        end
      end

      describe "typing the `substitute_older_paste` key without pasting first" do
        before { vim.type "<M-p>" }

        it "overwrites the selection with the most recently yanked string" do
          vim.line_number.should == 5
          vim.line.should == "fourth_line"
        end
      end

      describe "typing the `substitute_newer_paste` key without pasting first" do
        before { vim.type "<M-P>" }

        it "overwrites the selection with the most recently yanked string" do
          vim.line_number.should == 5
          vim.line.should == "fourth_line"
        end
      end

      it "allows pasting with a count" do
        vim.type "3p"

        vim.line_number.should == 5
        vim.line.should == "fourth_line"

        vim.normal 'j'
        vim.line_number.should == 6
        vim.line.should == "fourth_line"

        vim.normal 'j'
        vim.line_number.should == 7
        vim.line.should == "fourth_line"
      end
    end

    context "in insert mode" do
      before do
        vim.normal "A<Cr>", "()", "<Left>"
        vim.type "<M-p>"
      end

      describe "typing the `substitute_older_paste` after a character-wise yank" do
        it "pastes the most recently yanked text after the cursor" do
          vim.line_number.should == 5
          vim.line.should == "(fourth_line)"
        end

        it "stays in insert mode, with the cursor at the end of the pasted text" do
          vim.should be_in_insert_mode
          vim.column_number.should == "(fourth_line".length + 1
        end

        describe "typing the `substitute_older_paste` key again" do
          before { vim.type "<M-p>" }

          it "replaces the pasted text with the previously yanked text" do
            vim.line_number.should == 5
            vim.line.should == "(third_line)"
          end

          it "stays in insert mode, with the cursor at the end of the pasted text" do
            vim.should be_in_insert_mode
            vim.column_number.should == "(third_line".length+1
          end

          it "rotates the previously yanked text to the top of the yank stack" do
            yank_entries[0].should include 'third_line'
            yank_entries[1].should include 'second_line'
            yank_entries[2].should include 'first_line'
            yank_entries[-1].should include 'fourth_line'
          end

          it "rotates through the yanks when pressed multiple times" do
            vim.type "<M-p>"
            vim.line_number.should == 5
            vim.line.should == "(second_line)"

            vim.type "<M-p>"
            vim.line_number.should == 5
            vim.line.should == "(first_line)"

            vim.type "<M-P>"
            vim.line_number.should == 5
            vim.line.should == "(second_line)"

            vim.type "<M-P>"
            vim.line_number.should == 5
            vim.line.should == "(third_line)"

            vim.type "<M-P>"
            vim.line_number.should == 5
            vim.line.should == "(fourth_line)"
          end
        end
      end

      describe "typing `substitute_older_paste` after a line-wise yank" do
        let(:yank_keys) { "yy" }

        xit "pastes and puts the cursor after the pasted text" do
          vim.line_number.should == 6
          vim.line.should == ")"
          vim.type "<Up>"
          vim.line.should == "(fourth_line"
        end
      end
    end
  end

  describe "when using the normal default register" do
    it_has_behavior "yanking and pasting"
  end

  describe "when using the system clipboard as the default register" do
    before { vim.set "clipboard", "unnamed" }
    it_has_behavior "yanking and pasting"
  end

  def yank_entries
    @yank_entries ||= vim.command("Yanks").split("\n")[1..-1]
  end
end

