require 'spec_helper'

describe 'Indenting' do

  specify 'method args' do
    assert_correct_indenting <<~EOF
      render('product/show',
             product: product,
             on_sale: true,
            )
    EOF

    vim.command 'let g:ruby_indent_hanging_elements = 0'

    assert_correct_indenting <<~EOF
      render('product/show',
        product: product,
        on_sale: true,
      )
    EOF
  end

  specify 'method args with block' do
    assert_correct_indenting <<~EOF
      opts.on('--coordinator host=HOST[,port=PORT]',
              'Specify the HOST and the PORT of the coordinator') do |str|
                h = sub_opts_to_hash(str)
                puts h
              end
    EOF

    vim.command 'let g:ruby_indent_hanging_elements = 0'

    assert_correct_indenting <<~EOF
      opts.on('--coordinator host=HOST[,port=PORT]',
        'Specify the HOST and the PORT of the coordinator') do |str|
          h = sub_opts_to_hash(str)
          puts h
        end
    EOF
  end

  specify 'arrays' do
    assert_correct_indenting <<~EOF
      x = [1,
           2,
           3,
      ]
    EOF

    vim.command 'let g:ruby_indent_hanging_elements = 0'

    assert_correct_indenting <<~EOF
      x = [1,
        2,
        3,
      ]
    EOF
  end

  specify 'hashes' do
    assert_correct_indenting <<~EOF
      x = { a: 1,
            b: 2,
            c: 3,
      }
    EOF

    vim.command 'let g:ruby_indent_hanging_elements = 0'

    assert_correct_indenting <<~EOF
      x = { a: 1,
        b: 2,
        c: 3,
      }
    EOF
  end
end
