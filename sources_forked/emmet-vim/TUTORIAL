Tutorial for Emmet.vim

                                                    mattn <mattn.jp@gmail.com>

1. Expand an Abbreviation

  Type the abbreviation as 'div>p#foo$*3>a' and type '<c-y>,'.
  ---------------------
  <div>
      <p id="foo1">
          <a href=""></a>
      </p>
      <p id="foo2">
          <a href=""></a>
      </p>
      <p id="foo3">
          <a href=""></a>
      </p>
  </div>
  ---------------------

2. Wrap with an Abbreviation

  Write as below.
  ---------------------
  test1
  test2
  test3
  ---------------------
  Then do visual select(line wise) and type '<c-y>,'.
  Once you get to the 'Tag:' prompt, type 'ul>li*'.
  ---------------------
  <ul>
      <li>test1</li>
      <li>test2</li>
      <li>test3</li>
  </ul>
  ---------------------

  If you type a tag, such as 'blockquote', then you'll see the following:
  ---------------------
  <blockquote>
      test1
      test2
      test3
  </blockquote>
  ---------------------

3. Balance a Tag Inward

  type '<c-y>d' in insert mode.

4. Balance a Tag Outward

  type '<c-y>D' in insert mode.

5. Go to the Next Edit Point

  type '<c-y>n' in insert mode.

6. Go to the Previous Edit Point

  type '<c-y>N' in insert mode.

7. Update an <img>’s Size

  Move cursor to the img tag.
  ---------------------
  <img src="foo.png" />
  ---------------------
  Type '<c-y>i' on img tag
  ---------------------
  <img src="foo.png" width="32" height="48" />
  ---------------------

8. Merge Lines

  select the lines, which include '<li>'
  ---------------------
  <ul>
  	<li class="list1"></li>
  	<li class="list2"></li>
  	<li class="list3"></li>
  </ul>
  ---------------------
  and then type '<c-y>m'
  ---------------------
  <ul>
  	<li class="list1"></li><li class="list2"></li><li class="list3"></li>
  </ul>
  ---------------------

9. Remove a Tag

  Move cursor in block
  ---------------------
  <div class="foo">
  	<a>cursor is here</a>
  </div>
  ---------------------
  Type '<c-y>k' in insert mode.
  ---------------------
  <div class="foo">

  </div>
  ---------------------

  And type '<c-y>k' in there again.
  ---------------------

  ---------------------

10. Split/Join Tag

  Move the cursor inside block
  ---------------------
  <div class="foo">
  	cursor is here
  </div>
  ---------------------
  Type '<c-y>j' in insert mode.
  ---------------------
  <div class="foo"/>
  ---------------------

  And then type '<c-y>j' in there again.
  ---------------------
  <div class="foo">
  </div>
  ---------------------

11. Toggle Comment

  Move cursor inside the block
  ---------------------
  <div>
  	hello world
  </div>
  ---------------------
  Type '<c-y>/' in insert mode.
  ---------------------
  <!-- <div>
  	hello world
  </div> -->
  ---------------------
  Type '<c-y>/' in there again.
  ---------------------
  <div>
  	hello world
  </div>
  ---------------------

12. Make an anchor from a URL

  Move cursor to URL
  ---------------------
  http://www.google.com/
  ---------------------
  Type '<c-y>a'
  ---------------------
  <a href="http://www.google.com/">Google</a>
  ---------------------

13. Make some quoted text from a URL

  Move cursor to the URL
  ---------------------
  http://github.com/
  ---------------------
  Type '<c-y>A'
  ---------------------
  <blockquote class="quote">
  	<a href="http://github.com/">Secure source code hosting and collaborative development - GitHub</a><br />
  	<p>How does it work? Get up and running in seconds by forking a project, pushing an existing repository...</p>
  	<cite>http://github.com/</cite>
  </blockquote>
  ---------------------

14. Installing emmet.vim for the language you are using:

  # cd ~/.vim
  # unzip emmet-vim.zip

  Or if you are using pathogen.vim:

  # cd ~/.vim/bundle # or make directory
  # unzip /path/to/emmet-vim.zip

  Or if you get the sources from the repository:

  # cd ~/.vim/bundle # or make directory
  # git clone http://github.com/mattn/emmet-vim.git

15. Enable emmet.vim for the language you using.

  You can customize the behavior of the languages you are using.

  ---------------------
  # cat >> ~/.vimrc
  let g:user_emmet_settings = {
  \  'php' : {
  \    'extends' : 'html',
  \    'filters' : 'c',
  \  },
  \  'xml' : {
  \    'extends' : 'html',
  \  },
  \  'haml' : {
  \    'extends' : 'html',
  \  },
  \}
  ---------------------
