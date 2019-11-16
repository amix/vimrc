FROM testbed/vim:latest

RUN apk --no-cache add gtk+2.0-dev libx11-dev libxt-dev mcookie xauth xvfb
# NOTE: +profile needs huge features.
RUN install_vim -tag v8.1.0129 -name vim --with-features=huge \
  --disable-channel --disable-netbeans --disable-xim \
  --enable-gui=gtk2 --with-x -build
RUN ln -s /vim-build/bin/vim /usr/bin/gvim
RUN gvim --version

# Install covimerage and vint.
# NOTE: we have py2 already via gtk+2.0-dev.
# NOTE: enum34+pathlib+typing gets installed as workaround for broken vim-vint wheel.
RUN apk --no-cache add py2-pip \
  && pip install --no-cache-dir codecov covimerage==0.0.9 vim-vint enum34 pathlib typing \
  && rm -rf /usr/include /usr/lib/python*/turtle* /usr/lib/python*/tkinter

WORKDIR /vim-python-pep8-indent

ADD Gemfile .
RUN apk --no-cache add coreutils ruby-bundler
RUN bundle install

ENTRYPOINT ["rspec", "spec"]
