# This is brought as reference, to be able to reproduce a new image

FROM alonid/vim-testbed:10

RUN install_vim -tag v7.4.052 -name vim74-trusty -build \
                -tag v8.0.1850 -name vim80 -build \
                -tag v8.1.0105 -name vim81 -build \
                -tag neovim:v0.1.7 -build \
                -tag neovim:v0.2.2 -build

ENV PACKAGES="\
    bash \
    git \
    python \
    python2-pip \
    curl \
"

RUN dnf install -y $PACKAGES

RUN pip install vim-vint==0.3.19

RUN export HOME=/rust ; mkdir $HOME ; curl https://sh.rustup.rs -sSf | sh -s -- -y

RUN chown vimtest.vimtest -R /rust

RUN (dnf remove -y gcc \*-devel ; \
     dnf install -y gpm msgpack libvterm libtermkey unibilium ) || true
RUN dnf clean all

RUN echo "export PATH=~/.cargo/bin:$PATH" >> ~/.bashrc

RUN git clone https://github.com/da-x/vader.vim vader && \
    cd vader && git checkout v2017-12-26
