FROM testbed/vim:20

RUN install_vim -tag v8.0.0027 -build \
                -tag v8.2.4693 -build \
                -tag neovim:v0.2.0 -build \
                -tag neovim:v0.6.1 -build

ENV PACKAGES="\
    bash \
    git \
    python2 \
    python3 \
    py3-pip \
    grep \
    sed \
"
RUN apk --update add $PACKAGES && \
    rm -rf /var/cache/apk/* /tmp/* /var/tmp/*

RUN pip install vim-vint==0.3.21

RUN git clone https://github.com/junegunn/vader.vim vader && \
    cd vader && git checkout c6243dd81c98350df4dec608fa972df98fa2a3af

ARG GIT_VERSION
LABEL Version=${GIT_VERSION}
LABEL Name=denseanalysis/ale
