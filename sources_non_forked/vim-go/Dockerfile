FROM golang:1.9.1

RUN apt-get update -y && \
  apt-get install -y build-essential curl git libncurses5-dev python3-pip && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN useradd -ms /bin/bash -d /vim-go vim-go
USER vim-go

COPY . /vim-go/
WORKDIR /vim-go

RUN scripts/install-vim vim-7.4
RUN scripts/install-vim vim-8.0
RUN scripts/install-vim nvim
RUN pip3 install vim-vint

ENTRYPOINT ["make"]
