FROM golang:1.18.1

RUN apt-get update -y --allow-insecure-repositories && \
  apt-get install -y build-essential curl git libncurses5-dev python3-pip && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN pip3 install vim-vint

RUN useradd -ms /bin/bash -d /vim-go vim-go
USER vim-go

COPY scripts/install-vim /vim-go/scripts/install-vim
WORKDIR /vim-go

RUN scripts/install-vim vim-8.0
RUN scripts/install-vim vim-8.2
RUN scripts/install-vim nvim

COPY . /vim-go/
WORKDIR /vim-go

RUN scripts/install-tools vim-8.0
RUN scripts/install-tools vim-8.2
RUN scripts/install-tools nvim

ENTRYPOINT ["make"]
