# vi: ft=dockerfile
FROM ubuntu:latest

RUN apt-get update && apt-get install -yf neovim

RUN mkdir -p /root/.config/nvim

COPY test.init.vim /root/.config/nvim/init.vim
