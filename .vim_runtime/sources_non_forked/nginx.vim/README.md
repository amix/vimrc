# nginx.vim

## Description

[Vim](http://www.vim.org/) plugin for [Nginx](http://www.nginx.org)


## Features

The plugin is based on the recent vim-plugin distributed with `nginx-1.12.0` and additionally features the following syntax improvements:

- Highlight IPv4 and IPv6 addresses
- Mark insecure `ssl_protocols` as errors
- Inline template syntax highlight for **ERB** and **Jinja**
- Inline syntax highlight for **LUA**
- Improve integer matching
- Syntax highlighting for `proxy_next_upstream` options
- Syntax highlighting for `sticky` options
- Syntax highlighting for `upstream` `server` options
- More to come!

Furthermore:

- Remove annoying delimiters, resulting in strange word-boundaries

*Note: Also check out [sslsecure.vim](https://github.com/chr4/sslsecure.vim): it supports highlighting insecure SSL/TLS cipher suites and protocols in all your files!*


## Screenshots

A `server` block with highlighting of insecure `ssl_protocol` options:
![nginx server block with SSL configuration](https://chr4.org/images/nginx_ssl.png)

An `upstream` block with highlighted options:
![nginx upstream configuration](https://chr4.org/images/nginx_upstream.png)

Embedded highlighting for ERB and Jinja templates:
![Embedded highlighting for ERB and Jinja templates](https://chr4.org/images/nginx_templating.png)

Embedded LUA syntax highlighting:
![Embedded LUA syntax highlighting](https://chr4.org/images/nginx_lua.png)


## References

- Based on the original `nginx-1.12.0/contrib/vim`
- IPv4 and IPv6 address highlighting, based on expressions found in [this forum post](http://vim.1045645.n5.nabble.com/IPv6-support-for-quot-dns-quot-zonefile-syntax-highlighting-td1197292.html)
- [Blog post](https://chr4.org/blog/2017/04/14/better-syntax-highlighting-and-snippets-for-nginx-in-vim/) introducing this plugin including some more examples

For help with secure cipher selection, visit [Mozillas SSL Configuration Generator](https://ssl-config.mozilla.org/)

## Installation

If your Vim is at version 8 or later, the first method below is the quickest. Otherwise, install this plugin with any Vim plugin manager ([``vim-plug``](https://github.com/junegunn/vim-plug) is recommended).

### Native plugin management (Vim 8+)

Clone or submodule this repo into your Vim packages location. Example:

```
mkdir -p ~/.vim/pack/plugins/start
cd ~/.vim/pack/plugins/start
git clone https://github.com/chr4/nginx.vim.git
```

### Plug
```
Plug 'chr4/nginx.vim'
```

### Dein.vim
```
call dein#add('chr4/nginx.vim')
```

### Vundle
```
Plugin 'chr4/nginx.vim'
```

### Pathogen
```
git clone https://github.com/chr4/nginx.vim ~/.vim/bundle/nginx.vim
```

Optionally, if you like [Jinja](http://jinja.pocoo.org/) template syntax highlighting, install `lepture/vim-jinja`, too.
