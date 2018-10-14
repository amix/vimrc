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


## Snippets
The plugin comes with useful snippets which can be accessed using e.g. [vim-snipmate](https://github.com/garbas/vim-snipmate).

Select a decent cipher for your requirements (all of them can provide [SSLLabs A+ ratings](https://www.ssllabs.com/ssltest/analyze.html))

- `ciphers-paranoid<tab>`: Even-more-secure ciphers (elliptic curves, no GCM), not compatible with IE < 11, OpenSSL-0.9.8, Safari < 7, Android != 4.4
- **`ciphers-modern<tab>`: High-security ciphers (elliptic curves), not compatible with IE < 11, OpenSSL-0.9.8, Safari < 7, Android < 4.4 (recommended)**
- `ciphers-compat<tab>`: Medium-security ciphers with good compatibility (No IE on WinXP) but TLSv1 and SHA required
- `ciphers-old<tab>`: Low-security ciphers (using weak DES and SHA ciphers, TLSv1), but compatible with everything but IE6 and Java6
- `ssl-options<tab>`: Bootstrap secure SSL options

Example:
```nginx
# High-security ciphers (elliptic curves), less compatibility
# No IE < 10, OpenSSL-0.9.8, Safari < 7, Android < 4.4
ssl_protocols TLSv1.1 TLSv1.2;
ssl_ciphers 'ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256';
```

Or add a robots.txt file with `robots.txt<tab>`:
```nginx
# Tell bots to not index this site
location /robots.txt {
    default_type text/plain;
    return 200 'User-agent: *\nDisallow: /\n';
}
```

It also has auto-completion for location and server blocks with `location<tab>` resp. `server<tab>`, and [many more](https://github.com/chr4/nginx.vim/blob/master/snippets/nginx.snippets)!

- Add useful [snippets](https://github.com/chr4/nginx.vim/blob/master/snippets/nginx.snippets)

## References
- Based on the original `nginx-1.12.0/contrib/vim`
- IPv4 and IPv6 address highlighting, based on expressions found in [this forum post](http://vim.1045645.n5.nabble.com/IPv6-support-for-quot-dns-quot-zonefile-syntax-highlighting-td1197292.html)
- [Blog post](https://chr4.org/blog/2017/04/14/better-syntax-highlighting-and-snippets-for-nginx-in-vim/) introducing this plugin including some more examples

## Installation

Just plug it into your favorite Vim package manager:

```vim
" Plug
Plug 'chr4/nginx.vim'

" Dein.vim
call dein#add('chr4/nginx.vim')

" Vundle
Plugin 'chr4/nginx.vim'
```

Optionally, if you like [Jinja](http://jinja.pocoo.org/) template syntax highlighting, install `lepture/vim-jinja`, too.
