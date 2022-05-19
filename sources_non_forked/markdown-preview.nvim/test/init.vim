set nocompatible

" let $NVIM_SRAN_LOG_FILE = expand('~/sran-nvim.log')
" let $NVIM_SRAN_LOG_LEVEL = 'debug'
" let $NVIM_MKDP_LOG_FILE = expand('~/mkdp-log.log')
" let $NVIM_MKDP_LOG_LEVEL = 'debug'
" let $VIM_MKDP_RPC_LOG_FILE = expand('~/mkdp-rpc-log.log')
" let $VIM_MKDP_RPC_LOG_LEVEL = 'debug'


execute 'set rtp+=' . expand('<sfile>:p:h:h')
" execute 'set rtp+=' . expand('<sfile>:p:h:h:h') . '/coc.nvim'
