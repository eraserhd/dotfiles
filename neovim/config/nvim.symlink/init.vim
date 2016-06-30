
execute pathogen#infect()
syntax on
filetype plugin indent on

set background=dark
set nocompatible
set number
set autoread

" Get swap files out of the way
set directory=/private/tmp//,/tmp//

" Termainal config
let g:terminal_scrollback_buffer_size = 10000
tnoremap <Esc> <C-\><C-n>
