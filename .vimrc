set nocompatible              " be iMproved, required
filetype on                  " required
filetype indent on
filetype plugin on

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Plugin 'scrooloose/nerdcommenter'
Plugin 'christoomey/vim-sort-motion'
" Plugin 'christoomey/vim-system-copy'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-line'
Plugin 'kana/vim-textobj-user'
Plugin 'mattn/emmet-vim'
Plugin 'morhetz/gruvbox'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'rizzatti/dash.vim'
Plugin 'scrooloose/nerdtree'
" Plugin 'svermeulen/vim-easyclip'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'Valloric/YouCompleteMe'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'vim-ruby/vim-ruby'
Plugin 'vim-syntastic/syntastic'

" All of your Plugins must be added before the following line
call vundle#end()            " required

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal

" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Leader Key "
""""""""""""""
let mapleader = ","

" Leader Commands "
"""""""""""""""""""
" copy line, paste it below and change every character to "
nnoremap <leader>1 yypVr
" Close syntastic error window
nnoremap <leader>cl :lclose<CR>
nnoremap <leader>n :NERDTree<CR>
nnoremap <leader>d /def<CR>

nnoremap <leader>sn :lnext<cr>
nnoremap <leader>sp :lprev<cr>

" Keymappings "
"""""""""""""""
" escape
inoremap jj <ESC>

" remove arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

" Colour scheme "
"""""""""""""""""
colorscheme gruvbox

" Airline status bar theme "
""""""""""""""""""""""""""""
let g:airline_powerline_fonts = 1 "enable powerline font
let g:airline_theme='cobalt2'

" Tabs "
""""""""
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Language specific Tabs "
""""""""""""""""""""""""""
autocmd Filetype ruby setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

" Other options "
"""""""""""""""""
set autoindent
set backspace=indent,eol,start
set cursorline
set encoding=utf-8
set hidden
set hlsearch
set laststatus=2
set relativenumber
set number
set ruler
set scrolloff=3
set showcmd
set showmode
set ttyfast
set undofile
set visualbell
set wildmenu
set wildmode=list:longest

" prevents security exploits with modelines
set modelines=0

" turn on syntax highlighting
syntax on

" Syntastic options "
"""""""""""""""""""""
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list    = 1
let g:syntastic_auto_loc_list               = 1
let g:syntastic_check_on_open               = 1
let g:syntastic_check_on_wq                 = 0

let g:syntastic_ruby_checkers               = ['rubocop', 'mri']
let g:syntastic_javascript_checkers         = ['jshint']
"let g:syntastic_ruby_rubocop_exec           = '/Users/chris/.rbenv/shims/rubocop'
let g:syntastic_enable_signs                = 1
let g:syntastic_auto_jump                   = 0

" NERDCommenter options "
"""""""""""""""""""""""""
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Set a language to use its alternate delimiters by default
"let g:NERDAltDelims_java = 1
" Add your own custom formats or override the defaults
"let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
"

" Sets the sort-motion plugin to be case-insensitive
let g:sort_motion_flags = "i"

"This unsets the 'last search pattern' register by hitting return
nnoremap <CR> :noh<CR><CR>
