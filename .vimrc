" --- Use 'gx' to follow any URL ---

" --- Plug Start ---
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')

" --- Fuzzy Search ---
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" --- Tools ---
Plug 'https://github.com/junegunn/vim-plug'
Plug '907th/vim-auto-save'

Plug 'christoomey/vim-sort-motion'
Plug 'christoomey/vim-tmux-navigator'
Plug 'jiangmiao/auto-pairs'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-user'
Plug 'mattn/emmet-vim'
Plug 'morhetz/gruvbox'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'rizzatti/dash.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rails'
Plug 'Valloric/YouCompleteMe'
Plug 'ternjs/tern_for_vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-ruby/vim-ruby'
" Plug 'vim-syntastic/syntastic'
" Plug 'leafgarland/typescript-vim'
" Plug 'ianks/vim-tsx'
" Plug 'Quramy/tsuquyomi'
Plug 'pangloss/vim-javascript'
" Plug 'mxw/vim-jsx'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'vim-scripts/Emmet.vim'
Plug 'vim-scripts/WebAPI.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'elzr/vim-json'
Plug 'digitaltoad/vim-pug'
Plug 'w0rp/ale'
Plug 'tpope/vim-obsession'

call plug#end()
" --- Plug End ---

set nocompatible              " be iMproved, required
filetype on                  " required
filetype indent on
filetype plugin on

" export PATH="$PATH:$HOME/.rvm/bin"
set encoding=utf8

" Leader Key "
""""""""""""""
let mapleader = " "

" Leader Commands "
"""""""""""""""""""
" copy line, paste it below and change every character to "
nnoremap <leader>1 yypVr
" Close syntastic error window
nnoremap <leader>cl :lclose<CR>
nnoremap <leader>n :NERDTreeFind<CR>
nnoremap <Tab><Tab> :NERDTreeToggle<CR>
nnoremap <leader>d /def<CR>

nnoremap <leader>sn :lnext<cr>
nnoremap <leader>sp :lprev<cr>

nnoremap <leader>f :FZF<cr>

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
let g:airline_theme='base16'

" Tabs "
""""""""
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

" Language specific Tabs "
""""""""""""""""""""""""""
" autocmd Filetype ruby setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

" Other options "
"""""""""""""""""
set autoindent
set backspace=indent,eol,start
set cursorline
set cursorcolumn
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

set shortmess=
set cmdheight=2

" prevents security exploits with modelines
set modelines=0

" turn on syntax highlighting
syntax on
let g:jsx_ext_required = 0 
let g:javascript_plugin_flow = 1

" Syntastic options "
"""""""""""""""""""""
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" let g:syntastic_always_populate_loc_list    = 1
" let g:syntastic_auto_loc_list               = 1
" let g:syntastic_check_on_open               = 1
" let g:syntastic_check_on_wq                 = 0

" let g:syntastic_ruby_checkers               = ['rubocop', 'mri']
" let g:syntastic_javascript_checkers         = ['eslint']
" "let g:syntastic_ruby_rubocop_exec           = '/Users/chris/.rbenv/shims/rubocop'
" let g:syntastic_enable_signs                = 1
" let g:syntastic_auto_jump                   = 0

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

" Sets the sort-motion plugin to be case-insensitive
let g:sort_motion_flags = "i"

"This unsets the 'last search pattern' register by hitting return
nnoremap <CR> :noh<CR><CR>

" Lets me use space-e to expand emmet abbreviations
imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
nmap <leader>e <C-y>,i

" Use a custom emmet snippets file
let g:user_emmet_settings = webapi#json#decode(
\  join(readfile(expand('~/.snippets.json')), "\n"))

" enable AutoSave on Vim startup
let g:auto_save = 1
