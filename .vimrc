" --- Use 'gx' to follow any URL ---

" --- Plug Start ---
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')

" --- Auto-Completion
Plug 'jiangmiao/auto-pairs'
Plug 'ternjs/tern_for_vim'
Plug 'Valloric/YouCompleteMe'                   , {'do':  './install.py --tern-completer'}

" --- Editing ---
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" --- Fuzzy Search ---
Plug 'https://github.com/junegunn/fzf'
Plug 'https://github.com/junegunn/fzf.vim'

" --- Linting ---
Plug 'w0rp/ale'

" --- Look & Feel
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" --- Ruby ---
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'tpope/vim-rails'
Plug 'vim-ruby/vim-ruby'

" --- Snippets ---
Plug 'mattn/emmet-vim'
Plug 'vim-scripts/Emmet.vim'

" --- Syntax Highlighting ---
Plug 'pangloss/vim-javascript'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'vim-scripts/WebAPI.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'elzr/vim-json'
Plug 'digitaltoad/vim-pug'

" --- Text Objects ---
Plug 'christoomey/vim-sort-motion'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-user'

" --- TMUX ---
Plug 'christoomey/vim-tmux-navigator'

" --- Tools ---
Plug 'https://github.com/junegunn/vim-plug'
Plug '907th/vim-auto-save'
Plug 'rizzatti/dash.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-obsession'

call plug#end()
" --- Plug End ---

syntax on

set nocompatible
filetype plugin indent on

set encoding=utf8
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
set modelines=0 " prevents security exploits with modelines

" --- Tabs ---
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
" autocmd Filetype ruby setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

" --- Mappings ---
let mapleader = " "

inoremap jj <ESC>

nnoremap <leader>n :NERDTreeFind<CR>
nnoremap <Tab><Tab> :NERDTreeToggle<CR>

nnoremap <leader>d /def<CR>

nnoremap <leader>cl :lclose<CR>
nnoremap <leader>sn :lnext<cr>
nnoremap <leader>sp :lprev<cr>

nnoremap j gj
nnoremap k gk

" This unsets the 'last search pattern' register by hitting return
nnoremap <CR> :noh<CR><CR>

" Lets me use space-e to expand emmet abbreviations
" imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
nmap <leader>e <C-y>,i

" --- FZF ---
nnoremap <leader>f :FZF<cr>

" --- Look & Feel ---
colorscheme gruvbox

let g:airline_powerline_fonts = 1 "enable powerline font
let g:airline_theme='base16'


" Other options "
"""""""""""""""""
let g:jsx_ext_required = 0 
let g:javascript_plugin_flow = 1

" --- Syntastic options ---
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

" --- NERDCommenter options ---
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

" Use a custom emmet snippets file
let g:user_emmet_settings = webapi#json#decode(
\  join(readfile(expand('~/.snippets.json')), "\n"))

" enable AutoSave on Vim startup
let g:auto_save = 1

" --- Inspirational Dotfiles ---
" https://github.com/kmARC/vim/blob/master/vimrc
