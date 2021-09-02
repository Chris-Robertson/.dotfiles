" --- Use 'gx' to follow any URL ---

" --- Plug Start ---
" https://github.com/junegunn/vim-plug
" Install plugin manager if it is missing
" commands: PlugInstall, PlugUpdate
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.vim/plugged')

" --- Auto-Completion
Plug 'jiangmiao/auto-pairs'
" Plug 'ternjs/tern_for_vim'
" Plug 'Valloric/YouCompleteMe'                   ", {'do':  './install.py --tern-completer'}

" --- Editing ---
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'https://github.com/ConradIrwin/vim-bracketed-paste'

" --- Fuzzy Search ---
Plug 'https://github.com/junegunn/fzf'
Plug 'https://github.com/junegunn/fzf.vim'

" --- Games ---
" Plug 'johngrib/vim-game-code-break'

" --- Linting ---
Plug 'w0rp/ale'

" --- Look & Feel
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" --- Org Mode ---
" Plug 'jceb/vim-orgmode'
Plug 'vim-scripts/utl.vim'
Plug 'https://github.com/tpope/vim-speeddating'
Plug 'https://github.com/inkarkat/vim-SyntaxRange'
Plug 'https://github.com/mattn/calendar-vim'
Plug 'https://github.com/majutsushi/tagbar'

" --- Ruby ---
" Plug 'nelstrom/vim-textobj-rubyblock'
" Plug 'tpope/vim-rails'
" Plug 'vim-ruby/vim-ruby'

" --- Snippets ---
" Plug 'https://github.com/SirVer/ultisnips'
Plug 'mattn/emmet-vim'
Plug 'vim-scripts/Emmet.vim'

" --- Syntax Highlighting - MISC ---
" Plug 'sheerun/vim-polyglot'
" Plug 'vim-scripts/WebAPI.vim'
" Plug 'othree/javascript-libraries-syntax.vim'
Plug 'elzr/vim-json'
" Plug 'digitaltoad/vim-pug'
" Plug 'https://github.com/othree/yajs.vim'
" Plug 'https://github.com/jelera/vim-javascript-syntax'
" Plug 'MaxMEllon/vim-jsx-pretty'
" Plug 'pangloss/vim-javascript'
" Plug 'https://github.com/othree/es.next.syntax.vim'

" --- Syntax Highlighting - YAML ---
"  https://github.com/stephpy/vim-yaml
Plug 'stephpy/vim-yaml'

" --- Text Objects ---
Plug 'christoomey/vim-sort-motion'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-user'

" --- TMUX ---
" Plug 'christoomey/vim-tmux-navigator'

" --- Tools ---
Plug 'https://github.com/junegunn/vim-plug'
Plug '907th/vim-auto-save'
" Plug 'rizzatti/dash.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-obsession'
Plug 'https://github.com/tpope/vim-eunuch'

" --- Trello ---
"  This needs vim compiled with python, haven't had a chance to sort that out
" Plug 'https://github.com/malithsen/trello-vim'

call plug#end()
" --- Plug End ---

syntax on

set nocompatible
filetype plugin indent on

set encoding=utf8
set autoindent
set backspace=indent,eol,start
set colorcolumn=80 " highlight column 80 for line char limits
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
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" autocmd Filetype ruby setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
" autocmd Filetype yaml setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

" --- Mappings ---
let mapleader = " "

inoremap kj <ESC>

nnoremap <leader>n :NERDTreeFind<CR>
nnoremap <leader>t :NERDTreeToggle<CR>
" remapped this to fix clash in org mode
" nnoremap <Tab><Tab> :NERDTreeToggle<CR>

" Don't remember what this was for, and it clashes with org mode
" nnoremap <leader>d /def<CR>

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
nnoremap <leader>p :FZF<cr>

" --- ALE ---

let g:ale_javascript_eslint_use_global = 1
let g:ale_javascript_eslint_executable = '/Users/chris/.nvm/versions/node/v7.10.0/bin/eslint'
let g:ale_fixers = {'javascript': ['eslint']}

" --- Look & Feel ---
" set termguicolors "Fucks colours in TMUX
colorscheme gruvbox
set bg=dark

let g:airline_powerline_fonts = 1 "enable powerline font
let g:airline_theme='base16'

" Other options "
"""""""""""""""""
let g:jsx_ext_required = 0 
let g:javascript_plugin_flow = 1


" --- NERDCommenter options ---
"  I think tpope's plugin supercedes all this
"" Add spaces after comment delimiters by default
"let g:NERDSpaceDelims = 1
"" Use compact syntax for prettified multi-line comments
"let g:NERDCompactSexyComs = 1
"" Align line-wise comment delimiters flush left instead of following code indentation
"let g:NERDDefaultAlign = 'left'
"" Set a language to use its alternate delimiters by default
""let g:NERDAltDelims_java = 1
"" Add your own custom formats or override the defaults
""let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
"" Allow commenting and inverting empty lines (useful when commenting a region)
"let g:NERDCommentEmptyLines = 1
"" Enable trimming of trailing whitespace when uncommenting
"let g:NERDTrimTrailingWhitespace = 1

" Sets the sort-motion plugin to be case-insensitive
let g:sort_motion_flags = "i"

" Use a custom emmet snippets file
" let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.snippets.json')), "\n"))

" enable AutoSave on Vim startup
let g:auto_save = 1

" --- Inspirational Dotfiles ---
" https://github.com/kmARC/vim/blob/master/vimrc
