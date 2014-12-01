set nocompatible
filetype off                  " required
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" PLUGINS
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'msanders/snipmate.vim'

call vundle#end()            " required
filetype plugin indent on

" TextMate’s “save on losing focus” 
au FocusLost * :wa

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" you can have unwritten changes to a file and open a new file using :e,
" without being forced to write or undo your changes first. Also, undo buffers
" and marks are preserved while the buffer is open
set hidden

set nowrap        " don't wrap lines
set shiftwidth=4  " number of spaces to use for autoindenting
set tabstop=4     " a tab is four spaces
set softtabstop=4
set expandtab
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set number        " always show line numbers
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                    "    case-sensitive otherwise
set smarttab      " insert tabs on the start of a line according to
		  "    shiftwidth, not tabstop
set hlsearch      " highlight search terms
set incsearch     " show search	matches as you type

set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep
set pastetoggle=<F2>	 " temporarily switch to “paste mode”
set encoding=utf-8
set ruler

" show invisible characters with the same characters that TextMate uses
set list
set listchars=tab:▸\ ,eol:¬

" use w!! to sudo after editing file which required root priv
cmap w!! w !sudo tee % >/dev/null

if &t_Co >= 256 || has("gui_running")
    colorscheme mustang
endif

if &t_Co > 2 || has("gui_running")
" switch syntax highlighting on, when the terminal has colors
	syntax on
endif

