set nocompatible
filetype off                  " required
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" PLUGINS
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'msanders/snipmate.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'bling/vim-airline'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'easymotion/vim-easymotion' " acejump feature
Plugin 'terryma/vim-expand-region' " ctrl+w feature from emacs

call vundle#end()            " required
filetype plugin indent on

" TextMate’s “save on losing focus” 
au FocusLost * :wa

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Save
nmap <silent> <C-S> :update<CR> 

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
" set list
" set listchars=tab:▸\ ,eol:¬

" use w!! to sudo after editing file which required root priv
cmap w!! w !sudo tee % >/dev/null

if &t_Co >= 256 || has("gui_running")
    colorscheme mustang
endif

if &t_Co > 2 || has("gui_running")
" switch syntax highlighting on, when the terminal has colors
	syntax on
endif

""" HELPERS
" Here is a function and command to see a diff between the currently edited file and its unmodified version in the filesystem.
" To get out of diff view you can use the :diffoff command.
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

"
" File type specific settings
"
augroup WrapLineInMarkdownFile
    autocmd!
    autocmd FileType markdown setlocal nolist
    autocmd FileType markdown setlocal formatoptions=l
    autocmd FileType markdown setlocal linebreak
    autocmd FileType markdown setlocal wrap
    autocmd FileType markdown setlocal nonumber
augroup END
"
" PLUGIN SPECIFIC SETTINGS
"
let g:vim_markdown_folding_disabled=1   " do not fold text

" YouCompleteMe
let g:ycm_filetype_blacklist = {
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'markdown' : 1,
      \ 'md' : 1,
      \ 'mkd' : 1,
      \ 'unite' : 1,
      \ 'text' : 1,
      \ 'vimwiki' : 1,
      \ 'pandoc' : 1,
      \ 'infolog' : 1,
      \ 'mail' : 1
      \}

" Easy motion
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Bi-directional find motion
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
nmap s <Plug>(easymotion-s)
map  <Leader><Leader> <Plug>(easymotion-sn)
omap <Leader><Leader> <Plug>(easymotion-tn)

" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
