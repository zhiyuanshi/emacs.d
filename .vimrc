" .vimrc

set nocompatible " I want Vim, not Vi

" Required by Vundle
filetype off
set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" My Bundles
Bundle 'Guardian'
Bundle 'molokai'
Bundle 'Solarized'
Bundle 'Zenburn'
Bundle 'brettof86/vim-codeschool'
Bundle 'noahfrederick/vim-hemisu'
Bundle 'mru.vim'
Bundle 'The-NERD-tree'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-ruby/vim-ruby'
Bundle 'indenthaskell.vim'
Bundle 'Haskell-Highlight-Enhanced'
Bundle 'derekwyatt/vim-scala'
Bundle 'taglist.vim'

" Required by Vundle
filetype plugin indent on

" Prevent duplicate autocommands
augroup au
au!

" Appearance
syntax on
set t_Co=256
set guifont=Ubuntu\ Mono\ 14
"set guifont=Monospace\ 10
set background=dark
colorscheme molokai
"set cursorline

" General
"set autochdir   " Replaced by 'lcd %:p:h', which is purported to be better
"set autowrite   " Automatically write buffer before special actions
set shell=/bin/bash
set completeopt=menu,longest " Always show the menu, insert longest match
set nowrap
set guioptions=
set switchbuf=useopen,usetab,split " Want better buffer handling in quickfix mode

" Edit area
set textwidth=80
set colorcolumn=+1 " Highlight column after 'textwidth'
set columns=120
set lines=40

" Folding
set nofoldenable
set foldmethod=indent
" Space to toggle folds
map <Space> za

" Visual aids
"set cursorline
"set list
set listchars=tab:¿\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOL
set mousehide    " Hide mouse when typing
set number       " Show line number
set ruler
set scrolloff=3  " Maintain some more context around the cursor
set showcmd      " Show (partial) command in the bottom
set showmatch    " Show matching braces
set showmode

" Tab and indentation
set tabstop=8    "A tab is 8 spaces
set expandtab    " Tabs are evil
set smarttab     "Indent instead of tab at start of line
set shiftwidth=2
set autoindent
set smartindent
set nojoinspaces "Don't convert spaces to tabs

" Search and substitution
set hlsearch     " Highlight matches
set incsearch    " Incremental search
set ignorecase
set smartcase
set gdefault     " Make substitution flag 'g' is default on

set wildcharm=<Tab> " Let <Tab> be recognized when used inside a macro
set wildignore+=*~,*.native,*.byte,*.hi,*.pyc,*.o
set wildmenu        " Expand the command line using tab
set wildmode=list:longest,full

" We want to have a 'general' map
function! Map(lhs, rhs)
  execute 'map'  a:lhs           a:rhs
  execute 'imap' a:lhs '<Esc>' . a:rhs
endfunction

" Leader shortcuts
" , is a more convenient leader than \
let mapleader = ","
let maplocalleader = ",,"

map <Leader>a ggvG$
map <Leader>b :tabedit ~/Dropbox/Dev/dotfiles/.bashrc.append<CR>
map <Leader>c :!~/Dropbox/Dev/scripts/clean.sh<CR>
map <Leader>e :e<Space><Tab>
map <Leader>f :set foldenable! foldenable?<CR>
map <Leader>m :MRU<CR>
map <Leader>n :NERDTreeToggle<CR>
map <Leader>o :tabedit ~/Dropbox/org.org<CR>
map <Leader>q :q<CR>
"map <Leader>s :source %<CR>
map <Leader>t :TlistToggle<CR>
map <Leader>v :tabedit ~/Dropbox/Dev/dotfiles/.vimrc<CR>
map <Leader>w :w<CR>

"map <Leader>gc :!git add . && git commit -m '
"map <Leader>gb :!git push bitbucket master
"map <Leader>gg :!git push github master

" Function shortcuts
call Map('<F1>', ':help ')
call Map('<F2>', ':e ')
call Map('<F3>', ':w<CR>')
call Map('<F4>', ':colorscheme ')
call Map('<F8>', ':!')
call Map('<F9>', ':w<CR>:make<CR>')

au BufEnter * if &filetype == 'text' | set nonumber wrap | else | set number nowrap | endif

function! SetShiftWidth()
  if &filetype == 'c' || &filetype == 'cpp' || &filetype == 'java'
    set shiftwidth=4

  elseif &filetype == 'javascript' || &filetype == 'xml'
    set shiftwidth=4

  elseif &filetype == 'python' || &filetype == 'ruby' || &filetype == 'sh'
    set shiftwidth=2

  elseif &filetype == 'haskell' || &filetype == 'ocaml'
    set shiftwidth=2

  elseif &filetype == 'vim'
    set shiftwidth=2

  elseif &filetype == 'r'
    set shiftwidth=2

  endif
endfunction

au BufEnter * call SetShiftWidth()

function! SetIndent()
  if &filetype == 'c' || &filetype == 'cpp'
    set cindent

  elseif &filetype == 'ocaml'
    source ~/.opam/4.01.0/share/typerex/ocp-indent/ocp-indent.vim
  endif
endfunction

au BufEnter * call SetIndent()

function! SetMakePrg()
  if &filetype == 'c'
    set makeprg=gcc\ %\ -o\ %<.out

  elseif &filetype == 'cpp'
    set makeprg=g++\ %\ -o\ %<.out

  elseif &filetype == 'java'
    set makeprg=javac\ %

  elseif &filetype == 'scala'
    set makeprg=scalac\ %

  elseif &filetype == 'ocaml'
    set makeprg=ocamlbuild\ -use-ocamlfind\ -cflags\ '-warn-error\ A'\ %<.native

  elseif &filetype == 'haskell'
    set makeprg=ghc\ --make\ -Wall\ %\ -o\ %<.exe

  endif
endfunction

au BufEnter * call SetMakePrg()

function! SetRun()
  if &filetype == 'c' || &filetype == 'cpp'
    call Map('<F10>', ':!./%<.out<Space>')

  elseif &filetype == 'java'
    call Map('<F10>', ':!java %<<Space>')

  elseif &filetype == 'scala'
    call Map('<F10>', ':!scala %<<Space>')

  elseif &filetype == 'haskell'
    call Map('<F10>', ':!./%<.exe<Space>')

  elseif &filetype == 'ocaml'
    call Map('<F10>', ':!./%<.native<Space>')

  elseif &filetype == 'python' || &filetype == 'ruby' || &filetype == 'sh'
    call Map('<F10>', ':!chmod +x % && ./%<Space>')

  elseif &filetype == 'vim'
    call Map('<F10>', ':source %<CR>')

  endif
endfunction

au BufEnter * call SetRun()

" Soft wrap plain text
au BufEnter * if &filetype == 'markdown' || &filetype == 'text' | set wrap linebreak nolist | endif

vmap <C-j> gj
vmap <C-k> gk
vmap <C-4> g$
vmap <C-6> g^
vmap <C-0> g^
nmap <C-j> gj
nmap <C-k> gk
nmap <C-4> g$
nmap <C-6> g^
nmap <C-0> g^
"

" Buffers
"set hidden
"call Map('<C-Tab>', ':bnext<CR>')

" Tabs
set tabpagemax=9 " At most 9 tabs open
call Map('<C-t>'  , ':tabnew<CR>')
call Map('<C-Tab>', ':tabnext<CR>')
call Map('<C-F4>' , ':tabclose<CR>')

" Move around in insert mode
imap <C-h> <Left>
imap <C-j> <Up>
imap <C-k> <Down>
imap <C-l> <Right>

" Disable arrows
map <Up>    <Nop>
map <Down>  <Nop>
map <Left>  <Nop>
map <Right> <Nop>

" Auto-completion
" http://vim.wikia.com/wiki/Smart_mapping_for_tab_completion
function! SmartTab()
  " Treat <Tab> as a <Tab> if there is no character or just a <Space>
  " before the cursor. Otherwise treat <Tab> as the key for completion.
  let c =  strpart(getline("."), col(".") - 2, 1)
  if (c == "" || c == " ")
    return "\<Tab>"
  else
    return "\<C-n>"
  endif
endfunction

imap <Tab> <C-r>=SmartTab()<CR>

" Open a NERDTree automatically when Vim starts up if no files were specified
" Also change current directory to Dev
"au VimEnter * if (argc() == 0) | :NERDTree %:p:h | endif
let NERDTreeIgnore=['^_build$', '^_tags$', '\.native$', '\.exe$']

" Close Vim if the only window left open is a NERDTree
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Change directories automatically and print the directory after changing
au BufEnter * :lchdir %:p:h

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

" Remove Trailing Spaces
" http://vim.wikia.com/wiki/Remove_unwanted_spaces
au BufWritePre * :%s/\s\+$//e

" Copy the vimrc file to home after saving it
au BufWritePost .vimrc         :!cp % ~
au BufWritePost .bashrc.append :!cp /etc/skel/.bashrc ~ && cat % >> ~/.bashrc

" http://vim.wikia.com/wiki/Smart_mapping_for_tab_completion
function! SmartTab()
  " Treat <Tab> as a <Tab> if there is no character or just a <Space>
  " before the cursor. Otherwise treat <Tab> as the key for completion.
  let c =  strpart(getline("."), col(".") - 2, 1)
  if (c == "" || c == " ")
    return "\<Tab>"
  else
    return "\<C-n>"
  endif
endfunction

imap <Tab> <C-r>=SmartTab()<CR>

" Open a NERDTree automatically when Vim starts up if no files were specified
" Also change current directory to Dev
"au VimEnter * if (argc() == 0) | :NERDTree %:p:h | endif
let NERDTreeIgnore=['^_build$', '^_tags$', '\.native$', '\.exe$']

" Close Vim if the only window left open is a NERDTree
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Change directories automatically and print the directory after changing
au BufEnter * :lchdir %:p:h

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

" Remove Trailing Spaces
" http://vim.wikia.com/wiki/Remove_unwanted_spaces
au BufWritePre * :%s/\s\+$//e

" Copy the vimrc file to home after saving it
au BufWritePost .vimrc         :!cp % ~
au BufWritePost .bashrc.append :!cp /etc/skel/.bashrc ~ && cat % >> ~/.bashrc

augroup end
