" .vimrc

set nocompatible " I want Vim, not Vi

" Required by Vundle
filetype off
set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" My Bundles here:
Bundle 'The-NERD-tree'
Bundle 'The-NERD-Commenter'
Bundle 'Solarized'
Bundle 'Zenburn'

" Required by Vundle
filetype plugin indent on

" Appearance
syntax on
set background=dark
colorscheme solarized
set guifont=Ubuntu\ Mono\ 12
"set guifont=Bitstream\ Vera\ Sans\ Mono\ 10 " Font of Bitbucket

" General
"set autochdir   " Replaced by 'lcd %:p:h', which is purported to be better
"set autowrite   " Automatically write buffer before special actions
set completeopt=menu,longest " Always show the menu, insert longest match
set guioptions=
set switchbuf=useopen,usetab,split " Want better buffer handling in quickfix mode
set tabpagemax=9 " At most 9 tabs open

" Edit area
set textwidth=80
set colorcolumn=+1 " Highlight column after 'textwidth'
set columns=120
set lines=31

" Visual aids
set listchars=tab:▸\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOLs
set mousehide    " Hide mouse when typing
set number       " Show line number
set ruler
set scrolloff=3  " Maintain some more context around the cursor
set showcmd      " Show (partial) command in the bottom
set showmatch    " Show matching braces
set showmode
au FileType text set textwidth=0 nonumber nocursorline

" Tab and indentation
set expandtab    " Tabs are evil
set smarttab     " We want fancy tab handling
set shiftwidth=4
au FileType ocaml,haskell set shiftwidth=2
set autoindent
set smartindent
au FileType c,cpp set cindent

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
au FileType ocaml map <Leader>a Iassert (<Esc>A);<Esc>
map <Leader>h :nohlsearch<CR>
map <Leader>l :set list!<CR>
map <Leader>n :NERDTreeToggle<CR>
map <Leader>o :browse oldfiles<CR>
map <Leader>r :! rm -rf *~ *.swp *.hi *.o *.exe *.native *.byte *.out<CR>
map <Leader>v :tabedit ~/Dropbox/Dev/zhiyuanshi/dotfiles/.vimrc<CR>')
"map <Leader>gc :! git add . && git commit -m '
"map <Leader>gb :! git push bitbucket master
"map <Leader>gg :! git push github master

" Function shortcuts
call Map('<F1>', ':help<Space>')
call Map('<F6>', ':! sudo  ~/Dropbox/Dev/zhiyuanshi/dotfiles/setup.sh<CR>')
call Map('<F9>', ':w<CR>:make<CR>')
au FileType ocaml   set makeprg=ocamlbuild\ -use-ocamlfind\ -cflags\ '-warn-error\ A'\ '%<.native'
au FileType haskell set makeprg=ghc\ -Wall\ '%'\ -o\ '%<.exe'
au FileType c       set makeprg=gcc\ '%'\ -o\ '%<.out'
au FileType cpp     set makeprg=g++\ '%'\ -o\ '%<.out'
au FileType ocaml   call Map('<F10>', ':! ''./%<.native''<Space>')
au FileType haskell call Map('<F10>', ':! ''./%<.exe''<Space>')
au FileType python  call Map('<F10>', ':! python ''%''<Space>')
au FileType c,cpp   call Map('<F10>', ':! ''./%<.out''<Space>')
au FileType vim     call Map('<F10>', ':source %<CR>')

" Ctrl shortcuts
call Map('<C-o>'  , ':e<Space><Tab>')
call Map('<C-s>'  , ':w<CR>')
call Map('<C-q>'  , ':q<CR>')           " Leave editor quickly (when saved)
call Map('<C-a>'  , 'ggvG$')
call Map('<C-t>'  , ':tabnew<CR>')
call Map('<C-Tab>', ':tabnext<CR>')
call Map('<C-F4>' , ':tabclose<CR>')

" Easy window navigation
call Map('<C-h>', '<C-w>h')
call Map('<C-j>', '<C-w>j')
call Map('<C-k>', '<C-w>k')
call Map('<C-l>', '<C-w>l')

" Go to tab n
call Map('<M-1>', '1gt')
call Map('<M-2>', '2gt')
call Map('<M-3>', '3gt')
call Map('<M-4>', '4gt')
call Map('<M-5>', '5gt')
call Map('<M-6>', '6gt')
call Map('<M-7>', '7gt')
call Map('<M-8>', '8gt')
call Map('<M-9>', '9gt')

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
au VimEnter * if (argc() == 0) | :NERDTree %:p:h | endif
let NERDTreeIgnore=['^_build$', '^_tags$']

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

" Source the vimrc file after saving it
au BufWritePost .vimrc source $MYVIMRC

" Our shell code looks like a scheme programmer made up all the names
au FileType sh set iskeyword=~,@,48-57,_,192-255,-

" ocp-indent
au FileType ocaml source ~/.opam/4.01.0dev+trunk/share/typerex/ocp-indent/ocp-indent.vim
