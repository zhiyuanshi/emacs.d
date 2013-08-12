" .vimrc

set nocompatible " I want Vim, not Vi

" Required by Vundle
filetype off
set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" My Bundles
Bundle 'Solarized'
Bundle 'Zenburn'
Bundle 'The-NERD-tree'
Bundle 'mru.vim'
Bundle 'TwitVim'

" Required by Vundle
filetype plugin indent on

" Appearance
syntax on
set t_Co=256
set background=light
colorscheme solarized
set guifont=Ubuntu\ Mono\ 13

" General
"set autochdir   " Replaced by 'lcd %:p:h', which is purported to be better
"set autowrite   " Automatically write buffer before special actions
set completeopt=menu,longest " Always show the menu, insert longest match
set guioptions=
set switchbuf=useopen,usetab,split " Want better buffer handling in quickfix mode

" Edit area
set textwidth=80
set colorcolumn=+1 " Highlight column after 'textwidth'
set columns=100
set lines=40

" Visual aids
set cursorline
set list
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
set tabstop=8    "A tab is 8 spaces
set expandtab    " Tabs are evil
set smarttab     "Indent instead of tab at start of line
set shiftwidth=2
set autoindent
set smartindent
set nojoinspaces "Don't convert spaces to tabs
au FileType c,cpp,python set shiftwidth=4
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
au FileType ocaml map <LocalLeader>a Iassert (<Esc>A);<Esc>

map <Leader>a ggvG$
map <Leader>b :tabedit ~/Dropbox/Dev/zhiyuanshi/dotfiles/.bashrc.append<CR>
map <Leader>c :!~/Dropbox/Dev/zhiyuanshi/scripts/cleanup.sh<CR>
map <Leader>e :e<Space><Tab>
map <Leader>h :nohlsearch<CR>
map <Leader>l :set list!<CR>
map <Leader>m :MRU<CR>
map <Leader>n :NERDTreeToggle<CR>
map <Leader>o :tabedit ~/Dropbox/org.org<CR>
" Leave editor quickly (when saved)
map <Leader>q :q<CR>
"map <Leader>t :e ~/Dropbox/tweets.txt<CR>
map <Leader>tu :UserTwitter<CR>
map <Leader>tf :FriendsTwitter<CR>
map <Leader>tp :CPosttoTwitter<CR>
map <Leader>v :e ~/Dropbox/Dev/zhiyuanshi/dotfiles/.vimrc<CR>
map <Leader>w :w<CR>

"map <Leader>gc :!git add . && git commit -m '
"map <Leader>gb :!git push bitbucket master
"map <Leader>gg :!git push github master

" Function shortcuts
call Map('<F1>', ':help<Space>')
call Map('<F9>', ':w<CR>:make<CR>')
au FileType ocaml   set makeprg=ocamlbuild\ -use-ocamlfind\ -cflags\ '-warn-error\ A'\ '%<.native'
au FileType haskell set makeprg=ghc\ --make\ -Wall\ '%'\ -o\ '%<.exe'
au FileType c       set makeprg=gcc\ '%'\ -o\ '%<.out'
au FileType cpp     set makeprg=g++\ '%'\ -o\ '%<.out'
au FileType ocaml   call Map('<F10>', ':!''./%<.native''<Space>')
au FileType haskell call Map('<F10>', ':!''./%<.exe''<Space>')
au FileType python  call Map('<F10>', ':!python ''%''<Space>')
au FileType ruby    call Map('<F10>', ':!ruby ''%''<Space>')
au FileType c,cpp   call Map('<F10>', ':!''./%<.out''<Space>')
au FileType sh      call Map('<F10>', ':!chmod +x ''%'' && ''./%''<Space>')
au FileType vim     call Map('<F10>', ':source %<CR>')

" Buffers
"set hidden
"call Map('<C-Tab>', ':bnext<CR>')

" Tabs
set tabpagemax=9 " At most 9 tabs open
call Map('<C-n>'  , ':tabnew<CR>')
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
au BufWritePost .vimrc         :!~/Dropbox/Dev/zhiyuanshi/scripts/setup.sh d
au BufWritePost .bashrc.append :!~/Dropbox/Dev/zhiyuanshi/scripts/setup.sh d

" Our shell code looks like a scheme programmer made up all the names
au FileType sh set iskeyword=~,@,48-57,_,192-255,-

" ocp-indent
au FileType ocaml source ~/.opam/4.01.0dev+trunk/share/typerex/ocp-indent/ocp-indent.vim
