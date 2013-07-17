" .vimrc

set nocompatible " I want Vim, not Vi
syntax on
filetype plugin indent on

" Appearance
set background=light
colorscheme zenburn
set guifont=Ubuntu\ Mono\ 11
"set guifont=Bitstream\ Vera\ Sans\ Mono\ 9 " Font of Bitbucket

" General
set autowrite    " Automatically write buffer before special actions
set completeopt=menu,longest " Always show the menu, insert longest match
set gdefault     " Make substitution flag 'g' is default on
set guioptions=
set switchbuf=useopen,usetab,split " Want better buffer handling in quickfix mode
set tabpagemax=9 " At most 9 tabs open

" Visual aids
set listchars=tab:▸\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOLs
set mousehide    " Hide mouse when typing
set number       " Show line number
set ruler
set scrolloff=3  " Maintain some more context around the cursor
set showcmd      " Show (partial) command in the bottom
set showmatch    " Show matching braces
set showmode

" Edit area
set textwidth=80
set colorcolumn=81
set columns=180
set lines=40

" Tab and indentation
set expandtab    " Tabs are evil
set smarttab     " We want fancy tab handling
set shiftwidth=2
set autoindent
set smartindent

" Search
set hlsearch     " Highlight matches
set incsearch    " Incremental search
set ignorecase
set smartcase

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
let mapleader = ","
map <Leader>h :nohlsearch<CR>
map <leader>l :set list!<CR>
map <Leader>o :browse oldfiles<CR>
map <Leader>r :! rm -rf *~ *.swp *.hi *.o *.exe *.native *.byte<CR>
map <Leader>p :! git push origin master
map <Leader>c :! git add . && git commit -m '

" Function shortcuts
call Map('<F1>', ':help<Space>')
call Map('<F4>', ':tabedit ~/Dropbox/Dev/dotfiles/.vimrc<CR>')
call Map('<F5>', ':source  ~/Dropbox/Dev/dotfiles/.vimrc<CR>')
call Map('<F6>', ':! sudo  ~/Dropbox/Dev/dotfiles/setup.sh<CR>')
call Map('<F9>', ':w<CR>:make<CR>')
au FileType ocaml   set makeprg=ocamlbuild\ -use-ocamlfind\ -cflags\ '-warn-error\ A'\ '%<.native'
au FileType haskell set makeprg=ghc\ -Wall\ '%'\ -o\ '%<.exe'
au FileType ocaml   call Map('<F10>', ':! ''./%<.native''<Space>')
au FileType haskell call Map('<F10>', ':! ''./%<.exe''<Space>')

" Ctrl shortcuts
call Map('<C-o>'  , ':e<Space><Tab>')
call Map('<C-s>'  , ':w<CR>')
call Map('<C-q>'  , ':q<CR>')
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

" Change directories automatically
au BufEnter * lcd %:p:h

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

" Remove Trailing Spaces
" http://vim.wikia.com/wiki/Remove_unwanted_spaces
au BufWritePre * :%s/\s\+$//e

" Our shell code looks like a scheme programmer made up all the names
au FileType sh set iskeyword=~,@,48-57,_,192-255,-

" Editor exceptions for plain texts
au FileType text set textwidth=0 nonumber nocursorline

" ocp-indent
au FileType ocaml source /home/zshi/.opam/4.01.0dev+trunk/share/typerex/ocp-indent/ocp-indent.vim
