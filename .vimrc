" .vimrc

set nocompatible " I want Vim, not Vi
filetype plugin indent off

"-------------------------------------------------------------------------------
"       #Vundle
"-------------------------------------------------------------------------------
" Required by Vundle
filetype off
set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" vim-textobj-rubyblock requires that the matchit.vim plugin is enabled.
" https://github.com/nelstrom/vim-textobj-rubyblock
runtime macros/matchit.vim

" My Bundles
Bundle 'Shougo/vimproc.vim'
Bundle 'The-NERD-tree'
" Bundle 'airblade/vim-gitgutter'
Bundle 'jistr/vim-nerdtree-tabs'
Bundle 'justinmk/vim-syntax-extra'
Bundle 'kana/vim-textobj-user'
Bundle 'kien/ctrlp.vim'
Bundle 'mattn/emmet-vim'
Bundle 'mhinz/vim-signify'
Bundle 'msanders/snipmate.vim'
Bundle 'scrooloose/syntastic'
" Bundle 'taglist.vim'
Bundle 'thoughtbot/vim-rspec'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tsaleh/vim-matchit'

" Bundle 'Shougo/neocomplete.vim'
" let g:neocomplete#enable_at_startup = 1

" Haskell
" Bundle 'Twinside/vim-haskellConceal'
" Bundle 'bitc/vim-hdevtools'
" Bundle 'eagletmt/ghcmod-vim'
Bundle 'dag/vim2hs'
Bundle 'vim-scripts/hlint'

" Ruby and Rails
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-rails'
Bundle 'vim-ruby/vim-ruby'

" Rust
Bundle 'wting/rust.vim'

" Scala
Bundle 'derekwyatt/vim-scala'

" CoffeeScript
Bundle 'kchmck/vim-coffee-script'

" Colorschemes
Bundle 'Guardian'
Bundle 'Solarized'
Bundle 'Zenburn'
Bundle '29decibel/codeschool-vim-theme'
Bundle 'endel/vim-github-colorscheme'
Bundle 'jpo/vim-railscasts-theme'
Bundle 'molokai'
Bundle 'morhetz/gruvbox'
Bundle 'nanotech/jellybeans.vim'
Bundle 'noahfrederick/vim-hemisu'
Bundle 'w0ng/vim-hybrid'
Bundle 'zeis/vim-kolor'

" Required by Vundle
filetype plugin indent on

"-------------------------------------------------------------------------------
"       #General settings
"-------------------------------------------------------------------------------
" Appearance
syntax on
set t_Co=256
set guifont=Ubuntu\ Mono\ 15
" set guifont=Monospace\ 12
set background=dark
colorscheme guardian

" General
" set autochdir   " Replaced by 'lcd %:p:h', which is purported to be better
" set autowrite   " Automatically write buffer before special actions
set shell=/bin/bash
set completeopt=menu,longest " Always show the menu, insert longest match
set nowrap " Switch wrap off for everything
set guioptions=
set switchbuf=useopen,usetab,split " Want better buffer handling in quickfix mode
let @/=''       " Get rid of the annoyance that search keyword gets highlighted every time I source a file

" Edit area
set textwidth=100
set colorcolumn=100 " Highlight column after 'textwidth'
set columns=150
set lines=45

" Folding
set nofoldenable
set foldmethod=indent
" Space to toggle folds
" map <Space> za

" Visual aids
" set cursorline
set nolist
set listchars=tab:¿\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOL
set mousehide    " Hide mouse when typing
set number       " Show line number
set ruler
set scrolloff=0  " Maintain some more context around the cursor
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
set noignorecase
set smartcase
set gdefault     " Make substitution flag 'g' is default on

" Wild stuff
set wildcharm=<Tab> " Let <Tab> be recognized when used inside a macro
set wildignore+=*~,*.native,*.byte,*.hi,*.pyc,*.o
set wildmenu        " Expand the command line using tab
set wildmode=list:longest,full

" Don't use Ex mode, use Q for formatting
map Q gq

augroup set_filetype_for_the_unknown
  au!
  au BufEnter *.x       set filetype=haskell
  au BufEnter *.y       set filetype=haskell
  au BufEnter *.md      set filetype=markdown
augroup end

augroup do_not_hard_wrap_plain_text
  au!
  au FileType text        set wrap linebreak nolist
  au FileType markdown    set wrap linebreak nolist
augroup end

augroup before_loading
  au!
  " Change directories automatically and print the directory after changing
  au BufEnter * :lchdir %:p:h

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
augroup end

augroup before_saving
  au!
  au BufWritePre * retab

  " Remove trailing whitespaces
  " http://vim.wikia.com/wiki/Remove_unwanted_spaces
  " au BufWritePre * :%s/\s\+$//e
augroup end

augroup after_saving_dotfiles
  au!
  au BufWritePost .bashrc.append :silent !cp /etc/skel/.bashrc ~ && cat % >> ~/.bashrc

  au BufWritePost .emacs    :silent !cp % ~
  au BufWritePost .gemrc    :silent !cp % ~
  au BufWritePost .gnomerc  :silent !cp % ~
  au BufWritePost .hgrc     :silent !cp % ~
  au BufWritePost .irbrc    :silent !cp % ~
  au BufWritePost .vimrc    :silent !cp % ~
  au BufWritePost .xsession :silent !cp % ~
  au BufWritePost .zshrc    :silent !cp % ~

  au BufWritePost prelude-modules.el :silent !cp % ~/.emacs.d
  au BufWritePost xmonad.hs :silent !cp % ~/.xmonad
  au BufWritePost .vimrc :source %
augroup end

" Buffers
" set hidden
" call Map('<C-Tab>', ':bnext<CR>')

" Tabs
set tabpagemax=9 " At most 9 tabs open

"-------------------------------------------------------------------------------
"       #Completion
"-------------------------------------------------------------------------------
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
" imap <Tab> <C-r>=SmartTab()<CR>

"-------------------------------------------------------------------------------
"       #Map
"-------------------------------------------------------------------------------
function! Map(lhs, rhs)
  execute 'noremap'  a:lhs           a:rhs
  execute 'inoremap' a:lhs '<Esc>' . a:rhs
endfunction

"-------------------------------------------------------------------------------
"       #Leader
"-------------------------------------------------------------------------------
" , is a more convenient leader than \
let mapleader = " "
let maplocalleader = ",,"
nnoremap <Leader>a  ggvG$
nnoremap <Leader>cc :colorscheme<Space><Tab>
nnoremap <Leader>ct :call gruvbox#bg_toggle()<CR>
nnoremap <Leader>e  :e<Space><Tab>
nnoremap <Leader>f  :set foldenable! foldenable?<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>n  :NERDTreeTabsToggle<CR>
nnoremap <Leader>q  :q<CR>
nnoremap <Leader>tv :tabedit ~/Dropbox/Code/dotfiles/.vimrc<CR>
nnoremap <Leader>tz :tabedit ~/Dropbox/Code/dotfiles/.zshrc<CR>
nnoremap <Leader>w  :w<CR>

" au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
" au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>

"-------------------------------------------------------------------------------
"       #Ctrl
"-------------------------------------------------------------------------------
call Map('<C-a>', 'ggvG$')
call Map('<C-c>', '"+y')
call Map('<C-o>', ':e<Space><Tab>')
call Map('<C-q>', ':q<CR>')
call Map('<C-s>', ':w<CR>')
call Map('<C-v>', '"+p')
call Map('<C-x>', '"+d')

call Map('<C-t>'  , ':tabnew<CR>')
call Map('<C-Tab>', ':tabnext<CR>')
call Map('<C-F4>' , ':tabclose<CR>')

" Move around in insert mode
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>

"-------------------------------------------------------------------------------
"       #ghcmod
"-------------------------------------------------------------------------------
augroup ghc_mod
  au!
  au FileType haskell   nnoremap <buffer> <F1> :GhcModType<CR>
  au FileType haskell   nnoremap <buffer> <F2> :GhcModTypeClear<CR>
  au FileType haskell   nnoremap <buffer> <F3> :GhcModCheck<CR>
  au FileType haskell   nnoremap <buffer> <F4> :GhcModLint<CR>
augroup end

"-------------------------------------------------------------------------------
"       #make_and_run
"-------------------------------------------------------------------------------
augroup make_and_run
  au!
                          nnoremap          <F9>  :w<CR>:make<CR>
  au FileType c           nnoremap <buffer> <F10> :!./%<.out<Space>
  au FileType haskell     nnoremap <buffer> <F10> :!./%<.exe<Space>
  au FileType ocaml       nnoremap <buffer> <F10> :!./%<.native<Space>
  au FileType java        nnoremap <buffer> <F10> :!java %<<Space>
  au FileType scala       nnoremap <buffer> <F10> :!scala %<<Space>
  au FileType python      nnoremap <buffer> <F10> :!python %<Space>
  au FileType ruby        nnoremap <buffer> <F10> :!ruby %<Space>
  au FileType sh          nnoremap <buffer> <F10> :!bash %<Space>
augroup end

"-------------------------------------------------------------------------------
"       #arrows
"-------------------------------------------------------------------------------
map <Up>    <Nop>
map <Down>  <Nop>
map <Left>  <Nop>
map <Right> <Nop>

"-------------------------------------------------------------------------------
"       #NERDTree
"-------------------------------------------------------------------------------
" Open a NERDTree automatically when Vim starts up if no files were specified
" Also change current directory to Code
" au VimEnter * :NERDTree %:p:h
" au VimEnter * if (argc() == 0) | :NERDTree %:p:h | endif
let NERDTreeIgnore = ['^_build$', '^_tags$', '\.native$', '\.exe$', '\.sock$']

" Close Vim if the only window left open is a NERDTree
" au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
let g:NERDTreeWinSize = 24

"-------------------------------------------------------------------------------
"       #GitGutter
"-------------------------------------------------------------------------------
" To stop vim-gitgutter running real-time and eagerly
" let g:gitgutter_realtime = 0
" let g:gitgutter_eager = 0

"-------------------------------------------------------------------------------
"       #vim2hs
"-------------------------------------------------------------------------------
let g:haskell_conceal_wide = 0
let g:haskell_conceal_enumerations = 0

"-------------------------------------------------------------------------------
"       #ghcmod
"-------------------------------------------------------------------------------
" https://github.com/eagletmt/ghcmod-vim/wiki/Customize
let &l:statusline = '%{empty(getqflist()) ? "[No Errors]" : "[Errors Found]"}' . (empty(&l:statusline) ? &statusline : &l:statusline)

"-------------------------------------------------------------------------------
"       #haskell-mode
"-------------------------------------------------------------------------------
let g:haddock_browser = "/usr/bin/google-chrome"
let g:ghc = "/usr/bin/ghc"

"-------------------------------------------------------------------------------
"       #indentation
"-------------------------------------------------------------------------------
augroup indentation
  au!
  au FileType c           set shiftwidth=4 cindent
  au FileType cpp         set shiftwidth=4 cindent
  au FileType eruby       set shiftwidth=2
  au FileType haskell     set shiftwidth=4
  au FileType html        set shiftwidth=2
  au FileType java        set shiftwidth=4
  au FileType javascript  set shiftwidth=2
  au FileType lex         set shiftwidth=4
  au FileType ocaml       set shiftwidth=2
  au FileType python      set shiftwidth=4
  au FileType r           set shiftwidth=2
  au FileType ruby        set shiftwidth=2
  au FileType sh          set shiftwidth=2
  au FileType vim         set shiftwidth=2
  au FileType xml         set shiftwidth=4
  au FileType yacc        set shiftwidth=4
augroup end

"-------------------------------------------------------------------------------
"       #makeprg
"-------------------------------------------------------------------------------
augroup makeprg
  au!
  au FileType c           set makeprg=gcc\ -Wconversion\ %\ -o\ %<.out
  " au FileType c           set makeprg=gcc\ %\ -o\ %<.out\ -lm
  au FileType cpp         set makeprg=g++\ %\ -o\ %<.out
  au FileType haskell     set makeprg=ghc\ --make\ -Wall\ %\ -o\ %<.exe
  au FileType ocaml       set makeprg=corebuild\ -use-ocamlfind\ -cflags\ '-warn-error'\ %<.native
  au FileType java        set makeprg=javac\ %
  au FileType scala       set makeprg=scalac\ %
augroup end

"-------------------------------------------------------------------------------
"       #ocp-indent
"-------------------------------------------------------------------------------
augroup ocp_indent
  au!
  autocmd FileType ocaml source ~/.opam/4.01.0/share/vim/syntax/ocp-indent.vim
augroup end
