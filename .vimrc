" .vimrc

" I want Vim, not Vi
set nocompatible

"=============================================================================
" VUNDLE

filetype off                  " Required

" Set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Keep Plugin commands between vundle#begin/end.

"-----------------------------------------------------------------------------
" GENERAL PLUGINS

Plugin 'Valloric/YouCompleteMe'
Plugin 'justinmk/vim-syntax-extra'

Plugin 'kien/ctrlp.vim'

" Search in Files, Buffers and MRU files at the same time
" let g:ctrlp_cmd = 'CtrlPMixed'

Plugin 'rking/ag.vim'

Plugin 'mhinz/vim-signify'

" TextMate-style snippets for Vim
Plugin 'msanders/snipmate.vim'

Plugin 'scrooloose/syntastic'

Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'

Plugin 'The-NERD-tree'
Plugin 'jistr/vim-nerdtree-tabs'

" Open a NERDTree automatically when Vim starts up if no files were specified
" Also change current directory to Code
" au VimEnter * :NERDTree %:p:h
" au VimEnter * if (argc() == 0) | :NERDTree %:p:h | endif
" let NERDTreeIgnore = ['^_build$', '^_tags$', '\.native$', '\.exe$', '\.sock$']

" Close Vim if the only window left open is a NERDTree
" au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
" let g:NERDTreeWinSize = 24

"-----------------------------------------------------------------------------
" HASKELL

Plugin 'lukerandall/haskellmode-vim'

" The preferred HTML browser for viewing Haddock documentation, required
let g:haddock_browser = "usr/bin/google-chrome"

" Crossed out since it causes data constructors in Haskell not to be highlighted
" Plugin 'dag/vim2hs'

" Disable all conceals, including the simple ones like lambda and composition
" let g:haskell_conceal = 0

Plugin 'Shougo/vimproc.vim'     " Dependency
Plugin 'eagletmt/ghcmod-vim'

" https://github.com/eagletmt/ghcmod-vim/wiki/Customize
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = "ghcmodType"

au FileType haskell nmap <buffer> <F1> :GhcModType<CR>
au FileType haskell nmap <buffer> <F2> :GhcModTypeClear<CR>
au FileType haskell nmap <buffer> <F3> :GhcModCheck<CR>
au FileType haskell nmap <buffer> <F4> :GhcModLint<CR>

" Auto-checking on writing
" au BufWritePost *.hs GhcModCheckAndLintAsync

Plugin 'eagletmt/neco-ghc'

au FileType haskell setl omnifunc=necoghc#omnifunc
" To work with YouCompleteMe
" let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:necoghc_enable_detailed_browse = 1

"-----------------------------------------------------------------------------
" COQ

Plugin 'def-lkb/vimbufsync'             " Dependency
Plugin 'the-lambda-church/coquille'

au FileType coq nmap <buffer> <C-c><C-l> :CoqLaunch<CR>
au FileType coq nmap <buffer> <C-c><C-c> :CoqToCursor<CR>
au FileType coq nmap <buffer> <C-c><C-n> :CoqNext<CR>
au FileType coq nmap <buffer> <C-c><C-u> :CoqUndo<CR>
au FileType coq nmap <buffer> <C-c><C-k> :CoqKill<CR>

" Set it to 'true' if you want Coquille to move your cursor to the end of the
" lock zone after calls to CoqNext or CoqUndo.
let g:coquille_auto_move = "true"

"-----------------------------------------------------------------------------
" RUBY & RAILS

Plugin 'kana/vim-textobj-user'          " Dependency
runtime macros/matchit.vim              " Required
Plugin 'nelstrom/vim-textobj-rubyblock'

Plugin 'thoughtbot/vim-rspec'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-rails'
Plugin 'vim-ruby/vim-ruby'

"-----------------------------------------------------------------------------

" The tabular plugin must come _before_ vim-markdown.
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'

Plugin 'derekwyatt/vim-scala'
Plugin 'kchmck/vim-coffee-script'

" HTML editing
Plugin 'mattn/emmet-vim'

"-----------------------------------------------------------------------------
" COLOR SCHEMES

Plugin 'Solarized'
Plugin 'Zenburn'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'jpo/vim-railscasts-theme'
Plugin 'morhetz/gruvbox'
Plugin 'w0ng/vim-hybrid'
Plugin 'zeis/vim-kolor'

"-----------------------------------------------------------------------------

" All of your Plugins must be added before the following line
call vundle#end()            " Required
filetype plugin indent on    " Required

"=============================================================================
" VIM SETTINGS

syntax on
set t_Co=256
set guifont=Ubuntu\ Mono\ 15
" set guifont=Monospace\ 12
set background=light
colorscheme solarized

"-----------------------------------------------------------------------------

set columns=124
set lines=45

" Highlight column after 'textwidth'
set colorcolumn=+1,+41

" Maintain some more context around the cursor
set scrolloff=3

"-----------------------------------------------------------------------------

" Show line number
set number

" Show line and column number of the cursor position
set ruler

" Show (partial) command in the bottom
set showcmd

" Show matching braces
set showmatch

set showmode

" Always show status line
set laststatus=2

" Command line history
set history=99

" Hide mouse when typing
set mousehide

" Remove GUI-nonsense
set guioptions=

"-----------------------------------------------------------------------------

" Make tabs into spaces (set by tabstop)
set expandtab

"A tab is 8 spaces
set tabstop=8

set shiftwidth=2

set textwidth=78

"Indent instead of tab at start of line
set smarttab

"-----------------------------------------------------------------------------

" Switch wrap off for everything
set nowrap

" Copy indent from current line when starting a new line
set autoindent

" Do smart autoindenting when starting a new line, require 'autoindent'
set smartindent

"Don't convert spaces to tabs
set nojoinspaces

"-----------------------------------------------------------------------------
" FOLDING

" Automatically fold by indent level
set foldmethod=indent

" Disable folding by default
set nofoldenable

"-----------------------------------------------------------------------------
" COMPLETION

" Always show the menu, insert longest match
set completeopt=menu,longest

" Ignore Haskell's dist/ directory in CtrlP
set wildignore+=*/dist/*

" Expand the command line using tab
set wildmenu
set wildmode=list:longest,full

"-----------------------------------------------------------------------------
" UNPRINTABLE CHARACTERS

" Don't display unprintable characters by default
" Use the same symbols as TextMate for tabstops and EOL
set nolist
set listchars=tab:¿\ ,eol:¬

"-----------------------------------------------------------------------------
" SEARCH & SUBSTITUTION

" Case-insensitive search
set ignorecase

" Highlight matches
set hlsearch

" Incremental search
set incsearch

" Be case-sensitive when there's a capital letter
set smartcase

" Make substitution flag 'g' by default on
set gdefault

" Get rid of the annoyance that search keyword gets highlighted every time I
" source a file
let @/=''

"-----------------------------------------------------------------------------
" MISC

" Use current shell for shell commands
set shell=$SHELL

set noswapfile

" Want better buffer handling in quickfix mode
set switchbuf=useopen,usetab,split

"=============================================================================
" KEY MAPPINGS

" Let <Tab> be recognized when used inside a macro
set wildcharm=<Tab>

" , is a more convenient leader than \
let mapleader = " "
let maplocalleader = ",,"
nmap <Leader>a  ggvG$
nmap <Leader>c  :colorscheme<Space><Tab>
nmap <Leader>e  :e<Space><Tab>
nmap <Leader>f  :set foldenable! foldenable?<CR>
nmap <Leader>gw :Gwrite<CR>
nmap <Leader>h  :h<Space>
nmap <Leader>n  :NERDTreeMirrorToggle<CR>
nmap <Leader>q  :q<CR>
nmap <Leader>te :tabedit ~/Dropbox/Code/dotfiles/.emacs<CR>
nmap <Leader>tv :tabedit ~/Dropbox/Code/dotfiles/.vimrc<CR>
nmap <Leader>tz :tabedit ~/Dropbox/Code/dotfiles/.zshrc<CR>
nmap <Leader>w  :w<CR>

function! Map(lhs, rhs)
  execute "noremap"  a:lhs           a:rhs
  execute "inoremap" a:lhs "<Esc>" . a:rhs
endfunction

call Map("<C-x>", '"+d')
call Map("<C-c>", '"+y')
call Map("<C-v>", '"+p')

call Map("<C-t>"  , ":tabnew<CR>")
call Map("<C-Tab>", ":tabnext<CR>")
call Map("<C-F4>" , ":tabclose<CR>")

" Move around in insert mode
imap <C-h> <Left>
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-l> <Right>

" Don't use Ex mode, use Q for formatting
map Q gq

"=============================================================================
" AUTO COMMANDS

" Change directories automatically and print the directory after changing
" Replaced 'set autochdir'
au BufEnter * :lchdir %:p:h

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

"-----------------------------------------------------------------------------

" Replace white-spaces containing a <Tab> with new strings of white-space.
au BufWritePre * retab

" Remove trailing whitespaces
" http://vim.wikia.com/wiki/Remove_unwanted_spaces
au BufWritePre * :%s/\s\+$//e

"-----------------------------------------------------------------------------
" LANGUAGE-SPECIFIC

au BufWritePost {.vimrc,.emacs,.zshrc} :silent !cp % ~

au BufEnter {Gemfile,Rakefile,Guardfile,*.rake,config.ru} setl ft=ruby
au BufEnter {*.md,*.markdown}                             setl ft=markdown
au BufEnter {*.x,*.y}                                     setl ft=haskell

au FileType {c,cpp}                                     setl shiftwidth=4 cindent
au FileType {haskell,python,java,xml,lex,yacc}          setl shiftwidth=4
au FileType {ruby,eruby,sh,javascript,ocaml,vim,r,html} setl shiftwidth=2

" This option ('linebreak') is not used when the 'wrap' option is off or 'list' is on.
au FileType text     setl wrap nolist linebreak
au FileType markdown setl wrap nolist linebreak

au FileType ocaml source ~/.opam/4.01.0/share/vim/syntax/ocp-indent.vim
