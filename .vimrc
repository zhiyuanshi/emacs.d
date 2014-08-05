" .vimrc

" I want Vim, not Vi
set nocompatible

"=============================================================================
" Vundle

filetype off                  " Required

" Set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Keep Plugin commands between vundle#begin/end.
"-----------------------------------------------------------------------------

let mapleader = " "
let maplocalleader = ","

"-----------------------------------------------------------------------------

Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'

"-----------------------------------------------------------------------------
" Navigation

Plugin 'Shougo/unite.vim'

Plugin 'The-NERD-tree'
Plugin 'jistr/vim-nerdtree-tabs'
map <C-k><C-b> :NERDTreeMirrorToggle<CR>

" Modify NerdTree to make it use the split explorer model
" http://vimcasts.org/blog/2013/01/oil-and-vinegar-split-windows-and-project-drawer/
Plugin 'tpope/vim-vinegar'
let NERDTreeHijackNetrw = 1

Plugin 'kien/ctrlp.vim'

" Search in Files, Buffers and MRU files at the same time
" let g:ctrlp_cmd = 'CtrlPMixed'

map <M-p> :CtrlPMixed<CR>

Plugin 'rking/ag.vim'

Plugin 'majutsushi/tagbar'

map <Leader>= :TagbarToggle<CR>

" Open Tagbar if you open a supported file in an already running Vim
" au FileType * nested :call tagbar#autoopen(0)

" https://github.com/majutsushi/tagbar/wiki#haskell

Plugin 'bitc/lushtags'

"-----------------------------------------------------------------------------

" Lean & mean status/tabline for vim that's light as air
Plugin 'bling/vim-airline'

" The tabular plugin must come _before_ vim-markdown.
Plugin 'godlygeek/tabular'

" http://vimcasts.org/episodes/aligning-text-with-tabular-vim/
map <Leader>t-- :Tabularize /--<CR>
map <Leader>t=  :Tabularize /=<CR>
map <Leader>t:  :Tabularize /:\zs<CR>

" Faster than neocomplete
Plugin 'Valloric/YouCompleteMe'

" TextMate-style snippets for Vim
Plugin 'msanders/snipmate.vim'

" Faster than vim-gitgutter
Plugin 'mhinz/vim-signify'

" Don't run Sy by default
let g:signify_disable_by_default = 1

Plugin 'justinmk/vim-syntax-extra'

Plugin 'scrooloose/syntastic'

let g:syntastic_echo_current_error = 0
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_jump = 2
let g:syntastic_auto_loc_list = 1
let g:syntastic_haskell_checkers = ["hdevtools", "hlint"]

" Disable Syntastic's automatic syntax checking for Haskell because the error
" messages are not wrapped.
" Instead, use C-c C-c to do manual checks (offered by ghcmod-vim).
let g:syntastic_mode_map = { "mode": "active", "passive_filetypes": ["haskell"] }

Plugin 'terryma/vim-multiple-cursors'

" If set to 0, then pressing g:multi_cursor_quit_key in Insert mode will not
" quit and delete all existing cursors. This is useful if you want to press
" Escape and go back to Normal mode, and still be able to operate on all the
" cursors.
let g:multi_cursor_exit_from_insert_mode = 0

Plugin 'Shougo/vimshell.vim'

"=============================================================================
" Haskell

" Plugin 'lukerandall/haskellmode-vim'

" " The preferred HTML browser for viewing Haddock documentation, required
" let g:haddock_browser = "usr/bin/google-chrome"

" " By default, haskellmode sets omnifunc and completefunc to a GHC-based
" " completion of imported symbols and completion from Haddock, respectively. If
" " you want to turn these off (e.g. if you're using necoghc or an autocomplete
" " system like YCM or neocomplete), use:

" let g:haskellmode_completion_ghc = 0
" let g:haskellmode_completion_haddock = 0

" au BufEnter *.hs compiler ghc

Plugin 'Shougo/vimproc.vim'     " Dependency
Plugin 'eagletmt/ghcmod-vim'

" https://github.com/eagletmt/ghcmod-vim/wiki/Customize
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = "ghcmodType"

au FileType haskell nmap <buffer> <C-c><C-c> :GhcModCheckAsync<CR>
au FileType haskell nmap <buffer> <C-c><C-l> :GhcModLintAsync<CR>
au FileType haskell nmap <buffer> <C-c><C-t> :GhcModType<CR>
au FileType haskell nmap <buffer> <C-c><C-i> :GhcModInfoPreview<CR>
au FileType haskell nmap <buffer> <C-c><C-s> :GhcModTypeInsert<CR>

" Auto-checking on writing
" au BufWritePost *.hs GhcModCheckAndLintAsync

Plugin 'eagletmt/neco-ghc'

au FileType haskell setl omnifunc=necoghc#omnifunc
" To work with YouCompleteMe
" let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:necoghc_enable_detailed_browse = 1

" Crossed vim2hs out since it:
" (1) Does not highlight data constructors with (at least) the solarized theme, and
" (2) Pegs the CPU while you are scrolling.
" Plugin 'dag/vim2hs'

" Disable all conceals, including the simple ones like lambda and composition
" let g:haskell_conceal = 0

Plugin 'travitch/hasksyn'

"-----------------------------------------------------------------------------
" Coq

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
" Ruby, Rails & web development

Plugin 'kana/vim-textobj-user'          " Dependency
runtime macros/matchit.vim              " Required
Plugin 'nelstrom/vim-textobj-rubyblock'

Plugin 'thoughtbot/vim-rspec'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-rails'
Plugin 'vim-ruby/vim-ruby'

Plugin 'gregsexton/MatchTag'
Plugin 'mattn/emmet-vim'
Plugin 'tpope/vim-ragtag'

"-----------------------------------------------------------------------------

Plugin 'plasticboy/vim-markdown'

Plugin 'derekwyatt/vim-scala'
Plugin 'kchmck/vim-coffee-script'

" Plugin 'vim-scripts/TwitVim'

" " The number of tweets returned by :UserTwitter (default: 20)
" " You can set twitvim_count to any integer from 1 to 200.
" let twitvim_count = 200

" command Tlog    :UserTwitter

" " Post the current line in the buffer to Twitter
" command Tpost   :CPosttoTwitter

"-----------------------------------------------------------------------------
" Color schemes

Plugin 'chriskempson/base16-vim'

"-----------------------------------------------------------------------------

" All of your Plugins must be added before the following line
call vundle#end()            " Required
filetype plugin indent on    " Required

"=============================================================================

syntax on
set t_Co=256
set guifont=Ubuntu\ Mono\ 15
" set guifont=Monospace\ 12
set background=dark
colorscheme base16-mocha

"-----------------------------------------------------------------------------

set columns=110
set lines=45

set textwidth=88

" Highlight column after 'textwidth'
set colorcolumn=+1

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
" Folding

" Automatically fold by indent level
set foldmethod=indent

" Disable folding by default
set nofoldenable

"-----------------------------------------------------------------------------
" Completion

" Always show the menu, insert longest match
set completeopt=menu,longest

" Ignore Haskell's dist/ directory in CtrlP
set wildignore+=*/dist/*

" Expand the command line using tab
set wildmenu
set wildmode=list:longest,full

"-----------------------------------------------------------------------------
" Unprintable characters

" Don't display unprintable characters by default
" Use the same symbols as TextMate for tabstops and EOL
set nolist
set listchars=tab:¿\ ,eol:¬

"-----------------------------------------------------------------------------
" Search & substitution

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

" Use current shell for shell commands
set shell=$SHELL

set noswapfile

" Want better buffer handling in quickfix mode
set switchbuf=useopen,usetab,split

"=============================================================================
" Key mappings

" Let <Tab> be recognized when used inside a macro
set wildcharm=<Tab>

map <Leader>a ggvG$
map <Leader>c :colorscheme<Space><Tab>
map <Leader>e :e<Space><Tab>
map <Leader>h :h<Space>
map <Leader>q :q<CR>
map <Leader>s :%s/
map <Leader>v :tabedit ~/Dropbox/Code/dotfiles/.vimrc<CR>
map <Leader>w :w<CR>
map <Leader>x :qa<CR>

vmap <C-x> "+d
vmap <C-c> "+y

map  <C-v>      "+p
imap <C-v> <Esc>"+p

map  <C-t>      :tabnew<CR>
imap <C-t> <C-o>:tabnew<CR>

map  <C-Tab>      :tabnext<CR>
imap <C-Tab> <C-o>:tabnext<CR>

map  <C-S-Tab>      :tabprev<CR>
imap <C-S-Tab> <C-o>:tabprev<CR>

map  <C-F4>      :tabclose<CR>
imap <C-F4> <C-o>:tabclose<CR>

" Move around between windows
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Move around in insert mode
imap <C-h> <Left>
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-l> <Right>

" Don't use Ex mode, use Q for formatting
map Q gq

"=============================================================================
" Auto-commands

" Change directories automatically and print the directory after changing
" Replaced 'set autochdir'
au BufEnter * :lchdir %:p:h

" Fix broken syntax highlighting
au BufEnter * :syntax sync fromstart

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
" Language-specific auto-commands

" If we are editing a dotfile with one of the following names but it is not in
" the $DOTFILES directory, then there is obviously no setup.rb in the current
" directory and the error is silently ignored as we wish.
au BufWritePost {.vimrc,.emacs,.zshrc} :silent !ruby setup.rb

au BufEnter {Gemfile,Rakefile,Guardfile,*.rake,config.ru} setl ft=ruby
au BufEnter {*.md,*.markdown}                             setl ft=markdown
au BufEnter {*.x,*.y}                                     setl ft=haskell

au FileType {c,cpp}                                                  setl shiftwidth=4 cindent
au FileType {python,xml,lex,yacc}                                    setl shiftwidth=4
au FileType {haskell,ocaml,ruby,eruby,java,javascript,sh,vim,r,html} setl shiftwidth=2

" This option ('linebreak') is not used when the 'wrap' option is off or 'list' is on.
au FileType text     setl wrap nolist linebreak
au FileType markdown setl wrap nolist linebreak

au FileType ocaml source ~/.opam/4.01.0/share/vim/syntax/ocp-indent.vim
