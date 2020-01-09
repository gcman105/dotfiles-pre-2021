" Specify a directory for plugins For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'

"-----------------------------------------------------------------------------
" vim-plug plugins {{{1
"-----------------------------------------------------------------------------
" Make sure you use single quotes

call plug#begin('~/.vim/plugged')
"Plug 'Shougo/neomru.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/deoplete-zsh'
"Plug 'Shougo/denite.nvim'
"Plug 'kassio/neoterm'
Plug 'mhinz/vim-startify'
Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'Shougo/neoinclude.vim'
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'scrooloose/nerdcommenter'
"Plug 'wincent/command-t'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'posva/vim-vue'
"Plug 'arnaud-lb/vim-php-namespace'
Plug 'mhartington/nvim-typescript'
"Plug 'leafgarland/typescript-vim'
"Plug 'rizzatti/dash.vim'
Plug 'kshenoy/vim-signature'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
"Plug 'hkupty/iron.nvim'
Plug 'sbdchd/neoformat', {'on': 'Neoformat'}
"Plug 'mhartington/nvim-typescript'
"Plug 'terryma/vim-multiple-cursors'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/vim-easy-align', {'on': 'EasyAlign'}
Plug 'frankier/neovim-colors-solarized-truecolor-only',
Plug 'herrbischoff/cobalt2.vim'

function! BuildComposer(info)
  if a:info.status != 'unchanged' || a:info.force
    if has('nvim')
      !cargo build --release
    else
      !cargo build --release --no-default-features --features json-rpc
    endif
  endif
endfunction

Plug 'euclio/vim-markdown-composer', { 'do': function('BuildComposer') }

call plug#end()
" }}} end of vim-plug plugins ------------------------------------------------

"-----------------------------------------------------------------------------
" Settings {{{1
"-----------------------------------------------------------------------------
"Want a different map leader than \
let mapleader = ","

let g:python3_host_prog = expand('~/dotfiles/nvim/venv/bin/python')

"Enable filetypes
filetype on
filetype plugin on
filetype indent on
syntax on

"Write the old file out when switching between files.
set autowrite

set incsearch           " Set incremental searching"
set hlsearch            " Highlight searching
set wildmenu            " Command line completion

set mouse=a
set cmdheight=2
set autoindent

set spelllang=en_gb     " Set region to British English

set scrolloff=3         " Keep the cursor 3 lines off of bottom when scrolling
set timeoutlen=800     " A little bit more time for macros
set ttimeoutlen=50      " Make Esc work faster

" When completing by tag, show the whole tag, not just the function name
set showfulltag

"Syntax coloring lines that are too long just slows down the world
"set synmaxcol=2048
set synmaxcol=512

" Don't update the display while executing macros
set lazyredraw

"Switch between buffers without saving
set hidden

"command tabo, which makes the current tab the only tab
autocmd BufWinEnter,BufNewFile * silent tabo

"Allow backspacing over indent, eol, and the start of an insert
set backspace=2

" I'm happy to type the case of things.  I tried the ignorecase, smartcase
" thing but it just wasn't working out for me
set noignorecase

set number              " Show lines numbers
set relativenumber      " Make line numbers relative
set laststatus=2        " Always show the status line
set clipboard=unnamed
set showmatch           " Show matching brackets.
set cpoptions+=$        " Show $ at the end of a change command
set virtualedit=all     " Allow cursor into places it cant normally go

set splitbelow          " More natural split below
set splitright          " More natural split right

set nostartofline       " Do not jump to first character with page commands.
set expandtab           " Insert spaces when TAB is pressed.
set tabstop=2           " Render TABs using this many spaces.
set shiftwidth=2        " Indentation amount for < and > commands.

set foldlevel=3         " Set the opening fold level
set foldcolumn=3        " Set the fold gutter width
set foldmethod=marker
set foldopen+=jump

set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175

set termguicolors
set background=dark     " or light
colorscheme solarized

" }}} end of settings --------------------------------------------------------

"-----------------------------------------------------------------------------
" Keyboard mapping stuff {{{1
"-----------------------------------------------------------------------------
" Map CTRL-E to do what ',' used to do
nnoremap <c-e> ,
vnoremap <c-e> ,

" Shortcut for editing  vimrc file in a new tab
nmap <leader>ev :edit $MYVIMRC<CR>
nmap <leader>et :edit ~/.tmux.conf<CR>
nmap <leader>ez :edit ~/.zshrc<CR>
nmap <leader>sv :so $MYVIMRC<CR>
nmap <leader>st :so ~/.tmux.conf<CR>
nmap <leader>sz :so ~/.zshrc<CR>

" Toggle spell checking on and off
nmap <silent> <F3> :set spell!<CR>

" Map escape key to jj -- much faster
imap jj <esc>
cmap jj <esc>

"Saves time; maps the spacebar to colon
nmap <space> :

" Gist keyboard mappings
noremap <leader>gl    :Gist -l<CR>
noremap <leader>gp    :Gist<CR>
vnoremap <leader>gv   :'<,'>Gist<CR>

" Press F2 to toggle showing white space on/off
nmap <F2> :set list!<CR>

" Press F4 to toggle highlighting on/off, and show current value.
:noremap <F4> :set hlsearch! hlsearch?<CR>

" Insert a hash rocket with <c-l>
imap <c-l> <space>=><space>

" Emmet expandtab
imap <C-e><C-e> <C-y>,

" Align selected lines
vnoremap <leader>ib :!align<cr>

" Use CTRL-s for saving, also in instert mode
"noremap <silent> <C-s> :w<CR>
"inoremap <silent> <C-s> <Esc>:w<CR>a
"vnoremap <silent> <C-s> <C-c>:update<CR>

"Bubble single lines (kicks butt)
"http://vimcasts.org/episodes/bubbling-text/
nmap <C-Up> ddkP
nmap <C-Down> ddp

"Bubble multiple lines
vmap <C-Up> xkP`[V`]
vmap <C-Down> xp`[V`]

"Better line wrapping
set wrap
set textwidth=79
set formatoptions=qrn1

"Indent stuff
set smartindent
set autoindent
set shiftround

" A way to iter through buffers
nnoremap <C-b> :buffers<CR>:buffer<Space>

" sudo write this
cmap W! w !sudo tee % >/dev/null

let g:gist_use_password_in_gitconfig = 1

" netrw setup and key bindings {{{2 ------------------
let g:netrw_home = '$HOME/dotfiles'

" bookmarks setup and key bindings {{{2 ------------------
"map <leader>m g:netrw-mB

" typescript setup and key bindings {{{2 ------------------
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_debug = 1
let g:deoplete#enable_profile = 1
call deoplete#enable_logging('DEBUG', 'deoplete.log')
" }}} end of typescript setup and key bindings ------------

" ultisnips setup and key bindings {{{2 ------------------
let g:UltiSnipsExpandTrigger             =  "<tab>"
let g:UltiSnipsListSnippets              =  "<c-tab>"
let g:UltiSnipsJumpForwardTrigger        =  "<tab>"
let g:UltiSnipsJumpBackwardTrigger       =  "<c-tab>"
inoremap <c-x><c-k> <c-x><c-k>
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
" }}} end of UltiSnips setup and key bindings ------------

" Easymotion setup and key bindings {{{2 -----------------
map <leader><leader> <Plug>(easymotion-prefix)
" }}} end of Easymotion setup and key bindings -----------

" NERDTree setup and key bindings {{{2 -------------------
nnoremap <leader>n :NERDTreeToggle<cr>

"Show hidden files in NerdTree
let NERDTreeShowHidden=1

" Hide certain files
let NERDTreeIgnore = ['\.pyc$', '__pycache__', '.git', '.cache', '.idea', '.vscode', '.DS_Store', '.projectile', '.noseids', 'htmlcov', 'node_modules']

" }}} end of NERDTree setup and key bindings -------------

" }}} end of Keyboard mapping stuff ------------------------------------------

" Airline stuff {{{2 -------------------------------------
let g:airline_powerline_fonts = 1
let g:airline_solarized_bg='dark'
" }}} end of Airline stuff -------------------------------

" Disable Python 2 support:
let g:loaded_python_provider = 1

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" PhpInsertUse stuff {{{2 -----------------------------------------
" }}} end of PhpInsertUse stuff -------------------------------

" FZF stuff {{{2 -----------------------------------------
set rtp+=/usr/local/opt/fzf
" Open files in horizontal split
nnoremap <silent> <Leader>s :call fzf#run({
\   'down': '40%',
\   'sink': 'botright split' })<CR>

" Open files in vertical horizontal split
nnoremap <silent> <Leader>v :call fzf#run({
\   'right': winwidth('.') / 2,
\   'sink':  'vertical botright split' })<CR>

nnoremap <silent> <Leader>C :call fzf#run({
\   'source':
\     map(split(globpath(&rtp, "colors/*.vim"), "\n"),
\         "substitute(fnamemodify(v:val, ':t'), '\\..\\{-}$', '', '')"),
\   'sink':    'colo',
\   'options': '+m',
\   'left':    30
\ })<CR>

" Open buffer
function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction

function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

nnoremap <silent> <Leader>b :call fzf#run({
\   'source':  reverse(<sid>buflist()),
\   'sink':    function('<sid>bufopen'),
\   'options': '+m',
\   'down':    len(<sid>buflist()) + 2
\ })<CR>

nnoremap <C-p> :GFiles<CR>

" }}} end of FZF stuff -----------------------------------

