" Specify a directory for plugins For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'

"-----------------------------------------------------------------------------
" vim-plug plugins {{{1
"-----------------------------------------------------------------------------
" Make sure you use single quotes

call plug#begin('~/.vim/plugged')
Plug 'Shougo/denite.nvim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'
Plug 'Shougo/deoplete-zsh'
"Plug 'Shougo/neosnippet'
"Plug 'Shougo/neosnippet-snippets'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'Shougo/neoinclude.vim'
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'hkupty/iron.nvim'
Plug 'sbdchd/neoformat', {'on': 'Neoformat'}
Plug 'mhartington/nvim-typescript'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/vim-easy-align', {'on': 'EasyAlign'}
Plug 'frankier/neovim-colors-solarized-truecolor-only'
call plug#end()
" }}} end of vim-plug plugins ------------------------------------------------

"-----------------------------------------------------------------------------
" Settings {{{1
"-----------------------------------------------------------------------------
"Want a different map leader than \
let mapleader = ","

set incsearch           " Set incremental searching"
set hlsearch            " Highlight searching
set wildmenu            " Command line completion

set cmdheight=2
set autoindent

set spelllang=en_gb     " Set region to British English

set scrolloff=3         " Keep the cursor 3 lines off of bottom when scrolling
set timeoutlen=1200     " A little bit more time for macros
set ttimeoutlen=50      " Make Esc work faster

" When completing by tag, show the whole tag, not just the function name
set showfulltag

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

set foldlevel=1         " Set the opening fold level
set foldmethod=marker
set foldopen+=jump

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
nmap <leader>sv :so $MYVIMRC<CR>

" Toggle spell checking on and off
nmap <silent> <F3> :set spell!<CR>

" Map escape key to jj -- much faster
imap jj <esc>
cmap jj <esc>

"Saves time; maps the spacebar to colon
nmap <space> :

" Press F2 to toggle showing white space on/off
nmap <F2> :set list!<CR>

" Press F4 to toggle highlighting on/off, and show current value.
:noremap <F4> :set hlsearch! hlsearch?<CR>

" Insert a hash rocket with <c-l>
imap <c-l> <space>=><space>

" Emmet expandtab
imap ee <C-y>,

" Align selected lines
vnoremap <leader>ib :!align<cr>

" Use CTRL-s for saving, also in instert mode
noremap <silent> <C-s> :w<CR>
inoremap <silent> <C-s> <Esc>:w<CR>a
vnoremap <silent> <C-s> <C-c>:update<CR>

" UltiSnips setup and key bindings {{{2 ------------------
let g:UltiSnipsExpandTrigger="<C-k>"
let g:UltiSnipsJumpForwardTrigger="<C-b>"
let g:UltiSnipsJumpBackwardTrigger="<C-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
" }}} end of UltiSnips setup and key bindings ------------

" Easymotion setup and key bindings {{{2 -----------------
map <leader>    <Plug>(easymotion-prefix)
" }}} end of Easymotion setup and key bindings -----------

" }}} end of Keyboard mapping stuff ------------------------------------------

" Airline stuff {{{2 -------------------------------------
let g:airline_powerline_fonts = 1
let g:airline_solarized_bg='dark'
" }}} end of Airline stuff -------------------------------

" Disable Python 2 support:
let g:loaded_python_provider = 1
let g:python3_host_prog = '/home/gcman105/.virtualenvs/py3neovim/bin/python3'

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" Enable vimfiler as default explorer
:let g:vimfiler_as_default_explorer = 1

" FZF stuff {{{2 -----------------------------------------
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

nnoremap <silent> <Leader><Enter> :call fzf#run({
\   'source':  reverse(<sid>buflist()),
\   'sink':    function('<sid>bufopen'),
\   'options': '+m',
\   'down':    len(<sid>buflist()) + 2
\ })<CR>

nnoremap <C-p> :FZF<CR>
" }}} end of FZF stuff -----------------------------------

