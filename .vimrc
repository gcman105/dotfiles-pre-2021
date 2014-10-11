" .vimrc
" Author: Gary Cheeseman <gary@cheeseman.me.uk>
" http://gary.cheeseman.me.uk
"
" vim: foldmethod=marker
"
" The line below allows me to update the Gist with the command :Gist
" GistID: 5821422

"-----------------------------------------------------------------------------
" Global stuff {{{1
"-----------------------------------------------------------------------------
"Below is the line that can cause a No Mapping error on startup if I'm also
"using the Snipmate plugin.  It seems to be okay if I use it at the top.
set cpoptions=ces$

"Forget compatibility with Vi.
set nocompatible

"add $ to end of change selection
set cpoptions+=$

"Pathogen
filetype off
call pathogen#incubate()
call pathogen#helptags()   

if has("autocmd")
  filetype plugin indent on
endif

"-----------------------------------------------------------------------------
" Custom autocmds {{{1
"-----------------------------------------------------------------------------
augroup vimrcEx
  " Clear all autocmds in the group
  autocmd!
  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

"-----------------------------------------------------------------------------
" File stuff {{{1
"-----------------------------------------------------------------------------
"Enable filetypes
filetype on
filetype plugin on
filetype indent on
syntax on

"Write the old file out when switching between files.
set autowrite

"NOTE that the directories need to be setup for this next section !!!!!!!!!!
"Setup backup location and enable
set backupdir=$HOME/temp/vim_backups/
set backup
"Setup the Vim swap files location
set directory=$HOME/temp/vim_swp/

"Allow the cursor to go in to 'invalid' places
set virtualedit=all

"Syntax coloring lines that are too long just slows down the world
"set synmaxcol=2048
set synmaxcol=512

"Make command line two lines high
set ch=2

"Set the status line the way I like it
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
set stl=%f\ %m\ %r\ Line:\ %l/%L[%p%%]\ Col:\ %c\ Buf:\ #%n\ [%b][0x%B]
set stl+=\ %{fugitive#statusline()}
set stl+=\ %{SyntasticStatuslineFlag()} 

" sudo write this
cmap W! w !sudo tee % >/dev/null

"-----------------------------------------------------------------------------
" Printing options {{{1
"-----------------------------------------------------------------------------
set printoptions=header:0,duplex:long,paper:A4

" set the search scan to wrap lines
set wrapscan

"-----------------------------------------------------------------------------
" Language stuff {{{1
"-----------------------------------------------------------------------------
"Toggle spell checking on and off
nmap <silent> <F3> :set spell!<CR>

"Set region to British English
set spelllang=en_gb

"-----------------------------------------------------------------------------
" Display stuff {{{1
"-----------------------------------------------------------------------------
"Display current cursor position in lower right of status bar
set ruler

" Don't update the display while executing macros
set lazyredraw

" Set up the gui cursor to look nice
set guicursor=n-v-c:block-Cursor-blinkon0,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor,r-cr:hor20-Cursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

" set the gui options the way I like
set guioptions=acg

"command tabo, which makes the current tab the only tab
autocmd BufWinEnter,BufNewFile * silent tabo

syntax enable

"Set the color scheme.
set background=dark
set cul
if has("gui_running")    
  let base16colorspace=256
  colorscheme base16-tomorrow
  if has("gui_macvim")
    set guifont=Source\ Code\ Pro:h12
    set mouse=a
  else
    set guifont=Source\ Code\ Pro\ 10
    set mouse=c
  end
else
  autocmd InsertLeave * set cul
  autocmd InsertEnter * set nocul
  if exists('$TMUX')
    colorscheme lucius
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    colorscheme solarized
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
  set mouse=c
end

"Show lines numbers
set number

"Hide toolbar by default
set go-=T

"Keep some stuff in the history
set history=100

"-----------------------------------------------------------------------------
" Interface stuff {{{1
"-----------------------------------------------------------------------------
"Turn off the middle mouse button
map <MiddleMouse> <Nop>
imap <MiddleMouse> <Nop>
map <2-MiddleMouse> <Nop>
imap <2-MiddleMouse> <Nop>
map <3-MiddleMouse> <Nop>
imap <3-MiddleMouse> <Nop>
map <4-MiddleMouse> <Nop>
imap <4-MiddleMouse> <Nop>

"Ever notice a slight lag after typing the leader key + command? This lowers
"the timeout.
set timeoutlen=700

"When the page starts to scroll, keep the cursor 8 lines from
"the top and 3 lines from the bottom
set scrolloff=3

"Switch between buffers without saving
set hidden

"Allow backspacing over indent, eol, and the start of an insert
set backspace=2

"Tab and space stuff
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

"Better line wrapping 
set wrap
set textwidth=79
set formatoptions=qrn1

"Indent stuff
set smartindent
set autoindent
set shiftround

"folding settings {{{2
set foldcolumn=3
"set foldmethod=expr     "fold based on expression
set foldmethod=indent    "fold based on indent
set foldnestmax=10      "deepest fold is 10 levels
set foldlevel=0         "set the fold level
set foldenable          "fold by default

"Hide mouse when typing {{{2
set mousehide

"Show command in bottom right portion of the screen {{{2
set showcmd

"Always show the status line
set laststatus=2

"I Prefer the minimum line height
set linespace=0

"-----------------------------------------------------------------------------
" Formating stuff {{{2
"-----------------------------------------------------------------------------
set formatprg=par

"-----------------------------------------------------------------------------
" Searching stuff {{{2
"-----------------------------------------------------------------------------
"Set wildmenu to on
set wildmenu

"Auto-completion menu
"set wildmode=list:longest

"Set incremental searching"
set incsearch

"Highlight searching
set hlsearch

" When completing by tag, show the whole tag, not just the function name
set showfulltag

" Press F2 to toggle showing white space on/off
nmap <F2> :set list!<CR>
" Press F4 to toggle highlighting on/off, and show current value.
:noremap <F4> :set hlsearch! hlsearch?<CR>

" I'm happy to type the case of things.  I tried the ignorecase, smartcase
" thing but it just wasn't working out for me
set noignorecase

" case insensitive search
"set ignorecase
"set smartcase

"-----------------------------------------------------------------------------
" Keyboard mapping stuff {{{2
"-----------------------------------------------------------------------------
"Want a different map leader than \
let mapleader = ","

" Map CTRL-E to do what ',' used to do
nnoremap <c-e> ,
vnoremap <c-e> ,

"Hard-wrap paragraphs of text
nnoremap <leader>q gqip

" Create dictionary for custom expansions
set dictionary+=$HOME/.vim/dict.txt

"Opens a vertical split and switches over (\v)
nnoremap <leader>v <C-w>v<C-w>l

"Split windows below the current window.
set splitbelow              

"Map escape key to jj -- much faster
imap jj <esc>
cmap jj <esc>

"Delete all buffers (via Derek Wyatt)
nmap <silent> ,da :exec "1," . bufnr('$') . "bd"<cr>

"Bubble single lines (kicks butt)
"http://vimcasts.org/episodes/bubbling-text/
nmap <C-Up> ddkP
nmap <C-Down> ddp

"Bubble multiple lines
vmap <C-Up> xkP`[V`]
vmap <C-Down> xp`[V`]

"Saves time; maps the spacebar to colon
nmap <space> :

"Map code completion to , + tab
imap <leader><tab> <C-x><C-o>

" Insert a hash rocket with <c-l>
imap <c-l> <space>=><space>

" Align selected lines
vnoremap <leader>ib :!align<cr>

" Use CTRL-s for saving, also in instert mode
noremap <silent> <C-s> :w<CR>
inoremap <silent> <C-s> <Esc>:w<CR>a
vnoremap <silent> <C-s> <C-c>:update<CR>

"-----------------------------------------------------------------------------
" MULTIPURPOSE TAB KEY {{{2
" Indent if we're at the beginning of a line. Else, do completion.
"-----------------------------------------------------------------------------
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

"-----------------------------------------------------------------------------
" RENAME CURRENT FILE {{{2
"-----------------------------------------------------------------------------
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
map <leader>n :call RenameFile()<cr>

"-----------------------------------------------------------------------------
" PROMOTE VARIABLE TO RSPEC LETm {{{2
"-----------------------------------------------------------------------------
function! PromoteToLet()
  :normal! dd
  " :exec '?^\s*it\>'
  :normal! P
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
:command! PromoteToLet :call PromoteToLet()
:map <leader>p :PromoteToLet<cr>

"-----------------------------------------------------------------------------
" Session stuff {{{1
"-----------------------------------------------------------------------------
"Session settings
set sessionoptions=resize,winpos,winsize,buffers,tabpages,folds,curdir,help

"Set up an HTML5 template for all new .html files
"autocmd BufNewFile * silent! 0r $VIMHOME/templates/%:e.tpl

"Shortcut for editing  vimrc file in a new tab
nmap <leader>ev :edit $MYVIMRC<CR>
nmap <leader>sv :so $MYVIMRC<CR>

" cd to the directory containing the file in the buffer
nmap <leader>cd :lcd %:h<CR>
nmap <leader>md :!mkdir -p %:p:h<CR>

"Automatically change current directory to that of the file in the buffer
" autocmd BufEnter * cd %:p:h

"-----------------------------------------------------------------------------
" Plugin stuff {{{1
"-----------------------------------------------------------------------------
"------------------------"
"Neocomplcache {{{2
"------------------------"
let g:neocomplcache_enable_at_startup = 1
nmap <leader>ff :NeoComplCacheToggle<cr>

"------------------------"
"VimFiler {{{2
"------------------------"
let g:vimfiler_as_default_explorer = 1

"------------------------"
"Unite.vim {{{2
"------------------------"
call unite#filters#matcher_default#use(['matcher_fuzzy'])
nmap <silent> ss :Unite file file_mru bookmark buffer<cr>
nmap <silent> sS :Unite buffer bookmark file_mru<cr>
nmap <silent> sb :Unite bookmark<cr>
nmap <silent> Ss :Unite grep:.<cr>
nmap <silent> SS :Unite -start-insert file_rec/async<cr>
nmap <C-p> :Unite -quick-match buffer<cr>

"------------------------"
"NERDTree {{{2
"------------------------"
"Shortcut for NERDTreeToggle
nmap <leader>nt :NERDTreeToggle<cr>

"Show hidden files in NerdTree
let NERDTreeShowHidden=1

"------------------------"
"Syntastic {{{2
"------------------------"
let g:syntastic_check_on_open = 1
let g:syntastic_quite_warnings = 0
let g:syntastic_enable_signs = 1
let g:syntastic_enable_highlighting = 1
let g:syntastic_mode_map = {
      \ 'mode': 'active',
      \ 'active_filetypes': ['php'],
      \ 'passive_filetypes': ['html','python'] }
"let g:syntastic_python_checkers=['flake8']
"let g:syntastic_python_checker_args = '--ignore=E127'
"let g:syntastic_python_checker_args = '--ignore=W0401'
"let g:syntastic_python_checker_args='--ignore=E501,E302,E231,E261,E201,W402,W293'
let g:syntastic_php_checkers=['php', 'phpcs', 'phpmd']

"------------------------"
"Markdown {{{2
"------------------------"
nmap <leader>mm :%!/usr/local/bin/Markdown.pl --html4tags<cr>
vmap <leader>mm :!/usr/local/bin/Markdown.pl --html4tags<cr>
"nmap <leader>mm :'<,'>w !/usr/local/bin/Markdown.pl --html4tags<cr>
"vmap <leader>mm :'<,'>!/usr/local/bin/Markdown.pl --html4tags<cr>

"------------------------"
"Ultisnips {{{2
"------------------------"
"For autocompletion using the Ultisnips plugin
let g:snippets_dir = '$HOME/.vim/ultisnips-snippets/'

"------------------------"
"Emmet {{{2
"------------------------"

"Change emmet coding plugin expansion key
"let g:user_emmet_leader_key = '<F5>'
let g:user_emmet_leader_key = '<c-y>'

"------------------------"
"python-mode {{{2
"------------------------"

" Enable python folding
let g:pymode_folding = 1

" Auto fix vim python paths if virtualenv enabled
let g:pymode_virtualenv = 1

"------------------------"
"python script key mappings {{{2
"------------------------"
imap <c-g>u <c-r>=system('$HOME/.py-scripts/get_uuid.py')<cr>

"------------------------"
"Abolish ( I load replacemants from ./vim/after/plugin/abolish.vim ) {{{2
"------------------------"

"-----------------------------------------------------------------------------
" The lines below have been pulled out and moved into ($HOME/.vimlocal) as {{{2
" this allows for different setting on each system used.
"-----------------------------------------------------------------------------
"Set the initial window size (now moved to $HOME/.vimlocal)
"set lines=60
"set columns=188

"-----------------------------------------------------------------------------
" Source Personal stuff from local file. I store a file for each system {{{2
" in Dropbox and symlink .vimlocal to it. (.vimlocal_t430 for example)
"-----------------------------------------------------------------------------

source $HOME/.vimlocal

" .vimrc
