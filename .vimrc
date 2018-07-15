" Vundle
" Install Vundle using the following command:
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" Efficiency
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'scrooloose/nerdtree.git'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ludovicchabant/vim-gutentags'
Plugin 'junegunn/fzf.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-obsession'

" Language Plugins
"" Javascript
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'

"" PHP
Plugin 'StanAngeloff/php.vim'
Plugin '2072/PHP-Indenting-for-VIm'
Plugin 'rayburgemeestre/phpfolding.vim'


Plugin 'ajh17/VimCompletesMe.git'
Plugin 'vim-vdebug/vdebug'
Plugin 'scrooloose/nerdcommenter'

" Themes
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
call vundle#end() 

" Appearance
set background=dark

" - Cursor
if exists('$TMUX')
  let &t_SI = "\ePtmux;\e\e[5 q\e\\"
  let &t_EI = "\ePtmux;\e\e[2 q\e\\"
else
  let &t_SI = "\e[5 q"
  let &t_EI = "\e[2 q"
endif

" Editting
set ai

" Files
set autoread
set autowrite

" -FZF
map <C-P>p :Files<CR>
map <C-P>a :Ag<CR>
map <C-P>d :Buffers<CR>
map <C-P>f :Lines<CR>

" Misc  Settings
set nocompatible
filetype plugin on
syntax on 
set path=$PWD/**

" Movement

" -Between splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

:set ignorecase
:set smartcase

" NERDTree
map <C-n> :NERDTreeToggle<CR>
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Check if NERDTree is open or active
function! IsNERDTreeOpen()        
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

" Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
" file, and we're not in vimdiff
function! SyncTree()
  if &modifiable && IsNERDTreeOpen() && strlen(expand('%')) > 0 && !&diff
    NERDTreeFind
    wincmd p
  endif
endfunction

" Highlight currently open buffer in NERDTree
autocmd BufEnter * call SyncTree()



"--------------------------------MY EDITS ABOVE----------------------------------"
" A few sane defaults for use in ArchLabs

" load Arch Linux defaults
runtime! archlinux.vim

" yank text to system clipboard (requires +clipboard)
set clipboard^=unnamedplus

" enable line numbers
set number

" ask confirmation for certain things like when quitting before saving
" set confirm

" enable tab completion menu when using colon command mode (:)
set wildmenu

set shortmess+=aAcIws   " Hide certain messages like 'Search Hit Bottom' etc.
set expandtab           " Tab inserts Spaces not Tabs '\t'
set softtabstop=4       " Amount of spaces to enter when Tab is pressed
set shiftwidth=4        " 4 space indentation

" enable mouse, sgr is better but not every term supports it
set mouse=a
if has('mouse_sgr')
    set ttymouse=sgr
endif

" bracketed paste while in insert mode, bracketed paste preserves indentation
inoremap <silent><C-v> <Esc>:set paste<CR>a<C-r>+<Esc>:set nopaste<CR>a

" better defaults
nnoremap 0 ^
nnoremap Y y$
" nnoremap n nzzzv
" nnoremap N Nzzzv

" better motions with wrapped text while preserving numbered jumps
for g:key in ['k', 'j', '<Up>', '<Down>']
    execute 'noremap <buffer> <silent> <expr> ' .
                \ g:key . ' v:count ? ''' .
                \ g:key . ''' : ''g' . g:key . ''''
    execute 'onoremap <silent> <expr> ' .
                \ g:key . ' v:count ? ''' .
                \ g:key . ''' : ''g' . g:key . ''''
endfor

augroup file_load_change_and_position
    " clear this group so they don't pile up
    autocmd!

    " when quitting, save position in file
    " when re-opening go to last position
    autocmd BufReadPost * call setpos(".", getpos("'\""))

    " Reload changes if file changed outside of vim
    " requires autoread (enabled by default)
    autocmd FocusGained,BufEnter * if mode() !=? 'c' | checktime | endif
    autocmd FileChangedShellPost * echo "Changes loaded from file"
augroup END
