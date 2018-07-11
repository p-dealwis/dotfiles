" Vundle
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'scrooloose/nerdtree.git'
Plugin 'pangloss/vim-javascript'
Plugin 'ajh17/VimCompletesMe.git'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'altercation/vim-colors-solarized'
call vundle#end() 

" Custom Settings
set nocompatible
filetype plugin on
syntax on 
set path=$PWD/**

" NERDTree
map <C-n> :NERDTreeToggle<CR>
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

" Solarised

syntax enable
" set background=dark
" colorscheme solarized

if has('gui_running')
    set background=light
else
    set background=dark
endif

" Movement

"Between splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" A few sane defaults for use in ArchLabs

" load Arch Linux defaults
runtime! archlinux.vim

" yank text to system clipboard (requires +clipboard)
set clipboard^=unnamedplus

" enable line numbers
set number

" ask confirmation for certain things like when quitting before saving
set confirm

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

" syntax highlighting with true colors in the terminal
syntax enable
" TURNED OFF syntax highlighting becuase tmux doesn't support true colour
" if has('termguicolors')
"     if &term =~? 'screen\|tmux'
"         set t_8f=^[[38;2;%lu;%lu;%lum
"         set t_8b=^[[48;2;%lu;%lu;%lum
"     endif
"     set termguicolors
" endif

" bracketed paste while in insert mode, bracketed paste preserves indentation
inoremap <silent><C-v> <Esc>:set paste<CR>a<C-r>+<Esc>:set nopaste<CR>a

" better defaults
nnoremap 0 ^
nnoremap Y y$
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap <Tab> ==j

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
