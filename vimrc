" customized vim config

" the most important thing
set nocompatible

if has("gui_macvim")
  let macvim_hig_shift_movement = 1
endif

" load pathogen
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Syntax hilighting is essential
filetype on
syntax enable
colorscheme crepuscule

" Allow vim to swap out unsaved buffers
set hidden

" Add recently accessed projects menu (project plugin)
set viminfo^=!

" Minibuffer Explorer Settings
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

" let supertab and endwise co-exist peacefully
let g:SuperTabCrMapping = 0

" Ruby Settings

" Keep rails.vim the hell out of my tab settings
autocmd User Rails set noexpandtab
" Ruby formatting
autocmd FileType ruby,eruby setlocal ts=2 sts=2 sw=2 noexpandtab
" Hash rocket in ruby mode is control-l, like in textmate
autocmd FileType ruby,eruby imap <c-l> <space>=><space>
" Change which file opens after executing :Rails command
let g:rails_default_file='config/database.yml'


" Syntax of these languages is fussy over tabs Vs spaces
autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType javascript setlocal ts=4 sts=4 sw=4 noexpandtab

" Python auto indenting
autocmd BufNewFile,BufRead *.py setlocal smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class


" ok, basic command mapping and setup is out of the way, settings time
set clipboard+=unnamed  " Yanks go on clipboard instead.
set history=256  " Number of things to remember in history.
set autowrite  " Writes on make/shell commands
set ruler  " Ruler on
set nu  " Line numbers on
set nowrap  " Line wrapping off
set timeoutlen=250  " Time to wait after ESC (default causes an annoying delay)
set bs=2  " Backspace over everything in insert mode
set incsearch
" auto indenting
set autoindent
" load indent files, to automatically do language-dependent indenting.
filetype plugin indent on
set showmatch  " Show matching brackets.
set mat=5  " Bracket blinking.
set visualbell  " error blinking .
set noerrorbells  " No noise.
set laststatus=2  " Always show status line.
set mousehide  " Hide mouse after chars typed
set mouse=a  " Mouse in all modes
set whichwrap=<,>,h,l,[,] " moving off the start or end of a line goes to the previous next

" Better invisibles
set list
set listchars=tab:▸\ ,eol:¬
" 

" Backups & Files
set backup                     " Enable creation of backup file.
set backupdir=~/.vim/backups " Where backups will go.
set directory=~/.vim/tmp     " Where temporary files will go.

" Mappings
map <D-F> :Ack<space>
" open NERDTree and shift to it
map <D-P> :NERDTreeToggle<cr><c-l>
vmap <D-/> :TComment<cr>
map <D-t> <Plug>PeepOpen

" when we enter quickfix, cmd-w means close quickfix, when we leave it, it means close buffer
" don't close |nofile| buffers like nerdtree or minibufexpl
map <D-w> :Bclose<cr>
autocmd WinEnter * call RemapCloseForQuickfix()
autocmd BufWinEnter quickfix nmap <D-w> :ccl<cr>
function RemapCloseForQuickfix()
	 if (getbufvar(winbufnr(winnr()), "&buftype") == "quickfix")
		nmap <D-w> :ccl<cr>
	 elseif (getbufvar(winbufnr(winnr()), "&buftype") == "nofile")
		 nmap <D-w> <Nop>
	 else
		nmap <D-w> :Bclose<CR>
	 endif
endfunction

"autocmd VimEnter * NERDTree " Start NERDTree Automatically
"autocmd VimEnter * wincmd p " Then move cursor back to the main window

