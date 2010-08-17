" customized vim config

" the most important thing
set nocompatible

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

" Ruby Settings

" Keep rails.vim the hell out of my tab settings
autocmd User Rails set noexpandtab
" Ruby formatting
autocmd FileType ruby,eruby setlocal ts=2 sts=2 sw=2 noexpandtab
" Ruby autocompletion
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
" Change which file opens after executing :Rails command
let g:rails_default_file='config/database.yml'


" Syntax of these languages is fussy over tabs Vs spaces
autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType javascript setlocal ts=4 sts=4 sw=4 noexpandtab


" Python auto indenting
autocmd BufNewFile,BufRead *.py setlocal smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class

" autocompletion mapping

" omnicomplete on \\
imap <leader><leader> <c-x><c-o>
" regular, textmate style complete on ``
imap `` <c-x><c-u> 


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
set autoindent
set showmatch  " Show matching brackets.
set mat=5  " Bracket blinking.
set visualbell  " error blinking .
set noerrorbells  " No noise.
set laststatus=2  " Always show status line.
set mousehide  " Hide mouse after chars typed
set mouse=a  " Mouse in all modes
set whichwrap=<,>,h,l,[,] " moving off the start or end of a line goes to the previous next

" Backups & Files
set backup                     " Enable creation of backup file.
set backupdir=~/.vim/backups " Where backups will go.
set directory=~/.vim/tmp     " Where temporary files will go.

"autocmd VimEnter * NERDTree " Start NERDTree Automatically
"autocmd VimEnter * wincmd p " Then move cursor back to the main window

