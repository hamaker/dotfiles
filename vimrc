" Example Vim configuration.
" Copy or symlink to ~/.vimrc or ~/_vimrc.
"

set nocompatible                  " Must come first because it changes other options.

filetype off                  " required!

set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=/usr/local/share/lilypond/current/vim/
call vundle#begin()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/Vundle.vim'
" Plugins
Bundle 'ervandew/supertab'
Bundle 'tomtom/tcomment_vim'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-fugitive'
Bundle 'idanarye/vim-merginal'
Bundle 'tpope/vim-vinegar'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-ragtag'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-rbenv'
Bundle 'pangloss/vim-javascript'
Bundle 'mxw/vim-jsx'
Bundle 'vim-ruby/vim-ruby'
Bundle 'kien/ctrlp.vim'
Bundle 'tacahiroy/ctrlp-funky'
Bundle 'airblade/vim-gitgutter'
Bundle 'chrisbra/improvedft'
Bundle 'bling/vim-airline'
Bundle 'Shougo/neocomplete.vim'
" Plugin 'Valloric/YouCompleteMe'
" tmux integration
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'sjl/vitality.vim' "iterm 2 integration
" Syntax
Bundle 'groenewege/vim-less'
" Bundle 'heartsentwined/vim-ember-script'
Bundle 'kchmck/vim-coffee-script'
" Bundle 'nono/vim-handlebars'
Bundle 'slim-template/vim-slim'
" Bundle 'tpope/vim-cucumber'
Bundle 'pearofducks/ansible-vim'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'sudar/vim-arduino-syntax'
" Bundle 'altercation/vim-colors-solarized'
Plugin 'adlawson/vim-sorcerer'
Bundle 'elixir-lang/vim-elixir'
Bundle 'fatih/vim-go'
" Bundle 'skammer/vim-css-color'
Bundle 'ap/vim-css-color'
" Bundle 'ingydotnet/yaml-vim'
" Bundle 'skalnik/vim-vroom'
" Bundle 'hwartig/vim-seeing-is-believing'

call vundle#end()
filetype plugin on

" tmux integration
let g:tmux_navigator_save_on_switch = 1
set background=dark
" solarized options
" let g:solarized_hitrail = 0
" let g:solarized_termtrans = 1
" let g:solarized_visibility = 'high'
" let g:solarized_termcolors=256
" colorscheme solarized
colorscheme sorcerer

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0

let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
" let g:airline_section_c = "%f"
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#default#layout = [
      \ [ 'a', 'b', 'c' ],
      \ [ 'x', 'warning' ]
      \ ]

call airline#parts#define_raw('modified', '%{&modified ? "[+]" : ""}')
call airline#parts#define_accent('modified', 'red')
let g:airline_section_c =
      \ airline#section#create(['%{pathshorten(expand("%:."))}', 'modified'])
" function! SyntaxItem()
"   return synIDattr(synID(line("."),col("."),1),"name")
" endfunction
" let g:airline_section_c += '%{SyntaxItem()}'

" configure whether close button should be shown
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#tab_min_count = 2

let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1



" silent! call pathogen#runtime_append_all_bundles()

syntax enable                     " Turn on syntax highlighting.
filetype plugin indent on         " Turn on file type detection.

runtime macros/matchit.vim        " Load the matchit plugin.

set showcmd                       " Display incomplete commands.
set showmode                      " Display the mode you're in.

set backspace=indent,eol,start    " Intuitive backspacing.

set hidden                        " Handle multiple buffers better.

set wildmenu                      " Enhanced command line completion.
set wildmode=list:longest         " Complete files like a shell.

set ignorecase                    " Case-insensitive searching.
set smartcase                     " But case-sensitive if expression contains a capital letter.

set number                        " Show line numbers.
set ruler                         " Show cursor position.

set splitright
set splitbelow

set wrap                          " Turn on line wrapping.
set scrolloff=5                   " Show 3 lines of context around the cursor.

set title                         " Set the terminal's title

set visualbell                    " No beeping.

set nobackup                      " Don't make a backup before overwriting a file.
set nowritebackup                 " And again.
set directory=$HOME/.vim/tmp//,.  " Keep swap files in one location

" UNCOMMENT TO USE
set tabstop=2                    " Global tab width.
set shiftwidth=2                 " And again, related.
set expandtab                    " Use spaces instead of tabs

set laststatus=2                  " Show the status line all the time
" Useful status information at bottom of screen
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y\ %{fugitive#statusline()}=%-16(\ %l,%c-%v\ %)%P

"oh dear, that's sinful
set mouse=a

" set list
set listchars=""
" set listchars=tab:>\ "commented for whitspace
set listchars=tab:>·
set listchars+=trail:·
set listchars+=extends:>
set listchars+=precedes:<
set list
" ========================================================================
" Mappings
" ========================================================================
ca W w

" ========================================================================
" NETRW
" ========================================================================
" set nocp
" let g:netrw_liststyle=3

" ========================================================================
" leader commands
" ========================================================================

let mapleader=","

nnoremap <leader><leader> <c-^>

map <leader>a :CtrlP app/<cr>
map <leader>.c :CtrlP app/controllers<cr>
map <leader>.g :topleft :split Gemfile<cr>
map <leader>.j :CtrlP app/assets/javascripts<cr>
map <leader>.m :CtrlP app/models<cr>
map <leader>.r :topleft :split config/routes.rb<cr>
map <leader>.s :CtrlP app/assets/stylesheets<cr>
map <leader>.t :CtrlP spec<cr>
map <leader>b :Gblame<cr>
map <leader>c :TComment<cr>
map <silent><leader>C :call system('ctags -R --languages=ruby --exclude=.git --exclude=log .')
      \<cr>
map <leader>d :Gdiff<cr>
" map <leader>e :VroomRunNearestTest<CR>
map <leader>e :w<CR>:call RunCurrentLineInTest()<CR>
" map <leader>e :!bundle exec ./bin/rake test %<cr>
map <leader>F :call RefreshCTags() <bar> CtrlPTag<cr>
map <leader>f :CtrlP<cr>
map <leader>h :set hlsearch! hlsearch?<cr>
map <leader>i :%s/\t/  /g<CR> :KillWhitespace<CR>
" map <leader>l :call CompileLilypond()<CR>
map <leader>l :call InsertLookup<CR>
map <leader>n :call RenameFile()<cr>
map <leader>p "+p<cr>
" map <leader>r :!./bin/rake<cr>
map <leader>r :call RunCurrentTest()<cr>
" map <leader>r :VroomRunTestFile<cr>
map <leader>q :bd<CR>
map <leader>tr :w<CR>:call RunCurrentTest()<CR>
map <leader>v :tabe $MYVIMRC<CR>
map <leader>V :source $MYVIMRC<CR>
map <leader>w :bp<CR>:bd#<CR>
map <leader>W :KillWhitespace<CR>
map <leader>x :bn<CR>
map <leader>y "+y<cr>
map <leader>z :bp<CR>
map <silent> <leader>/ :let @/=""<CR>

" Tab mappings.
map <leader>tt :tabnew<cr>
map <leader>te :tabedit
map <leader>tc :tabclose<cr>
map <leader>to :tabonly<cr>
map <leader>tn :tabnext<cr>
map <leader>tp :tabprevious<cr>
map <leader>tf :tabfirst<cr>
map <leader>tl :tablast<cr>
map <leader>tm :tabmove
map <leader><Tab> :tabnext<cr>
map <leader><S-Tab> :tabprevious<cr>

map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
      \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
      \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
" Controversial...swap colon and semicolon for easier commands
" nnoremap ; :
" nnoremap : ;

"vnoremap ; :
"vnoremap : ;

" Automatic fold settings for specific files. Uncomment to use.
" autocmd FileType ruby setlocal foldmethod=syntax
" autocmd FileType css  setlocal foldmethod=indent shiftwidth=2 tabstop=2

" For the MakeGreen plugin and Ruby RSpec. Uncomment to use.
autocmd BufNewFile,BufRead *_spec.rb compiler rspec
autocmd FileType c,cpp,java,php,ruby,html,eruby,vim autocmd BufWritePre <buffer> %s/\s\+$//e

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Use ag instead of grep
set grepprg=ag\ --nogroup\ --nocolor
let g:ackprg = 'ag --nogroup --column'
" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

" ag is fast enough that CtrlP doesn't need to cache
let g:ctrlp_use_caching = 0

" Able to 'gf' files
set suffixesadd=.rb,.coffee,.js

" (Hopefully) removes the delay when hitting esc in insert mode
set ttimeout
set ttimeoutlen=20
set notimeout
set ttyfast
set ttyscroll=5

set winheight=7
set winminheight=7
set winheight=999

" let loaded_matchparen=1 " Don't load matchit.vim (paren/bracket matching)
" set noshowmatch         " Don't match parentheses/brackets
set nocursorline        " Don't paint cursor line
" set cursorline
set nocursorcolumn      " Don't paint cursor column
" set lazyredraw          " Wait to redraw
set scrolljump=1
let html_no_rendering=1 " Don't render italic, bold, links in HTML

" View full list when tab-complete in command mode
set wildmode=list:full
set wildignore+=*.log,tmp/*,public/assets/*,attachments/*,*.jpg,*.ogg,*.mp3,*.mp4,*.gif,*.png,*.jpeg,*.svg,*.xml,*.flv,*.m4v,*.sql,*.log,.DS_Store
" No difference between ; and ;
" map ; :

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr> :let @/ = ""<cr>

let g:CtrlPCancelMap=['<ESC>','<C-c>']
let CtrlPMaxFiles = 10000
let g:ctrlp_prompt_mappings = {
  \ 'PrtClearCache()':      ['<F6>'],
  \ }
"
" augroup myfiletypes
"   autocmd!
"   autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
" augroup END


""""""""""""""""""""""""
" FUNCTIONS
""""""""""""""""""""""""

function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction

function! RunCurrentTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFile()

    if match(expand('%'), '\.feature$') != -1
      call SetTestRunner("!cucumber")
      exec g:bjo_test_runner g:bjo_test_file
    elseif match(expand('%'), '_spec\.rb$') != -1
      " call SetTestRunner("!spring rspec")
      call SetTestRunner("!zeus rspec")
      exec g:bjo_test_runner g:bjo_test_file
    else
      call SetTestRunner("!ruby -Itest")
      exec g:bjo_test_runner g:bjo_test_file
    endif
  else
    exec g:bjo_test_runner g:bjo_test_file
  endif
endfunction

function! SetTestRunner(runner)
  let g:bjo_test_runner=a:runner
endfunction

function! RunCurrentLineInTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFileWithLine()
  end
  exec "!zeus rspec" g:bjo_test_file . ":" . g:bjo_test_file_line
  " exec "!rspec" g:bjo_test_file . ":" . g:bjo_test_file_line
endfunction

function! SetTestFile()
  let g:bjo_test_file=@%
endfunction

function! SetTestFileWithLine()
  let g:bjo_test_file=@%
  let g:bjo_test_file_line=line(".")
endfunction

function! CorrectTestRunner()
  if match(expand('%'), '\.feature$') != -1
    return "cucumber"
  elseif match(expand('%'), '_spec\.rb$') != -1
    return "rspec"
  else
    return "ruby"
  endif
endfunction

" lets go for it...
" noremap <Up> <NOP>
" noremap <Down> <NOP>
" noremap <Left> <NOP>
" noremap <Right> <NOP>
"
function! CompileLilypond()
  let currentFile = expand('%')
  let pdfFile = substitute(currentFile, '\.ly', '.pdf', '')
  exec "!lilypond % && open" pdfFile
endfunction

function! InsertLookup()
  :normal i{{lookup("credstash", "", region="us-east-1")}}
endfunction

:command! -nargs=1 LookupSingle :normal a {{lookup('credstash', '<args>', region='us-east-1')}}<ESC>
:command! -nargs=1 LookupDouble :normal a {{lookup("credstash", "<args>", region="us-east-1")}}<ESC>

function! RefreshCTags()
  exec system('ctags -R --languages=ruby,Go --exclude=.git --exclude=log .')
endfunction

" Enable seeing-is-believing mappings only for Ruby
augroup seeingIsBelievingSettings
  autocmd!

  autocmd FileType ruby nmap <buffer> <Enter> <Plug>(seeing-is-believing-mark-and-run)
  autocmd FileType ruby xmap <buffer> <Enter> <Plug>(seeing-is-believing-mark-and-run)

  autocmd FileType ruby nmap <buffer> <F4> <Plug>(seeing-is-believing-mark)
  autocmd FileType ruby xmap <buffer> <F4> <Plug>(seeing-is-believing-mark)
  autocmd FileType ruby imap <buffer> <F4> <Plug>(seeing-is-believing-mark)

  autocmd FileType ruby nmap <buffer> <F5> <Plug>(seeing-is-believing-run)
  autocmd FileType ruby imap <buffer> <F5> <Plug>(seeing-is-believing-run)
augroup END
