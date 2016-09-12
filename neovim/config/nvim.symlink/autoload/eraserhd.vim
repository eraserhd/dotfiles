
" Find a terminal on the same tab page
function! eraserhd#repl_winnr()
  let l:terminal_buffer = -1
  for i in tabpagebuflist()
    if getbufvar(i, "&buftype") == "terminal"
      let l:terminal_buffer = i
    endif
  endfor
  if l:terminal_buffer == -1
    return -1
  endif
  return bufwinnr(l:terminal_buffer)
endfunction

function! eraserhd#todo_winnr()
  for i in tabpagebuflist()
    if bufname(i) == ".git/TODO"
      return bufwinnr(i)
    endif
  endfor
endfunction

function! eraserhd#goto_repl()
  call eraserhd#configure()
  let l:terminal_win = eraserhd#repl_winnr()
  if l:terminal_win == -1
    echoerr "No terminal found!"
    return
  endif
  execute l:terminal_win . "wincmd w"
endfunction

function! eraserhd#repeat_last_repl_command()
  let t:return_on_escape = 1
  let l:start_winnr = winnr()
  let l:terminal_win = eraserhd#repl_winnr()
  if l:terminal_win == -1
    echoe "No terminal found!"
    return
  endif
  execute l:terminal_win . "wincmd w"
  call feedkeys("i\<Up>\<CR>\<Esc>")
endfunction

function! eraserhd#leave_insert()
  if !exists("t:return_on_escape")
    return
  endif
  if !t:return_on_escape
    return
  endif
  let t:return_on_escape = 0
  execute winnr("#") . "wincmd w" 
endfunction

function! eraserhd#goto_todo()
  call eraserhd#configure()
  let l:todo = eraserhd#todo_winnr()
  if !empty(l:todo)
    execute l:todo . "wincmd w"
    return
  endif
endfunction

function! eraserhd#goto(what, ...)
  if a:what == "repl"
    call eraserhd#goto_repl()
    if a:1 == "insert"
      let t:return_on_escape = 1
      startinsert
    endif
  elseif a:what == "todo"
    call eraserhd#goto_todo()
  else
    echoe "Don't know how to go to '" . a:what . "'"
  endif
endfunction

function! eraserhd#configure()
  if exists("t:eraserhd_configured") && t:eraserhd_configured
    return
  endif
  let t:eraserhd_configured = 1
  let l:original_window = winnr()
  below vsplit term://bash\ -l
  wincmd L
  split .git/TODO
  10wincmd _
  set winfixheight
  execute l:original_window . "wincmd w"
  if filereadable("project.clj")
    call eraserhd#goto_repl()
    let t:return_on_escape = 1
    startinsert
    call feedkeys("lein repl\<CR>\<Esc>")
  endif
endfunction
