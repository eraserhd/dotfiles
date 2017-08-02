
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

function! eraserhd#goto(what, ...)
  if a:what == "repl"
    call eraserhd#goto_repl()
    if a:0 >= 1 && a:1 == "insert"
      let t:return_on_escape = 1
      startinsert
    endif
  else
    echoe "Don't know how to go to '" . a:what . "'"
  endif
endfunction

let s:ReplCommands = {
  \ "clojure": "lein repl" ,
  \ "idris": "idris" }

function! eraserhd#configure()
  if exists("t:eraserhd_configured") && t:eraserhd_configured
    return
  endif
  let t:eraserhd_configured = 1
  let l:original_window = winnr()
  if has_key(s:ReplCommands, &filetype)
    let l:repl_command = s:ReplCommands[&filetype]
  else
    let l:repl_command = ""
  endif
  below vsplit term://bash\ -l
  wincmd L
  execute l:original_window . "wincmd w"
  if l:repl_command != ""
    call eraserhd#goto_repl()
    let t:return_on_escape = 1
    startinsert
    call feedkeys(l:repl_command . "\<CR>\<Esc>")
  endif
endfunction
