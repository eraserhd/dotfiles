
" Find a terminal on the same tab page
function! eraserhd#nearest_terminal()
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

function! eraserhd#goto_nearest_terminal()
  let l:terminal_win = eraserhd#nearest_terminal()
  if l:terminal_win == -1
    echoerr "No terminal found!"
    return
  endif
  execute l:terminal_win . "wincmd w"
endfunction

function! eraserhd#repeat_last_terminal_command()
  let t:return_on_escape = 1
  let l:start_winnr = winnr()
  let l:terminal_win = eraserhd#nearest_terminal()
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

function! eraserhd#existing_TODO()
  for i in tabpagebuflist()
    if bufname(i) == ".git/TODO"
      return bufwinnr(i)
    endif
  endfor
endfunction

function! eraserhd#open_TODO()
  let l:current_win = winnr()
  wincmd b
  split .git/TODO
  wincmd k
  10wincmd _
  set winfixheight
endfunction

function! eraserhd#goto_TODO()
  let l:todo = eraserhd#existing_TODO()
  if !empty(l:todo)
    execute l:todo . "wincmd w"
    return
  endif
  call eraserhd#open_TODO()
endfunction
