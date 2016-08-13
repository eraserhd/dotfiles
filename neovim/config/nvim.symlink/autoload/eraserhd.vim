
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
  startinsert
endfunction

function! eraserhd#repeat_last_terminal_command()
  let l:terminal_win = eraserhd#nearest_terminal()
  if l:terminal_win == -1
    echoerr "No terminal found!"
    return
  endif
  execute l:terminal_win . "wincmd w"
  call feedkeys("i\<Up>\<CR>\<Esc>")
endfunction
