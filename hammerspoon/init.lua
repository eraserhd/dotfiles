local function window_number(n)
  local windows = hs.window.visibleWindows()
  local bounds = hs.screen.mainScreen():frame()
  local screen_windows = {}
  for _, w in ipairs(windows) do
    local wframe = w:frame()
    if w:frame():inside(bounds) then
      table.insert(screen_windows, w)
    end
  end
  windows = screen_windows
  table.sort(windows, function(a, b)
    local af, bf = a:frame(), b:frame()
    if af.x < bf.x then return true end
    if af.x > bf.x then return false end
    return af.y < bf.y
  end)
  if n == 9 or n > #windows then
    return windows[#windows]
  else
    return windows[n+1]
  end
end

local function send_to_space(space_name)
  local window = hs.window.focusedWindow()
  hs.execute("yabai -m window --space " .. space_name, true)
  hs.execute("yabai-focus-space " .. space_name, true)
  window:focus()
end

config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

local function move_focus(direction)
  local window = hs.window.focusedWindow()
  window['focusWindow' .. direction](window, nil, true, true)
end

local function send_control_w()
  hs.eventtap.keyStroke({"control"}, "W")
end

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

local function focus_kitty_window(title)
  kitty("focus-window --match=title:" .. title)
end

local function paste_as_keystrokes()
  hs.eventtap.keyStrokes(hs.pasteboard.readString())
end

local function focus_space(space_name)
  hs.execute("yabai-focus-space " .. space_name, true)
end

local function focus_window_number(n)
  window_number(n):focus()
end

local function toggle_full_screen()
  hs.window.focusedWindow():toggleFullScreen()
end

local function rerun_last_command()
  kitty("send-text --match=title:kak_repl_window '\x10\x0d'")
end

local function balance_space()
  hs.execute("yabai -m space --balance", true)
end

local function toggle_split_direction()
  hs.execute("yabai -m window --toggle split", true)
end

local function swap_window(what)
  hs.execute("yabai -m window --swap " .. tostring(what), true)
end

local function swap_window_number(n)
  swap_window(window_number(n):id())
end

local function warp_window(to)
  hs.execute("yabai -m window --space code", true)
  hs.execute("yabai -m window --warp " .. tostring(to), true)
  hs.execute("yabai -m space code --balance", true)
end

local function warp_window_number(n)
  warp_window(window_number(n):id())
end

local function ignore_notification()
  hs.execute("notification --close", true)
end

local function activate_notification()
  hs.execute("notification --activate", true)
end

ctrlw = hs.hotkey.modal.new('ctrl', 'w')
swap = hs.hotkey.modal.new('cmd-ctrl-alt-shift', 's') -- bogus, unused key
warp = hs.hotkey.modal.new('cmd-ctrl-alt-shift', 'w') -- also bogus

local function map_all_the_things(mode, keys)
  for key, mapping in pairs(keys) do
    local action = mapping[1] or function() end
    local arg = mapping[2]
    local mods = ''
    if string.match(key, "%u") then
      mods = 'shift'
    end
    if mapping['delay_exiting_mode'] then
      mode:bind(mods, key, function()
        action(arg)
        mode:exit()
      end)
    else
      mode:bind(mods, key, function()
        mode:exit()
        action(arg)
      end)
    end
  end
end

map_all_the_things(ctrlw, {
  escape = {},
  f14    = {},
  ['.']  = {send_control_w, delay_exiting_mode = true},
  b      = {focus_space, 'browse'},
  B      = {send_to_space, 'browse'},
  c      = {focus_space, 'code'},
  C      = {send_to_space, 'code'},
  f      = {toggle_full_screen},
  h      = {move_focus, 'West'},
  j      = {move_focus, 'South'},
  k      = {move_focus, 'North'},
  l      = {move_focus, 'East'},
  r      = {focus_kitty_window, 'kak_repl_window'},
  R      = {focus_kitty_window, 'shell_window'},
  s      = {function() swap:enter() end},
  i      = {function() warp:enter() end},
  I      = {ignore_notification},
  N      = {activate_notification},
  v      = {paste_as_keystrokes},
  ['0']  = {focus_window_number, 0},
  ['1']  = {focus_window_number, 1},
  ['2']  = {focus_window_number, 2},
  ['3']  = {focus_window_number, 3},
  ['4']  = {focus_window_number, 4},
  ['5']  = {focus_window_number, 5},
  ['6']  = {focus_window_number, 6},
  ['7']  = {focus_window_number, 7},
  ['8']  = {focus_window_number, 8},
  ['9']  = {focus_window_number, 9},
  [',']  = {rerun_last_command},
  ['=']  = {balance_space},
  ['/']  = {toggle_split_direction},
})

map_all_the_things(swap, {
  escape = {},
  h      = {swap_window, 'west'},
  j      = {swap_window, 'south'},
  k      = {swap_window, 'north'},
  l      = {swap_window, 'east'},
  ['0']  = {swap_window_number, 0},
  ['1']  = {swap_window_number, 1},
  ['2']  = {swap_window_number, 2},
  ['3']  = {swap_window_number, 3},
  ['4']  = {swap_window_number, 4},
  ['5']  = {swap_window_number, 5},
  ['6']  = {swap_window_number, 6},
  ['7']  = {swap_window_number, 7},
  ['8']  = {swap_window_number, 8},
  ['9']  = {swap_window_number, 9},
})

map_all_the_things(warp, {
  escape = {},
  h      = {warp_window, 'west'},
  j      = {warp_window, 'south'},
  k      = {warp_window, 'north'},
  l      = {warp_window, 'east'},
  ['0']  = {warp_window_number, 0},
  ['1']  = {warp_window_number, 1},
  ['2']  = {warp_window_number, 2},
  ['3']  = {warp_window_number, 3},
  ['4']  = {warp_window_number, 4},
  ['5']  = {warp_window_number, 5},
  ['6']  = {warp_window_number, 6},
  ['7']  = {warp_window_number, 7},
  ['8']  = {warp_window_number, 8},
  ['9']  = {warp_window_number, 9},
})
