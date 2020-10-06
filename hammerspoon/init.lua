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

local spaces = {
  browse = "Color LCD",
  code   = "PHL 328E1"
}

local function focus_space(space_name)
  local screen = hs.screen.find(spaces[space_name])
  if not screen then return end
  local function isOnScreen(window)
    return window:screen() == screen
  end
  local window = hs.fnutils.filter(hs.window.orderedWindows(), isOnScreen)[1] or hs.window.desktop()
  window:focus()
end

local function send_to_space(space_name)
  local screen = hs.screen.find(spaces[space_name])
  local window = hs.window.focusedWindow()
  window:centerOnScreen(screen, true, 0)
  if space_name == 'browse' then
    window:moveToUnit('[0,0,100,100]')
  end
end

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

local function focus_window(n)
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

local function yabai_window_id(what)
  if type(what) == 'string' then
    return what
  end
  return tostring(window_number(what):id())
end

local function swap_window(what)
  hs.execute("yabai -m window --swap " .. yabai_window_id(what), true)
end

local function warp_window(to)
  hs.execute("yabai -m window --warp " .. yabai_window_id(to), true)
  balance_space()
end

local function ignore_notification()
  hs.execute("notification --close", true)
end

local function activate_notification()
  hs.execute("notification --activate", true)
end

local function map_all_the_things(mode, keys)
  for key, mapping in pairs(keys) do
    local action = mapping[1] or function() end
    local arg = mapping[2]
    local mods = ''
    if string.match(key, "%u") then
      mods = 'shift'
    end
    if mapping['submode'] then
      local submode = map_all_the_things(hs.hotkey.modal.new(), mapping['submode'])
      action = function() submode:enter() end
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
  return mode
end

ctrlw = map_all_the_things(hs.hotkey.modal.new('ctrl', 'w'), {
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
  s      = {submode = {
             escape = {},
             h      = {swap_window, 'west'},
             j      = {swap_window, 'south'},
             k      = {swap_window, 'north'},
             l      = {swap_window, 'east'},
             ['0']  = {swap_window, 0},
             ['1']  = {swap_window, 1},
             ['2']  = {swap_window, 2},
             ['3']  = {swap_window, 3},
             ['4']  = {swap_window, 4},
             ['5']  = {swap_window, 5},
             ['6']  = {swap_window, 6},
             ['7']  = {swap_window, 7},
             ['8']  = {swap_window, 8},
             ['9']  = {swap_window, 9},
           }},
  i      = {submode = {
             escape = {},
             h      = {warp_window, 'west'},
             j      = {warp_window, 'south'},
             k      = {warp_window, 'north'},
             l      = {warp_window, 'east'},
             ['0']  = {warp_window, 0},
             ['1']  = {warp_window, 1},
             ['2']  = {warp_window, 2},
             ['3']  = {warp_window, 3},
             ['4']  = {warp_window, 4},
             ['5']  = {warp_window, 5},
             ['6']  = {warp_window, 6},
             ['7']  = {warp_window, 7},
             ['8']  = {warp_window, 8},
             ['9']  = {warp_window, 9},
           }},
  I      = {ignore_notification},
  N      = {activate_notification},
  v      = {paste_as_keystrokes},
  ['0']  = {focus_window, 0},
  ['1']  = {focus_window, 1},
  ['2']  = {focus_window, 2},
  ['3']  = {focus_window, 3},
  ['4']  = {focus_window, 4},
  ['5']  = {focus_window, 5},
  ['6']  = {focus_window, 6},
  ['7']  = {focus_window, 7},
  ['8']  = {focus_window, 8},
  ['9']  = {focus_window, 9},
  [',']  = {rerun_last_command},
  ['=']  = {balance_space},
  ['/']  = {toggle_split_direction},
})

config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

require("hs.ipc")
