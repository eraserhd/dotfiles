require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

sigils = hs.loadSpoon("WindowSigils")
sigils:start()

local function send_control_w()
  hs.eventtap.keyStroke({"control"}, "W")
end

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

local function paste_as_keystrokes()
  hs.eventtap.keyStrokes(hs.pasteboard.readString())
end

local function focus_window(what)
  local window = sigils:window(what)
  window:focus()
  if hs.window.focusedWindow() ~= window then
    window:focus()
  end
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
  hs.execute("yabai -m window --swap " .. sigils:window(what):id(), true)
end

local function warp_window(to)
  hs.execute("yabai -m window --warp " .. sigils:window(to):id(), true)
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
    local mods = {}
    for mod in string.gmatch(key, "([^-]+)") do
      table.insert(mods, mod)
    end
    key = mods[#mods]
    mods[#mods] = nil
    if string.match(key, "%u") then
      table.insert(mods, 'shift')
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
  F      = {toggle_full_screen},
  I      = {ignore_notification},
  N      = {activate_notification},
  v      = {paste_as_keystrokes},

  -- Focusing
  h     = {focus_window, 'West'},
  j     = {focus_window, 'South'},
  k     = {focus_window, 'North'},
  l     = {focus_window, 'East'},

  a     = {focus_window, 'a'},
  b     = {focus_window, 'b'},
  c     = {focus_window, 'c'},
  d     = {focus_window, 'd'},
  e     = {focus_window, 'e'},
  f     = {focus_window, 'f'},
  g     = {focus_window, 'g'},
  m     = {focus_window, 'm'},
  n     = {focus_window, 'n'},
  o     = {focus_window, 'o'},
  p     = {focus_window, 'p'},
  q     = {focus_window, 'q'},
  r     = {focus_window, 'r'},
  t     = {focus_window, 't'},
  u     = {focus_window, 'u'},
  w     = {focus_window, 'w'},
  x     = {focus_window, 'x'},
  y     = {focus_window, 'y'},
  z     = {focus_window, 'z'},
  ['0'] = {focus_window, '0'},
  ['1'] = {focus_window, '1'},
  ['2'] = {focus_window, '2'},
  ['3'] = {focus_window, '3'},
  ['4'] = {focus_window, '4'},
  ['5'] = {focus_window, '5'},
  ['6'] = {focus_window, '6'},
  ['7'] = {focus_window, '7'},
  ['8'] = {focus_window, '8'},
  ['9'] = {focus_window, '9'},

  -- Swapping
  ['ctrl-h'] = {swap_window, 'West'},
  ['ctrl-j'] = {swap_window, 'South'},
  ['ctrl-k'] = {swap_window, 'North'},
  ['ctrl-l'] = {swap_window, 'East'},

  ['ctrl-a'] = {swap_window, 'a'},
  ['ctrl-b'] = {swap_window, 'b'},
  ['ctrl-c'] = {swap_window, 'c'},
  ['ctrl-d'] = {swap_window, 'd'},
  ['ctrl-e'] = {swap_window, 'e'},
  ['ctrl-f'] = {swap_window, 'f'},
  ['ctrl-g'] = {swap_window, 'g'},
  ['ctrl-m'] = {swap_window, 'm'},
  ['ctrl-n'] = {swap_window, 'n'},
  ['ctrl-o'] = {swap_window, 'o'},
  ['ctrl-p'] = {swap_window, 'p'},
  ['ctrl-q'] = {swap_window, 'q'},
  ['ctrl-r'] = {swap_window, 'r'},
  ['ctrl-t'] = {swap_window, 't'},
  ['ctrl-u'] = {swap_window, 'u'},
  ['ctrl-w'] = {swap_window, 'w'},
  ['ctrl-x'] = {swap_window, 'x'},
  ['ctrl-y'] = {swap_window, 'y'},
  ['ctrl-z'] = {swap_window, 'z'},
  ['ctrl-0'] = {swap_window, '0'},
  ['ctrl-1'] = {swap_window, '1'},
  ['ctrl-2'] = {swap_window, '2'},
  ['ctrl-3'] = {swap_window, '3'},
  ['ctrl-4'] = {swap_window, '4'},
  ['ctrl-5'] = {swap_window, '5'},
  ['ctrl-6'] = {swap_window, '6'},
  ['ctrl-7'] = {swap_window, '7'},
  ['ctrl-8'] = {swap_window, '8'},
  ['ctrl-9'] = {swap_window, '9'},

  -- Warping
  ['alt-h'] = {warp_window, 'West'},
  ['alt-j'] = {warp_window, 'South'},
  ['alt-k'] = {warp_window, 'North'},
  ['alt-l'] = {warp_window, 'East'},

  ['alt-a'] = {warp_window, 'a'},
  ['alt-b'] = {warp_window, 'b'},
  ['alt-c'] = {warp_window, 'c'},
  ['alt-d'] = {warp_window, 'd'},
  ['alt-e'] = {warp_window, 'e'},
  ['alt-f'] = {warp_window, 'f'},
  ['alt-g'] = {warp_window, 'g'},
  ['alt-m'] = {warp_window, 'm'},
  ['alt-n'] = {warp_window, 'n'},
  ['alt-o'] = {warp_window, 'o'},
  ['alt-p'] = {warp_window, 'p'},
  ['alt-q'] = {warp_window, 'q'},
  ['alt-r'] = {warp_window, 'r'},
  ['alt-t'] = {warp_window, 't'},
  ['alt-u'] = {warp_window, 'u'},
  ['alt-w'] = {warp_window, 'w'},
  ['alt-x'] = {warp_window, 'x'},
  ['alt-y'] = {warp_window, 'y'},
  ['alt-z'] = {warp_window, 'z'},
  ['alt-0'] = {warp_window, '0'},
  ['alt-1'] = {warp_window, '1'},
  ['alt-2'] = {warp_window, '2'},
  ['alt-3'] = {warp_window, '3'},
  ['alt-4'] = {warp_window, '4'},
  ['alt-5'] = {warp_window, '5'},
  ['alt-6'] = {warp_window, '6'},
  ['alt-7'] = {warp_window, '7'},
  ['alt-8'] = {warp_window, '8'},
  ['alt-9'] = {warp_window, '9'},

  [',']  = {rerun_last_command},
  ['=']  = {balance_space},
  ['/']  = {toggle_split_direction},
})
