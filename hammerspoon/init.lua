require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

sigils = hs.loadSpoon("WindowSigils")
sigils:bindHotkeys({
  enter = {{"ctrl"}, "w"}
})
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

local function map_all_the_things(keys)
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
      sigils.mode:bind(mods, key, function()
        action(arg)
        sigils.mode:exit()
      end)
    else
      sigils.mode:bind(mods, key, function()
        sigils.mode:exit()
        action(arg)
      end)
    end
  end
end

ctrlw = map_all_the_things({
  escape = {},
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

  -- Swapping
  ['ctrl-h'] = {swap_window, 'West'},
  ['ctrl-j'] = {swap_window, 'South'},
  ['ctrl-k'] = {swap_window, 'North'},
  ['ctrl-l'] = {swap_window, 'East'},

  -- Warping
  ['alt-h'] = {warp_window, 'West'},
  ['alt-j'] = {warp_window, 'South'},
  ['alt-k'] = {warp_window, 'North'},
  ['alt-l'] = {warp_window, 'East'},

  [',']  = {rerun_last_command},
  ['=']  = {balance_space},
  ['/']  = {toggle_split_direction},
})

sigils:bindSigilAction({}, function(window)
  window:focus()
  if hs.window.focusedWindow() ~= window then
    window:focus()
  end
end)

sigils:bindSigilAction({'ctrl'}, function(window)
  hs.execute("yabai -m window --swap " .. window:id(), true)
end)

sigils:bindSigilAction({'alt'}, function(window)
  hs.execute("yabai -m window --warp " .. window:id(), true)
  balance_space()
end)
