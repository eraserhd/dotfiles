require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

sigils = hs.loadSpoon("WindowSigils")
sigils:bindHotkeys({
  enter = {{"control"}, "W"}
})
sigils:start()

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

local function paste_as_keystrokes()
  hs.eventtap.keyStrokes(hs.pasteboard.readString())
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
  F      = {toggle_full_screen},
  I      = {ignore_notification},
  N      = {activate_notification},
  v      = {paste_as_keystrokes},

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
