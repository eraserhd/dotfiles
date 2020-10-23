require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

sigils = hs.loadSpoon("WindowSigils")
sigils:bindHotkeys({
  enter = {{"control"}, "W"}
})

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

sigils:bindModeKey({'shift'}, 'f', toggle_full_screen)
sigils:bindModeKey({'shift'}, 'i', ignore_notification)
sigils:bindModeKey({'shift'}, 'n', activate_notification)
sigils:bindModeKey({}, 'v', paste_as_keystrokes)
sigils:bindModeKey({}, ',', rerun_last_command)
sigils:bindModeKey({}, '=', balance_space)
sigils:bindModeKey({}, '/', toggle_split_direction)

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

sigils:start()
