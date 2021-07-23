require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

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

local function ignore_notification()
  hs.execute("notification --close", true)
end

local function activate_notification()
  hs.execute("notification --activate", true)
end

local function focus_window(window)
  window:focus()
  if hs.window.focusedWindow() ~= window then
    window:focus()
  end
end

local function swap_window(window)
  local focused_frame = hs.window.focusedWindow():frame()
  local selected_frame = window:frame()
  hs.window.focusedWindow():setFrame(selected_frame, 0)
  window:setFrame(focused_frame, 0)
end

local function stack_window(window)
  local frame = window:frame()
  hs.window.focusedWindow():setFrame(frame, 0)
end

local function split_vertically()
  local window = hs.window.focusedWindow()
  local frame = window:frame()
  frame.w = frame.w/2
  window:setFrame(frame, 0)
end

local function split_horizontally()
  local window = hs.window.focusedWindow()
  local frame = window:frame()
  frame.h = frame.h/2
  window:setFrame(frame, 0)
end

--not_sigils = hs.loadSpoon("WindowSigils")
not_sigils = dofile('/Users/jfelice/src/Spoons/Source/WindowSigils.spoon/init.lua')
not_sigils:configure({
  hotkeys = {
    enter = {{"control"}, "W"}
  },
  mode_keys = {
    [{{'shift'}, 'f'}]  = toggle_full_screen,
    [{{'shift'}, 'i'}]  = ignore_notification,
    [{{'shift'}, 'n'}]  = activate_notification,
    [{{}, '-'}]         = split_horizontally,
    [{{'shift'}, '\\'}] = split_vertically,
    [{{}, 'v'}]         = paste_as_keystrokes,
    [{{}, ','}]         = rerun_last_command,
  },
  sigil_actions = {
    [{}]       = focus_window,
    [{'ctrl'}] = stack_window,
    [{'alt'}]  = swap_window,
  }
})

not_sigils:start()

mouse_follows_focus = hs.loadSpoon("MouseFollowsFocus")
mouse_follows_focus:configure({})
mouse_follows_focus:start()
