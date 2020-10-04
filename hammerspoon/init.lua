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

local Mode = {}

function Mode:new()
  newObj = { keys = {} }
  self.__index = self
  return setmetatable(newObj, self)
end

function Mode:enable()
  for _, key in ipairs(self.keys) do
    key:enable()
  end
end

function Mode:disable()
  for _, key in ipairs(self.keys) do
    key:disable()
  end
end

function Mode:append(keys)
  for _, key in ipairs(keys) do
    table.insert(self.keys, key)
  end
  return self
end

local CtrlW = {}

function CtrlW:new()
  newObj = {
    current_mode = nil,
    modes = {}
  }
  self.__index = self
  return setmetatable(newObj, self)
end

function CtrlW:init()
  self.modes.default = self:make_default_mode()
  self.modes.ctrlw = self:make_ctrlw_mode()
  self.modes.swap = self:make_swap_mode()
  self.modes.warp = self:make_warp_mode()
  self.modes.keycommand = self:make_keycommand_mode()
  self:enter_mode("default")
end

function CtrlW:shell_hotkey(mods, key, command)
  return hs.hotkey.new(mods, key, function()
    self:enter_mode("default")
    hs.execute(command, true)
  end)
end

function CtrlW:make_default_mode()
  return Mode:new():append({
    hs.hotkey.new({"control"}, "W", function() self:enter_mode("ctrlw") end),
    hs.hotkey.new({"command", "shift", "alt", "control"}, "K", function() self:enter_mode("keycommand") end),
  })
end

function CtrlW:make_ctrlw_mode()
  local mode = {}
  mode:append({
    hs.hotkey.new({}, "S", function() self:enter_mode("swap") end),
    hs.hotkey.new({}, "I", function() self:enter_mode("warp") end),
  })
  return mode
end

function CtrlW:make_swap_mode()
  local function swap(to)
    self:enter_mode("default")
    hs.execute("yabai -m window --swap " .. tostring(to), true)
  end

  local mode = Mode:new():append({
    hs.hotkey.new({}, "H", function() swap("west") end),

    hs.hotkey.new({}, "J", function() swap("south") end),
    hs.hotkey.new({}, "K", function() swap("north") end),
    hs.hotkey.new({}, "L", function() swap("east") end),
  })

  for i=0,9 do
    mode:append({
      hs.hotkey.new({}, tostring(i), function()
        local window = window_number(i)
        swap(window:id())
      end)
    })
  end

  return mode
end

function CtrlW:make_warp_mode()
  local function warp(to)
    self:enter_mode("default")
    hs.execute("yabai -m window --space code", true)
    hs.execute("yabai -m window --warp " .. tostring(to), true)
    hs.execute("yabai -m space code --balance", true)
  end

  local mode = Mode:new():append({
    hs.hotkey.new({}, "escape", function() self:enter_mode("default") end),
    hs.hotkey.new({}, "H", function() warp("west") end),
    hs.hotkey.new({}, "J", function() warp("south") end),
    hs.hotkey.new({}, "K", function() warp("north") end),
    hs.hotkey.new({}, "L", function() warp("east") end),
  })

  for i=0,9 do
    mode:append({
      hs.hotkey.new({}, tostring(i), function()
        local window = window_number(i)
        wrap(window:id())
      end)
    })
  end

  return mode
end

function CtrlW:make_keycommand_mode()
  return Mode:new():append({
    hs.hotkey.new({}, "escape", function() self:enter_mode("default") end),
    hs.hotkey.new({}, "H", function()
      hs.window.focusedWindow():focusWindowWest(nil, true, true)
    end),
    hs.hotkey.new({}, "J", function()
      hs.window.focusedWindow():focusWindowSouth(nil, true, true)
    end),
    hs.hotkey.new({}, "K", function()
      hs.window.focusedWindow():focusWindowNorth(nil, true, true)
    end),
    hs.hotkey.new({}, "L", function()
      hs.window.focusedWindow():focusWindowEast(nil, true, true)
    end),
    hs.hotkey.new({}, "N", function() hs.execute("notification --activate", true) end),
    hs.hotkey.new({}, "M", function() hs.execute("notification --menu", true) end),
    hs.hotkey.new({}, "I", function() hs.execute("notification --close", true) end),
  })
end

function CtrlW:enter_mode(mode_name)
  if self.current_mode then
    self.current_mode:disable()
  end
  self.current_mode = self.modes[mode_name]
  self.current_mode:enable()
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

ctrlw = hs.hotkey.modal.new('ctrl', 'w')
swap = hs.hotkey.modal.new('cmd-ctrl-alt-shift', 's') -- bogus, unused key

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
})
