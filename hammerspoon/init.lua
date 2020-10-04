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
    hs.hotkey.new({}, "F", function()
      self:enter_mode("default")
      hs.window.focusedWindow():toggleFullScreen()
    end),

    self:shell_hotkey({}, ",", "kitty @ --to unix:/Users/jfelice/.run/kitty send-text --match=title:kak_repl_window '\x10\x0d'"),
    self:shell_hotkey({}, "=", "yabai -m space --balance"),
    self:shell_hotkey({}, "/", "yabai -m window --toggle split"),

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
    hs.hotkey.new({}, "escape", function() self:enter_mode("default") end),
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

ctrlw = hs.hotkey.modal.new('ctrl', 'w')

local function moveFocus(direction)
  ctrlw:exit()
  local window = hs.window.focusedWindow()
  window['focusWindow' .. direction](window, nil, true, true)
end

local function actualCtrlW()
  hs.eventtap.keyStroke({"control"}, "W")
  ctrlw:exit()
end

ctrlw:bind('', 'escape', function() ctrlw:exit() end)
ctrlw:bind('', 'f14', function() ctrlw:exit() end)
ctrlw:bind('', '.', function() actualCtrlW() end)
ctrlw:bind('', 'h', function() moveFocus('West') end)
ctrlw:bind('', 'j', function() moveFocus('South') end)
ctrlw:bind('', 'k', function() moveFocus('North') end)
ctrlw:bind('', 'l', function() moveFocus('East') end)
ctrlw:bind('', 'r', function()
  ctrlw:exit()
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:kak_repl_window", true)
end)
ctrlw:bind('shift', 'R', function()
  ctrlw:exit()
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:shell_window", true)
end)
ctrlw:bind('', 'v', function()
  ctrlw:exit()
  hs.eventtap.keyStrokes(hs.pasteboard.readString())
end)
for i=0,9 do
  ctrlw:bind('', tostring(i), function()
    ctrlw:exit()
    window_number(i):focus()
  end)
end
ctrlw:bind('', 'c', function()
  ctrlw:exit()
  hs.execute("yabai-focus-space code", true)
end)
ctrlw:bind('shift', 'c', function()
  ctrlw:exit()
  send_to_space("code")
end)
ctrlw:bind('', 'b', function()
  ctrlw:exit()
  hs.execute("yabai-focus-space browse", true)
end)
ctrlw:bind('shift', 'b', function()
  ctrlw:exit()
  send_to_space("browse")
end)

