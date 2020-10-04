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
  local mode = Mode:new():append({
    hs.hotkey.new({}, "escape", function() self:enter_mode("default") end),
    hs.hotkey.new({}, "f14", function() self:enter_mode("default") end),

    hs.hotkey.new({}, ".", function()
      hs.eventtap.keyStroke({"control"}, "W")
      self:enter_mode("default")
    end),

    hs.hotkey.new({}, "H", function()
      self:enter_mode("default")
      hs.window.focusedWindow():focusWindowWest(nil, true, true)
    end),
    hs.hotkey.new({}, "J", function()
      self:enter_mode("default")
      hs.window.focusedWindow():focusWindowSouth(nil, true, true)
    end),
    hs.hotkey.new({}, "K", function()
      self:enter_mode("default")
      hs.window.focusedWindow():focusWindowNorth(nil, true, true)
    end),
    hs.hotkey.new({}, "L", function()
      self:enter_mode("default")
      hs.window.focusedWindow():focusWindowEast(nil, true, true)
    end),
    self:shell_hotkey({},        "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:kak_repl_window"),
    self:shell_hotkey({"shift"}, "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:shell_window"),

    hs.hotkey.new({}, "V", function()
      self:enter_mode("default")
      hs.eventtap.keyStrokes(hs.pasteboard.readString())
    end),
  })

  for i=0,9 do
    mode:append({
      hs.hotkey.new({}, tostring(i), function()
        self:enter_mode("default")
        local window = window_number(i)
        window:focus()
      end)
    })
  end

  local function send_to_space(space_name)
    local window = hs.window.focusedWindow()
    hs.execute("yabai -m window --space " .. space_name, true)
    hs.execute("yabai-focus-space " .. space_name, true)
    window:focus()
  end

  mode:append({
    self:shell_hotkey({}, "C", "yabai-focus-space code"),
    hs.hotkey.new({"shift"}, "C", function()
      self:enter_mode("default")
      send_to_space("code")
    end),
    self:shell_hotkey({}, "B", "yabai-focus-space browse"),
    hs.hotkey.new({"shift"}, "B", function()
      self:enter_mode("default")
      send_to_space("browse")
    end),

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

ctrlw = CtrlW:new()
ctrlw:init()

config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

ctrlaltw = hs.hotkey.modal.new('ctrl-alt', 'w')
function ctrlaltw:entered()
  hs.alert("Entered ctrlaltw mode")
end
function ctrlaltw:exited()
  hs.alert("Exited ctrlaltw mode")
end

ctrlaltw:bind('', 'escape', function() ctrlaltw:exit() end)
ctrlaltw:bind('', 'f14', function() ctrlaltw:exit() end)
ctrlaltw:bind('', 'h', function()
  ctrlaltw:exit()
  hs.window.focusedWindow():focusWindowWest(nil, true, true)
end)
ctrlaltw:bind('', 'j', function()
  ctrlaltw:exit()
  hs.window.focusedWindow():focusWindowSouth(nil, true, true)
end)
ctrlaltw:bind('', 'k', function()
  ctrlaltw:exit()
  hs.window.focusedWindow():focusWindowNorth(nil, true, true)
end)
ctrlaltw:bind('', 'l', function()
  ctrlaltw:exit()
  hs.window.focusedWindow():focusWindowEast(nil, true, true)
end)
