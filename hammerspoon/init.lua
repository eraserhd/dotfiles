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

  local function shell_hotkey(mods, key, command)
    return hs.hotkey.new(mods, key, function()
      self:enter_mode("default")
      hs.execute(command, true)
    end)
  end

  self.modes.default = Mode:new():append({
    hs.hotkey.new({"alt", "control"}, "W", function() self:enter_mode("ctrlw") end)
  })

  self.modes.ctrlw = Mode:new():append({
    hs.hotkey.new({}, "escape", function() self:enter_mode("default") end),
    hs.hotkey.new({}, "f14", function() self:enter_mode("default") end),
    shell_hotkey({},        "H", "yabai -m window --focus west"),
    shell_hotkey({},        "J", "yabai -m window --focus south"),
    shell_hotkey({},        "K", "yabai -m window --focus north"),
    shell_hotkey({},        "L", "yabai -m window --focus east"),
    shell_hotkey({},        "P", "yabai -m window --focus recent"),
    shell_hotkey({},        "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:kak_repl_window"),
    shell_hotkey({"shift"}, "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:shell_window"),
  })

  for i=0,9 do
    self.modes.ctrlw:append({
      hs.hotkey.new({}, tostring(i), function()
        self:enter_mode("default")
        local window = window_number(i)
        window:focus()
      end)
    })
  end

  self.modes.ctrlw:append({
    shell_hotkey({}, "C", "yabai-focus-space code"),
    shell_hotkey({}, "F", "yabai-focus-space focus"),
    shell_hotkey({}, "B", "yabai-focus-space browse"),
  })

  self:enter_mode("default")
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
