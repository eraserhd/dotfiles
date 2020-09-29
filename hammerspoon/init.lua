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
local ctrlw

function CtrlW:new()
  newObj = {
    current_mode = nil,
    modes = {
      default = Mode:new():append({
        hs.hotkey.new({"alt", "control"}, "W", function() ctrlw:enter_mode("ctrlw") end)
      }),
      ctrlw = Mode:new():append({
        hs.hotkey.new({}, "escape", function() ctrlw:enter_mode("default") end)
      })
    }
  }
  self.__index = self
  return setmetatable(newObj, self)
end

function CtrlW:init()
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

function window_number(n)
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

function hotkey_exec(mode_table, mods, key, command)
  mode_table:append({
    hs.hotkey.new(mods, key, function()
      ctrlw:enter_mode("default")
      hs.execute(command, true)
    end)
  })
end

hotkey_exec(ctrlw.modes.ctrlw, {},        "H", "yabai -m window --focus west")
hotkey_exec(ctrlw.modes.ctrlw, {},        "J", "yabai -m window --focus south")
hotkey_exec(ctrlw.modes.ctrlw, {},        "K", "yabai -m window --focus north")
hotkey_exec(ctrlw.modes.ctrlw, {},        "L", "yabai -m window --focus east")
hotkey_exec(ctrlw.modes.ctrlw, {},        "P", "yabai -m window --focus recent")
hotkey_exec(ctrlw.modes.ctrlw, {},        "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:kak_repl_window")
hotkey_exec(ctrlw.modes.ctrlw, {"shift"}, "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:shell_window")

for i=0,9 do
  ctrlw.modes.ctrlw:append({
    hs.hotkey.new({}, tostring(i), function()
      ctrlw:enter_mode("default")
      local window = window_number(i)
      window:focus()
    end)
  })
end

hotkey_exec(ctrlw.modes.ctrlw, {}, "C", "yabai-focus-space code")
hotkey_exec(ctrlw.modes.ctrlw, {}, "F", "yabai-focus-space focus")
hotkey_exec(ctrlw.modes.ctrlw, {}, "B", "yabai-focus-space browse")

ctrlw:init()
