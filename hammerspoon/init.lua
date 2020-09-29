
ctrlw = {
  init = function(self)
    self:enter_mode("default")
  end,

  enter_mode = function(self, mode)
    if self.current_mode then
      for _, hotkey in pairs(self.modes[self.current_mode]) do
        hotkey:disable()
      end
    end
    for _, hotkey in pairs(self.modes[mode]) do
      hotkey:enable()
    end
    self.current_mode = mode
  end,

  current_mode = nil,
  modes = {
    default = {
      hs.hotkey.new({"alt", "control"}, "W", function() ctrlw:enter_mode("ctrlw") end)
    },
    ctrlw = {
      hs.hotkey.new({}, "escape", function() ctrlw:enter_mode("default") end),
    }
  }
}

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
  table.insert(mode_table, hs.hotkey.new(mods, key, function()
    ctrlw:enter_mode("default")
    hs.execute(command, true)
  end))
end

hotkey_exec(ctrlw.modes.ctrlw, {},        "H", "yabai -m window --focus west")
hotkey_exec(ctrlw.modes.ctrlw, {},        "J", "yabai -m window --focus south")
hotkey_exec(ctrlw.modes.ctrlw, {},        "K", "yabai -m window --focus north")
hotkey_exec(ctrlw.modes.ctrlw, {},        "L", "yabai -m window --focus east")
hotkey_exec(ctrlw.modes.ctrlw, {},        "P", "yabai -m window --focus recent")
hotkey_exec(ctrlw.modes.ctrlw, {},        "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:kak_repl_window")
hotkey_exec(ctrlw.modes.ctrlw, {"shift"}, "R", "kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:shell_window")

for i=0,9 do
  table.insert(ctrlw.modes.ctrlw, hs.hotkey.new({}, tostring(i), function()
    ctrlw:enter_mode("default")
    local window = window_number(i)
    window:focus()
  end))
end

hotkey_exec(ctrlw.modes.ctrlw, {}, "C", "yabai-focus-space code")
hotkey_exec(ctrlw.modes.ctrlw, {}, "F", "yabai-focus-space focus")
hotkey_exec(ctrlw.modes.ctrlw, {}, "B", "yabai-focus-space browse")

ctrlw:init()
