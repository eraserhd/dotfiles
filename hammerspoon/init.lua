
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
      hs.hotkey.new({"alt", "control"}, "W", function()
        hs.alert.show("enter")
        ctrlw:enter_mode("ctrlw")
      end)
    },
    ctrlw = {
      hs.hotkey.new({}, "escape", function()
        ctrlw:enter_mode("default")
      end),
    }
  }
}

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

ctrlw:init()
