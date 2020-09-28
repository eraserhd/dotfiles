
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
      hs.hotkey.new({}, "H", function()
        ctrlw:enter_mode("default")
        hs.execute("yabai -m window --focus west", true)
      end),
      hs.hotkey.new({}, "J", function()
        ctrlw:enter_mode("default")
        hs.execute("yabai -m window --focus south", true)
      end),
      hs.hotkey.new({}, "K", function()
        ctrlw:enter_mode("default")
        hs.execute("yabai -m window --focus north", true)
      end),
      hs.hotkey.new({}, "L", function()
        ctrlw:enter_mode("default")
        hs.execute("yabai -m window --focus east", true)
      end),
    }
  }
}

ctrlw:init()
