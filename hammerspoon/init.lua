
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
        ctrlw:enter_mode("ctrlw")
        hs.alert.show("enter")
      end)
    },
    ctrlw = {
      hs.hotkey.new({}, "escape", function()
        ctrlw:enter_mode("default")
        hs.alert.show("leave")
      end)
    }
  }
}

ctrlw:init()
