--- === WindowSigils ===
---
--- A new Sample Spoon
---
--- Download: [https://github.com/Hammerspoon/Spoons/raw/master/Spoons/WindowSigils.spoon.zip](https://github.com/Hammerspoon/Spoons/raw/master/Spoons/WindowSigils.spoon.zip)

local obj={}
obj.__index = obj

-- Metadata
obj.name = "WindowSigils"
obj.version = "0.1"
obj.author = "Jason Felice <jason.m.felice@gmail.com>"
obj.homepage = "https://github.com/Hammerspoon/Spoons"
obj.license = "MIT - https://opensource.org/licenses/MIT"

--- WindowSigils.logger
--- Variable
--- Logger object used within the Spoon. Can be accessed to set the default log level for the messages coming from the Spoon.
obj.logger = hs.logger.new('WindowSigils')


--- Some internal variable
obj.key_hello = nil

--- WindowSigils.some_config_param
--- Variable
--- Some configuration parameter
obj.some_config_param = true

--- WindowSigils:sayHello()
--- Method
--- Greet the user
function obj:sayHello()
  hs.alert.show("Hello!")
  return self
end

function obj:init()
end

--- WindowSigils:bindHotkeys(mapping)
--- Method
--- Binds hotkeys for WindowSigils
---
--- Parameters:
---  * mapping - A table containing hotkey objifier/key details for the following items:
---   * hello - Say Hello
function obj:bindHotkeys(mapping)
  if mapping["hello"] then
    if (self.key_hello) then
      self.key_hello:delete()
    end
    self.key_hello = hs.hotkey.bindSpec(mapping["hello"], function() self:sayHello() end)
  end
end

--- WindowSigils:start()
--- Method
--- Starts rendering the sigils and handling hotkeys
---
--- Parameters:
function obj:start()
  self.window_filter = hs.window.filter.new({override={
    visible = true,
  }}):setDefaultFilter({
    visible = true,
  })
end

--- WindowSigils:stop()
--- Method
--- Stops rendering the sigils and handling hotkeys
---
--- Parameters:
function obj:stop()
  self.window_filter = nil
end


--- WindowSigils:orderedWindows()
--- Method
--- A list of windows, in the order sigils are assigned.
function obj:orderedWindows()
  local windows = self.window_filter:getWindows()
  table.sort(windows, function(a, b)
    local af, bf = a:frame(), b:frame()
    if af.x < bf.x then return true end
    if af.x > bf.x then return false end
    return af.y < bf.y
  end)
  return windows
end

--- WindowSigils:window(sigil)
--- Method
--- Find the window with the given index or sigil.
---
--- Parameters:
---  * sigil - If a number, the index of the window; if a string, the sigil of the window.
---    Can also be 'North', 'East', 'South', or 'West' to find a window related to the
---    currently focused window.
function obj:window(sigil)
  if type(sigil) == 'number' then
    return self:orderedWindows()[sigil]
  elseif sigil == 'North' then
    return hs.window.focusedWindow():windowsToNorth(nil, true, true)[1]
  elseif sigil == 'East' then
    return hs.window.focusedWindow():windowsToEast(nil, true, true)[1]
  elseif sigil == 'South' then
    return hs.window.focusedWindow():windowsToSouth(nil, true, true)[1]
  elseif sigil == 'West' then
    return hs.window.focusedWindow():windowsToWest(nil, true, true)[1]
  else
    local sigils = {
      "a",
      "b", "c", "d", "e", "f", "g", "m", "n", "o", "p", "q", "r", "t", "u",
      "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    }
    for i,k in ipairs(sigils) do
      if k == sigil then
        return self:orderedWindows()[i]
      end
    end
  end
  return nil
end

return obj
