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

return obj
