--- === Bubbles ===
---
--- Download: [https://github.com/Hammerspoon/Spoons/raw/master/Spoons/Bubbles.spoon.zip](https://github.com/Hammerspoon/Spoons/raw/master/Spoons/Bubbles.spoon.zip)

local obj={}
obj.__index = obj

-- Metadata
obj.name = "Bubbles"
obj.version = "0.1"
obj.author = "Jason Felice <jason.m.felice@gmail.com>"
obj.homepage = "https://github.com/Hammerspoon/Spoons"
obj.license = "MIT - https://opensource.org/licenses/MIT"

--- Bubbles.logger
--- Variable
--- Logger object used within the Spoon. Can be accessed to set the default log level for the messages coming from the Spoon.
obj.logger = hs.logger.new('Bubbles')

--- Bubbles:configure(configuration)
--- Method
--- Configures the spoon.
---
--- Parameters:
---   * configuration - :
---    * hotkeys -
function obj:configure(configuration)
end

--- Bubbles:start()
--- Method
--- Starts rendering the sigils and handling hotkeys
---
--- Parameters:
function obj:start()
end

--- Bubbles:stop()
--- Method
--- Stops rendering the sigils and handling hotkeys
---
--- Parameters:
function obj:stop()
end

return obj
