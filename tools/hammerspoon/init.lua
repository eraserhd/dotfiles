require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

local function aerospace(command)
  return hs.execute("/run/current-system/sw/bin/aerospace " .. command)
end

-- Moving the mouse to another monitor focuses it, so new windows and the
-- workspace commands act there.  Wait for the pointer to settle, since
-- AeroSpace moves it briefly while focus passes between monitors.
local function monitor_position(screen)
  local x = screen:fullFrame().x
  local position = 1
  for _, other in ipairs(hs.screen.allScreens()) do
    if other:fullFrame().x < x then position = position + 1 end
  end
  return position
end

local mouse_screen_id, settled_ticks = nil, 0
mouse_monitor_watcher = hs.timer.doEvery(0.1, function()
  local screen = hs.mouse.getCurrentScreen()
  if not screen then return end
  if screen:id() ~= mouse_screen_id then
    mouse_screen_id, settled_ticks = screen:id(), 0
    return
  end
  settled_ticks = settled_ticks + 1
  if settled_ticks ~= 2 then return end

  local position = monitor_position(screen)
  local focused = aerospace("list-monitors --focused --format '%{monitor-id}'"):match("%S+")
  if focused ~= tostring(position) then
    aerospace("focus-monitor " .. position)
  end
end)

-- Start kitty if it is not open
if hs.application.find('net.kovidgoyal.kitty') == nil then
    hs.application.open('/run/current-system/Applications/kitty.app')
end
