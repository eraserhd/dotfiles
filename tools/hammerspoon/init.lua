require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

local function aerospace(command)
  return hs.execute("/run/current-system/sw/bin/aerospace " .. command)
end

-- A centered, floating kitty running broot (see tools/broot) which plumbs the
-- chosen file and goes away.
local function broot_popup()
  local focused = hs.window.focusedWindow()
  local screen = focused and focused:screen() or hs.screen.mainScreen()
  local f = screen:frame()
  local popup = hs.geometry.rect(f.x + f.w * 0.25, f.y + f.h * 0.2, f.w * 0.5, f.h * 0.6)

  -- Open at the final size, since broot misses resizes which arrive while it
  -- is starting and stays small.  AeroSpace floats the window on detection
  -- (see tools/aerospace), so it isn't tiled to some other size first.
  local task = hs.task.new("/run/current-system/sw/bin/kitty", nil, {
    "--title=broot-popup",
    "-o", "remember_window_size=no",
    "-o", "initial_window_width=" .. math.floor(popup.w),
    "-o", "initial_window_height=" .. math.floor(popup.h),
    "-o", "confirm_os_window_close=0",
    "/run/current-system/sw/bin/broot-popup",
  })
  task:start()

  -- Poll fast, and only the one process, so it's moved before it's seen.
  local attempts = 0
  local timer
  timer = hs.timer.doEvery(0.01, function()
    attempts = attempts + 1
    local app = hs.application.applicationForPID(task:pid())
    local window = app and app:mainWindow()
    if window then
      timer:stop()
      window:setFrame(popup)
      window:focus()
    elseif attempts > 500 then
      timer:stop()
    end
  end)
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
