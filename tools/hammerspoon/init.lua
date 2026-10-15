require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

-- Window geometry and workspaces belong to AeroSpace (see tools/aerospace);
-- WindowSigils just keeps the 'C-w' prefix and drives it from here.
local function aerospace(command)
  return hs.execute("/run/current-system/sw/bin/aerospace " .. command)
end

-- Like XMonad's greedyView: a workspace showing on another monitor swaps with
-- the focused monitor's, where 'summon-workspace' alone sends ours somewhere
-- arbitrary.
local function swap_in_workspace(workspace)
  local current_workspace = aerospace("list-workspaces --focused"):match("%S+")
  local current_monitor = aerospace("list-monitors --focused --format '%{monitor-id}'"):match("%S+")
  local visible = aerospace("list-workspaces --monitor all --visible --format '%{workspace} %{monitor-id}'")
  local other_monitor
  for name, monitor in visible:gmatch("(%S+) (%S+)") do
    if name == workspace and monitor ~= current_monitor then other_monitor = monitor end
  end

  aerospace("summon-workspace " .. workspace)
  if other_monitor then
    aerospace("focus-monitor " .. other_monitor)
    aerospace("summon-workspace " .. current_workspace)
    aerospace("focus-monitor " .. current_monitor)
  end
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
  hs.task.new("/run/current-system/sw/bin/kitty", nil, {
    "--title=broot-popup",
    "-o", "remember_window_size=no",
    "-o", "initial_window_width=" .. math.floor(popup.w),
    "-o", "initial_window_height=" .. math.floor(popup.h),
    "-o", "confirm_os_window_close=0",
    "/run/current-system/sw/bin/broot-popup",
  }):start()

  local attempts = 0
  local timer
  timer = hs.timer.doEvery(0.1, function()
    attempts = attempts + 1
    local window = hs.fnutils.find(hs.window.allWindows(), function(w)
      return w:title() == "broot-popup"
    end)
    if window then
      timer:stop()
      aerospace("layout floating --window-id " .. window:id())
      window:setFrame(popup)
      window:focus()
      -- In case the resizes still beat broot's startup.
      hs.timer.doAfter(0.5, function()
        hs.execute("/usr/bin/pkill -WINCH -f broot-popup.toml")
      end)
    elseif attempts > 50 then
      timer:stop()
    end
  end)
end

-- The monitors switch to the other machine when this one sleeps; take them
-- back on wake (see tools/monitors).  This runs while the screen is locked,
-- but the monitors may take a while to answer DDC after waking.
local function monitors_here(attempt)
  attempt = attempt or 1
  hs.task.new("/run/current-system/sw/bin/monitors-here", function(code, _, stderr)
    if code == 0 then
      return
    elseif attempt < 6 then
      hs.timer.doAfter(2, function() monitors_here(attempt + 1) end)
    else
      hs.alert.show("monitors-here failed: " .. stderr)
    end
  end):start()
end

monitors_wake_watcher = hs.caffeinate.watcher.new(function(event)
  if event == hs.caffeinate.watcher.systemDidWake
      or event == hs.caffeinate.watcher.screensDidWake then
    hs.timer.doAfter(1, monitors_here)
  end
end):start()

local function paste_as_keystrokes()
  hs.eventtap.keyStrokes(hs.pasteboard.readString())
end

local function rerun_last_command()
  kitty("send-text --match=title:kak_repl_window '\x10\x0d'")
end

local function focus_window(window)
  window:focus()
  if hs.window.focusedWindow() ~= window then
    window:focus()
  end
end

not_sigils = hs.loadSpoon("WindowSigils")
--not_sigils = dofile('/Users/jfelice/src/Spoons/Source/WindowSigils.spoon/init.lua')

-- SketchyBar isn't a window; keep its strip (see tools/sketchybar) from
-- becoming an empty space with a sigil.
not_sigils.reservedInsets = { top = 40 }

-- AeroSpace emulates workspaces by parking the windows of inactive ones just
-- off the visible area rather than using macOS Spaces.  They stay unminimized,
-- so the spoon's window filter still returns them, and since sigils are
-- assigned by frame position they would both consume letters and shift the
-- assignment of the windows actually on screen.  Both orderedWindows() and
-- _makeSigilBoxes() funnel through here, so one wrapper keeps the keys and the
-- overlay agreeing.  Belongs upstream in eraserhd/Spoons eventually.
local base_removeUnuseableWindows = not_sigils._removeUnuseableWindows
function not_sigils:_removeUnuseableWindows(windows)
  return hs.fnutils.filter(base_removeUnuseableWindows(self, windows), function(window)
    local frame = window:frame()
    for _, screen in ipairs(hs.screen.allScreens()) do
      local overlap = frame:intersect(screen:frame())
      -- Not .area: disjoint rects can intersect to two negative dimensions.
      if overlap.w > 0 and overlap.h > 0 then
        return true
      end
    end
    return false
  end)
end

local mode_keys = {
  [{{}, 'f'}]         = function() aerospace("fullscreen") end,
  [{{}, '='}]         = function() aerospace("balance-sizes") end,
  [{{}, '-'}]         = function() aerospace("layout v_tiles") end,
  [{{'shift'}, '\\'}] = function() aerospace("layout h_tiles") end,
  [{{}, 'delete'}]    = function() aerospace("close") end,
  [{{}, 'space'}]     = broot_popup,
  [{{}, 'v'}]         = paste_as_keystrokes,
  [{{}, ','}]         = rerun_last_command,
}

-- 'swap' and 'join-with' take a direction rather than a target window, so these
-- are mode keys instead of sigil actions.  h/j/k/l are never sigils.
for key, direction in pairs({ h = 'left', j = 'down', k = 'up', l = 'right' }) do
  mode_keys[{{'alt'}, key}] = function() aerospace("swap " .. direction) end
  mode_keys[{{'ctrl'}, key}] = function() aerospace("join-with " .. direction) end
end

-- Binding the digits as mode keys also drops them from the sigil pool, leaving
-- the letters that tools/xmonad/config/xmonad.hs uses.
for digit = 0, 9 do
  local key = tostring(digit)
  local workspace = (digit == 0) and "10" or key
  mode_keys[{{}, key}] = function() swap_in_workspace(workspace) end
  mode_keys[{{'ctrl'}, key}] = function() aerospace("move-node-to-workspace " .. workspace) end
end

not_sigils:configure({
  hotkeys = {
    enter = {{"control"}, "W"}
  },
  mode_keys = mode_keys,
  sigil_actions = {
    [{}] = focus_window,
  }
})

not_sigils:start()

mouse_follows_focus = hs.loadSpoon("MouseFollowsFocus")
mouse_follows_focus:configure({})
mouse_follows_focus:start()

-- Start kitty if it is not open
if hs.application.find('net.kovidgoyal.kitty') == nil then
    hs.application.open('/run/current-system/Applications/kitty.app')
end
