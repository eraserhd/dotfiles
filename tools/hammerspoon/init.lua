require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

-- Window geometry and workspaces belong to AeroSpace (see tools/aerospace);
-- WindowSigils just keeps the 'C-w' prefix and drives it from here.
local function aerospace(command)
  hs.execute("/run/current-system/sw/bin/aerospace " .. command)
end

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
  [{{}, '-'}]         = function() aerospace("split vertical") end,
  [{{'shift'}, '\\'}] = function() aerospace("split horizontal") end,
  [{{}, 'delete'}]    = function() aerospace("close") end,
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
  mode_keys[{{}, key}] = function() aerospace("summon-workspace " .. workspace) end
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
