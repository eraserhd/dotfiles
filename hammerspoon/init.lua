require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

sigils = hs.loadSpoon("WindowSigils")
sigils:start()

local function ordered_code_windows()
  local windows = sigils.window_filter:getWindows()
  table.sort(windows, function(a, b)
    local af, bf = a:frame(), b:frame()
    if af.x < bf.x then return true end
    if af.x > bf.x then return false end
    return af.y < bf.y
  end)
  return windows
end

local function window_number(n)
  return ordered_code_windows()[n+1]
end

local function send_control_w()
  hs.eventtap.keyStroke({"control"}, "W")
end

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

local function paste_as_keystrokes()
  hs.eventtap.keyStrokes(hs.pasteboard.readString())
end

local function focus_window(what)
  if type(what) == 'string' then
    local window = hs.window.focusedWindow()
    window['focusWindow' .. what](window, nil, true, true)
  else
    window_number(what):focus()
  end
end

local function toggle_full_screen()
  hs.window.focusedWindow():toggleFullScreen()
end

local function rerun_last_command()
  kitty("send-text --match=title:kak_repl_window '\x10\x0d'")
end

local function balance_space()
  hs.execute("yabai -m space --balance", true)
end

local function toggle_split_direction()
  hs.execute("yabai -m window --toggle split", true)
end

local function yabai_window_id(what)
  if type(what) == 'string' then
    return what
  end
  return tostring(window_number(what):id())
end

local function swap_window(what)
  hs.execute("yabai -m window --swap " .. yabai_window_id(what), true)
end

local function warp_window(to)
  hs.execute("yabai -m window --warp " .. yabai_window_id(to), true)
  balance_space()
end

local function ignore_notification()
  hs.execute("notification --close", true)
end

local function activate_notification()
  hs.execute("notification --activate", true)
end

local function map_all_the_things(mode, keys)
  for key, mapping in pairs(keys) do
    local action = mapping[1] or function() end
    local arg = mapping[2]
    local mods = {}
    for mod in string.gmatch(key, "([^-]+)") do
      table.insert(mods, mod)
    end
    key = mods[#mods]
    mods[#mods] = nil
    if string.match(key, "%u") then
      table.insert(mods, 'shift')
    end
    if mapping['delay_exiting_mode'] then
      mode:bind(mods, key, function()
        action(arg)
        mode:exit()
      end)
    else
      mode:bind(mods, key, function()
        mode:exit()
        action(arg)
      end)
    end
  end
  return mode
end

local window_keys = {
  "b", "c", "d", "e", "f", "g", "m", "n", "o", "p", "q", "r", "t", "u",
  "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
  [0] = "a",
}

ctrlw = map_all_the_things(hs.hotkey.modal.new('ctrl', 'w'), {
  escape = {},
  f14    = {},
  ['.']  = {send_control_w, delay_exiting_mode = true},
  F      = {toggle_full_screen},
  I      = {ignore_notification},
  N      = {activate_notification},
  v      = {paste_as_keystrokes},

  -- Focusing
  h     = {focus_window, 'West'},
  j     = {focus_window, 'South'},
  k     = {focus_window, 'North'},
  l     = {focus_window, 'East'},

  a     = {focus_window, 0},
  b     = {focus_window, 1},
  c     = {focus_window, 2},
  d     = {focus_window, 3},
  e     = {focus_window, 4},
  f     = {focus_window, 5},
  g     = {focus_window, 6},
  m     = {focus_window, 7},
  n     = {focus_window, 8},
  o     = {focus_window, 9},
  p     = {focus_window, 10},
  q     = {focus_window, 11},
  r     = {focus_window, 12},
  t     = {focus_window, 13},
  u     = {focus_window, 14},
  w     = {focus_window, 15},
  x     = {focus_window, 16},
  y     = {focus_window, 17},
  z     = {focus_window, 18},
  ['0'] = {focus_window, 19},
  ['1'] = {focus_window, 20},
  ['2'] = {focus_window, 21},
  ['3'] = {focus_window, 22},
  ['4'] = {focus_window, 23},
  ['5'] = {focus_window, 24},
  ['6'] = {focus_window, 25},
  ['7'] = {focus_window, 26},
  ['8'] = {focus_window, 27},
  ['9'] = {focus_window, 28},

  -- Swapping
  ['ctrl-h'] = {swap_window, 'west'},
  ['ctrl-j'] = {swap_window, 'south'},
  ['ctrl-k'] = {swap_window, 'north'},
  ['ctrl-l'] = {swap_window, 'east'},

  ['ctrl-a'] = {swap_window, 0},
  ['ctrl-b'] = {swap_window, 1},
  ['ctrl-c'] = {swap_window, 2},
  ['ctrl-d'] = {swap_window, 3},
  ['ctrl-e'] = {swap_window, 4},
  ['ctrl-f'] = {swap_window, 5},
  ['ctrl-g'] = {swap_window, 6},
  ['ctrl-m'] = {swap_window, 7},
  ['ctrl-n'] = {swap_window, 8},
  ['ctrl-o'] = {swap_window, 9},
  ['ctrl-p'] = {swap_window, 10},
  ['ctrl-q'] = {swap_window, 11},
  ['ctrl-r'] = {swap_window, 12},
  ['ctrl-t'] = {swap_window, 13},
  ['ctrl-u'] = {swap_window, 14},
  ['ctrl-w'] = {swap_window, 15},
  ['ctrl-x'] = {swap_window, 16},
  ['ctrl-y'] = {swap_window, 17},
  ['ctrl-z'] = {swap_window, 18},
  ['ctrl-0'] = {swap_window, 19},
  ['ctrl-1'] = {swap_window, 20},
  ['ctrl-2'] = {swap_window, 21},
  ['ctrl-3'] = {swap_window, 22},
  ['ctrl-4'] = {swap_window, 23},
  ['ctrl-5'] = {swap_window, 24},
  ['ctrl-6'] = {swap_window, 25},
  ['ctrl-7'] = {swap_window, 26},
  ['ctrl-8'] = {swap_window, 27},
  ['ctrl-9'] = {swap_window, 28},

  -- Warping
  ['alt-h'] = {warp_window, 'west'},
  ['alt-j'] = {warp_window, 'south'},
  ['alt-k'] = {warp_window, 'north'},
  ['alt-l'] = {warp_window, 'east'},

  ['alt-a'] = {warp_window, 0},
  ['alt-b'] = {warp_window, 1},
  ['alt-c'] = {warp_window, 2},
  ['alt-d'] = {warp_window, 3},
  ['alt-e'] = {warp_window, 4},
  ['alt-f'] = {warp_window, 5},
  ['alt-g'] = {warp_window, 6},
  ['alt-m'] = {warp_window, 7},
  ['alt-n'] = {warp_window, 8},
  ['alt-o'] = {warp_window, 9},
  ['alt-p'] = {warp_window, 10},
  ['alt-q'] = {warp_window, 11},
  ['alt-r'] = {warp_window, 12},
  ['alt-t'] = {warp_window, 13},
  ['alt-u'] = {warp_window, 14},
  ['alt-w'] = {warp_window, 15},
  ['alt-x'] = {warp_window, 16},
  ['alt-y'] = {warp_window, 17},
  ['alt-z'] = {warp_window, 18},
  ['alt-0'] = {warp_window, 19},
  ['alt-1'] = {warp_window, 20},
  ['alt-2'] = {warp_window, 21},
  ['alt-3'] = {warp_window, 22},
  ['alt-4'] = {warp_window, 23},
  ['alt-5'] = {warp_window, 24},
  ['alt-6'] = {warp_window, 25},
  ['alt-7'] = {warp_window, 26},
  ['alt-8'] = {warp_window, 27},
  ['alt-9'] = {warp_window, 28},

  [',']  = {rerun_last_command},
  ['=']  = {balance_space},
  ['/']  = {toggle_split_direction},
})

local WindowSigils = {}

function WindowSigils:new(...)
  local obj = {}
  self.__index = self
  return setmetatable(obj, WindowSigils):init(...)
end

function WindowSigils:init(window_filter)
  self.screens = hs.fnutils.map(hs.screen.allScreens(), function(screen)
    local bounds = screen:frame()
    local canvas = hs.canvas.new(bounds)
    canvas:show()
    return {
      screen = screen,
      canvas = canvas
    }
  end)

  self.window_filter = window_filter
  self.window_filter:subscribe({
    hs.window.filter.windowCreated,
    hs.window.filter.windowDestroyed,
    hs.window.filter.windowMoved,
    hs.window.filter.windowAllowed,
    hs.window.filter.windowRejected,
    hs.window.filter.windowNotVisible,
    hs.window.filter.windowVisible,
  }, function()
    self:refresh()
  end)

  self:refresh()
  return self
end

function WindowSigils:refresh()
  for _, screen_data in ipairs(self.screens) do
    local bounds = screen_data.screen:frame()

    local function make_frame(wframe)
      local rect = hs.geometry.toUnitRect(wframe, bounds)
      return { x = tostring(rect.x), y = tostring(rect.y), w = tostring(rect.w), h = tostring(rect.h) }
    end

    local new_elements = {}
    local windows = ordered_code_windows()
    for i, window in ipairs(windows) do
      local wframe = window:frame()
      table.insert(new_elements, {
        action = "fill",
        fillColor = { alpha = 0.3, green = 1.0, blue = 1.0 },
        frame = make_frame{x = wframe.x + 70, y = wframe.y + 1, w = 20, h = 19},
        type = "rectangle",
        withShadow = true,
      })
      table.insert(new_elements, {
        type = "text",
        text = window_keys[i-1],
        textFont = "JuliaMono Regular",
        textSize = 18,
        textLineBreak = 'trancateTail',
        frame = make_frame{x = wframe.x + 73, y = wframe.y - 3, w = 17, h = 19 + 7},
      })
    end
    if #new_elements > 0 then
      screen_data.canvas:replaceElements(new_elements)
    end
  end
end

old_sigils = WindowSigils:new(sigils.window_filter)
