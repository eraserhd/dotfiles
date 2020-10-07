require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

local spaces = {
  browse = "Color LCD",
  code   = "PHL 328E1"
}

code_window_filter = hs.window.filter.new({override={
  visible = true,
  allowScreens = spaces['code'],
}}):setDefaultFilter({
  visible = true,
  allowScreens = spaces['code'],
})


local function ordered_code_windows()
  local windows = code_window_filter:getWindows()
  table.sort(windows, function(a, b)
    local af, bf = a:frame(), b:frame()
    if af.x < bf.x then return true end
    if af.x > bf.x then return false end
    return af.y < bf.y
  end)
  return windows
end

local function window_number(n)
  local windows = ordered_code_windows()
  if n == 9 or n > #windows then
    return windows[#windows]
  else
    return windows[n+1]
  end
end

local function focus_space(space_name)
  local screen = hs.screen.find(spaces[space_name])
  if not screen then return end
  local function isOnScreen(window)
    return window:screen() == screen
  end
  local window = hs.fnutils.filter(hs.window.orderedWindows(), isOnScreen)[1] or hs.window.desktop()
  window:focus()
end

local function send_to_space(space_name)
  local screen = hs.screen.find(spaces[space_name])
  local window = hs.window.focusedWindow()
  window:centerOnScreen(screen, true, 0)
  if space_name == 'browse' then
    window:moveToUnit('[0,0,100,100]')
  end
end

local function send_control_w()
  hs.eventtap.keyStroke({"control"}, "W")
end

local function kitty(command)
  hs.execute("kitty @ --to unix:/Users/jfelice/.run/kitty " .. command, true)
end

local function focus_kitty_window(title)
  kitty("focus-window --match=title:" .. title)
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
    local mods = ''
    if string.match(key, "%u") then
      mods = 'shift'
    end
    if mapping['submode'] then
      local submode = map_all_the_things(hs.hotkey.modal.new(), mapping['submode'])
      action = function() submode:enter() end
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

ctrlw = map_all_the_things(hs.hotkey.modal.new('ctrl', 'w'), {
  escape = {},
  f14    = {},
  ['.']  = {send_control_w, delay_exiting_mode = true},
  b      = {focus_space, 'browse'},
  B      = {send_to_space, 'browse'},
  c      = {focus_space, 'code'},
  C      = {send_to_space, 'code'},
  f      = {toggle_full_screen},
  h      = {focus_window, 'West'},
  j      = {focus_window, 'South'},
  k      = {focus_window, 'North'},
  l      = {focus_window, 'East'},
  r      = {focus_kitty_window, 'kak_repl_window'},
  R      = {focus_kitty_window, 'shell_window'},
  s      = {submode = {
             escape = {},
             h      = {swap_window, 'west'},
             j      = {swap_window, 'south'},
             k      = {swap_window, 'north'},
             l      = {swap_window, 'east'},
             ['0']  = {swap_window, 0},
             ['1']  = {swap_window, 1},
             ['2']  = {swap_window, 2},
             ['3']  = {swap_window, 3},
             ['4']  = {swap_window, 4},
             ['5']  = {swap_window, 5},
             ['6']  = {swap_window, 6},
             ['7']  = {swap_window, 7},
             ['8']  = {swap_window, 8},
             ['9']  = {swap_window, 9},
           }},
  i      = {submode = {
             escape = {},
             h      = {warp_window, 'west'},
             j      = {warp_window, 'south'},
             k      = {warp_window, 'north'},
             l      = {warp_window, 'east'},
             ['0']  = {warp_window, 0},
             ['1']  = {warp_window, 1},
             ['2']  = {warp_window, 2},
             ['3']  = {warp_window, 3},
             ['4']  = {warp_window, 4},
             ['5']  = {warp_window, 5},
             ['6']  = {warp_window, 6},
             ['7']  = {warp_window, 7},
             ['8']  = {warp_window, 8},
             ['9']  = {warp_window, 9},
           }},
  I      = {ignore_notification},
  N      = {activate_notification},
  v      = {paste_as_keystrokes},
  ['0']  = {focus_window, 0},
  ['1']  = {focus_window, 1},
  ['2']  = {focus_window, 2},
  ['3']  = {focus_window, 3},
  ['4']  = {focus_window, 4},
  ['5']  = {focus_window, 5},
  ['6']  = {focus_window, 6},
  ['7']  = {focus_window, 7},
  ['8']  = {focus_window, 8},
  ['9']  = {focus_window, 9},
  [',']  = {rerun_last_command},
  ['=']  = {balance_space},
  ['/']  = {toggle_split_direction},
})

local NumberOverlay = {}

function NumberOverlay:new(...)
  local obj = {}
  self.__index = self
  return setmetatable(obj, NumberOverlay):init(...)
end

function NumberOverlay:init(screen_name, window_filter)
  self.screen = hs.screen.find(screen_name)
  local bounds = self.screen:frame()
  self.canvas = hs.canvas.new(bounds)
  self.canvas:show()

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

function NumberOverlay:refresh()
  local bounds = self.screen:frame()

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
      frame = make_frame{x = wframe.x + 5, y = wframe.y + 5, w = 30, h = 30},
      type = "rectangle",
      withShadow = true,
    })
    table.insert(new_elements, {
      type = "text",
      text = tostring(i - 1),
      frame = make_frame{x = wframe.x + 10, y = wframe.y + 5, w = 30, h = 30},
    })
  end
  if #new_elements > 0 then
    self.canvas:replaceElements(new_elements)
  end
end

overlay = NumberOverlay:new(spaces['code'], code_window_filter)
