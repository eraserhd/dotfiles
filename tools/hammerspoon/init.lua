require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

-- Start kitty if it is not open
if hs.application.find('net.kovidgoyal.kitty') == nil then
    hs.application.open('/run/current-system/Applications/kitty.app')
end
