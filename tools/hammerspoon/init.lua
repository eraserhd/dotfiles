require("hs.ipc")
config_watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
