{:user {:plugins [[cider/cider-nrepl "0.21.0"]]
        :dependencies [[net.eraserhead/select-nrepl "0.1.0-SNAPSHOT"]]
        :repl-options {:nrepl-middleware [select-nrepl.core/wrap-select]}}}
