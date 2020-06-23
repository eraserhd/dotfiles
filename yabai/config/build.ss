#!/usr/bin/env gxi
(import :std/build-script)

(defbuild-script
  '((static-exe: "yabai-config" "-ld-options" "-lz -lyaml")
    (static-exe: "yabai-window-number" "-ld-options" "-lz -lyaml")))
