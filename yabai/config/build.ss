#!/usr/bin/env gxi
(import :std/build-script)

(defbuild-script
  '("yabai"
    (static-exe: "yabai-config" "-ld-options" "-lz -lyaml")))
