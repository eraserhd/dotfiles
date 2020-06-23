(import :std/misc/process)
(export yabai)

(def (yabai . args)
  (def (->arg x)
    (if (string? x)
      x
      (with-output-to-string (cut display x))))
  (run-process ["yabai" (map ->arg args) ...]))

