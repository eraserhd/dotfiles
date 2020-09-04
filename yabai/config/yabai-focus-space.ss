(import :std/misc/process
        :std/sugar
        "yabai")
(export main)

(def (focus-space-display space-name)
  (alet* ((space              (yabai-query "--spaces" "--space" space-name))
          (display-id         (hash-ref space 'display))
          (window             (yabai-query "--windows" "--window"))
          (focused-display-id (hash-ref window 'display))
          (unfocused?         (not (= display-id focused-display-id))))
    (yabai "-m" "display" "--focus" display-id)))

(def (focus-space space-name)
  (alet* ((window           (yabai-query "--windows" "--window"))
          (focused-space-id (hash-ref window 'space))
          (space            (yabai-query "--spaces" "--space" space-name))
          (space-id         (hash-ref space 'id))
          (unfocused?       (not (= space-id focused-space-id)))
          (space-index      (hash-ref space 'index)))
    (run-process ["skhd" "--key" (string-append "ctrl - " (number->string space-index))])))

(def (main space-name)
  (focus-space-display space-name)
  (focus-space space-name))
