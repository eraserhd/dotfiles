(import :std/misc/process
        :std/text/json)
(export yabai
        yabai-add-rule
        yabai-configure
        yabai-query)

(def (yabai . args)
  (def (->arg x)
    (if (string? x)
      x
      (with-output-to-string (cut display x))))
  (run-process ["yabai" (map ->arg args) ...]))

(def (yabai-query . args)
  (run-process ["yabai" "-m" "query" args ...]
               coprocess: read-json))

(def (yabai-configure option value space: (space #f))
  (apply yabai "-m" "config" [(if space ["--space" space] []) ... option value]))

(def (yabai-add-rule label:  (label #f)
                     app:    (app #f)
                     space:  (space #f)
                     manage: (manage no-change:)
                     grid:   (grid #f))
  (apply yabai "-m" "rule" "--add"
         [(if label [(string-append "label=" label)] []) ...
          (if app [(string-append "app=" app)] []) ...
          (if space [(string-append "space=" space)] []) ...
          (if grid [(string-append "grid=" grid)] []) ...
          (case manage
            ((no-change:) [])
            ((#t)         ["manage=on"])
            ((#f)         ["manage=off"])) ...]))

(def (yabai-add-signal event: event
                       action: action)
  (yabai "-m" "signal" "--add" (string-append "event=" event) (string-append "action=" action)))
