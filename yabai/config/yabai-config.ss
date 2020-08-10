(import :std/iter
        :std/text/json
        :std/misc/process
        :std/sugar
        "yabai")
(export main)

(def yabai-events
  ["application_launched"
   "application_terminated"
   "application_front_switched"
   "application_activated"
   "application_deactivated"
   "application_visible"
   "application_hidden"
   "window_created"
   "window_destroyed"
   "window_focused"
   "window_moved"
   "window_resized"
   "window_minimized"
   "window_deminimized"
   "window_title_changed"
   "space_changed"
   "display_added"
   "display_removed"
   "display_moved"
   "display_resized"
   "display_changed"
   "mouse_down"
   "mouse_up"
   "mouse_dragged"
   "mouse_moved"
   "mission_control_enter"
   "mission_control_check_for_exit"
   "mission_control_exit"
   "dock_did_restart"
   "menu_opened"
   "menu_bar_hidden_changed"
   "dock_did_change_pref"
   "system_woke"
   "daemon_message"])

(def yabai-variables
  ["YABAI_BUTTON"
   "YABAI_DISPLAY_ID"
   "YABAI_POINT"
   "YABAI_PROCESS_ID"
   "YABAI_RECENT_DISPLAY_ID"
   "YABAI_RECENT_PROCESS_ID"
   "YABAI_RECENT_SPACE_ID"
   "YABAI_SPACE_ID"
   "YABAI_WINDOW_ID"
   "YABAI_PROCESS_ID"])

(def laptop-display "143ABEAF-FB72-322D-E98C-F6A9BBDF00CA")

(def browse-space-apps
  ["Anki"
   "Books"
   "Calendar"
   "Discord"
   "Firefox"
   "Messages"
   "Music"
   "Spotify"])

(def (remove)
  (def (remove-signal event)
    (try
      (run-process ["yabai" "-m" "signal" "--remove" (string-append "yabai-config:" event)])
      (catch _
        #f)))
  (def signals (run-process ["yabai" "-m" "signal" "--list"] coprocess: read-json))
  (for-each remove-signal yabai-events))

(def (install)
  (def (add-signal event)
    (def label-spec (string-append "label=yabai-config:" event))
    (def action-spec (string-append "action=yabai-config --signal " event))
    (run-process ["yabai" "-m" "signal" "--add" label-spec (string-append "event=" event) action-spec]))
  (remove)
  (for-each add-signal yabai-events))

(def (log-event event)
  (def message (string-append "yabai: " event ": "))
  (for* ((var yabai-variables)
         when (getenv var #f))
    (set! message (string-append message var "='" (getenv var #f) "'")))
  (run-process ["logger" message]))

(def (get-laptop-display-number)
  (alet* ((displays (yabai-query "--displays"))
          (display  (find (lambda (display)
                            (string=? (hash-get display 'uuid) laptop-display))
                          displays)))
    (hash-get display 'index)))

(def (configure-laptop-space index)
  (yabai-configure space: index "layout" "float"))

(def (configure-monitor-space index)
  (yabai-configure space: index "layout" "bsp")
  (yabai-configure space: index "bottom_padding" 55))

(def (label-space index label)
  (yabai "-m" "space" index "--label" label))

(def (update-spaces)
  (def laptop-display-number (get-laptop-display-number))
  (def code-space #f)
  (def browse-space #f)
  (def meeting-space #f)
  (for-each (lambda (space)
              (let-hash space
                (if (= .display laptop-display-number)
                  (begin
                    (when (not browse-space)
                      (set! browse-space .index))
                    (configure-laptop-space .index))
                  (begin
                    (cond
                     ((not code-space)    (set! code-space .index))
                     ((not meeting-space) (set! meeting-space .index))
                     (else                #f))
                    (configure-monitor-space .index)))))
            (yabai-query "--spaces"))
  (when code-space
    (label-space code-space "code"))
  (when browse-space
    (label-space browse-space "browse"))
  (when meeting-space
    (label-space meeting-space "meeting")))

(def (reconfigure)
  (install)
  ; global settings
  (yabai-configure "mouse_follows_focus"        "on")
  (yabai-configure "focus_follows_mouse"        "off")
  (yabai-configure "window_placement"           "second_child")
  (yabai-configure "window_topmost"             "off")
  (yabai-configure "window_opacity"             "off")
  (yabai-configure "window_opacity_duration"    "0.0")
  (yabai-configure "active_window_opacity"      "1.0")
  (yabai-configure "normal_window_opacity"      "0.90")
  (yabai-configure "split_ratio"                "0.50")
  (yabai-configure "auto_balance"               "off")
  (yabai-configure "mouse_modifier"             "fn")
  (yabai-configure "mouse_action1"              "move")
  (yabai-configure "mouse_action2"              "resize")

  (yabai-configure "window_shadow"              "off")
  (yabai-configure "window_border"              "on")
  (yabai-configure "active_window_border_color" "0xff8c6cff")

  (update-spaces)

  ; (1) Coding
  (yabai-add-rule label: "kitty" app: "kitty" space: "^code")

  ; (2) Laptop window
  (for-each (lambda (app-name)
              (yabai-add-rule label: app-name app: app-name space: "browse" grid: "1:1:0:0:1:1"))
            browse-space-apps)
  
  (yabai-add-rule label: "Firefox-Meet" app: "Firefox" title: "Meet .*" space: "focus")

  ; general space settings
  (yabai-configure "layout"         "bsp")
  (yabai-configure "top_padding"    "2")
  (yabai-configure "bottom_padding" "2")
  (yabai-configure "left_padding"   "2")
  (yabai-configure "right_padding"  "2")
  (yabai-configure "window_gap"     "2")

  ; Things not to manage
  (yabai-add-rule label: "preferences" app: "System Preferences" manage: #f))

(def (handle-signal signal-name)
  (log-event signal-name)
  (match signal-name
   ("display_added"   (update-spaces))
   ("display_removed" (update-spaces))
   (_                 #f)))

(def (usage)
  (displayln "yabai-config { --init }")
  (newline)
  (exit 1))

(def (main . args)
  (match args
   (["--init"]               (reconfigure))
   (["--signal" signal-name] (handle-signal signal-name))
   (_                        (usage))))
