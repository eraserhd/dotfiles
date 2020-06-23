(import :std/amb
        :std/sort
        "yabai")
(export main)

(def workspace-display-uuid "49D1D28D-1A48-5D9A-4EB3-F347611CD9E0")

(def (window-appears-earlier? a b)
  (let ((ax (hash-ref (hash-ref a 'frame) 'x))
        (ay (hash-ref (hash-ref a 'frame) 'y))
        (bx (hash-ref (hash-ref b 'frame) 'x))
        (by (hash-ref (hash-ref b 'frame) 'y)))
    (cond
     ((< ax bx) #t)
     ((> ax bx) #f)
     (else      (< ay by)))))

(def (choose xs)
  (amb-do (map (cut lambda _ <>) xs)))

(def (ordered-workspace-windows)
  (def displays (yabai-query "--displays"))
  (def spaces   (yabai-query "--spaces"))
  (def windows  (yabai-query "--windows"))
  (begin-amb
    (def display (choose displays))
    (def space (choose spaces))
    (def window (choose windows))
    (required (equal? (hash-get display 'uuid) workspace-display-uuid))
    (required (equal? 1 (hash-get space 'visible)))
    (required (equal? (hash-get display 'index) (hash-get space 'display)))
    (required (equal? (hash-get window 'space) (hash-get space 'index)))
    (sort (amb-collect window) window-appears-earlier?)))

(def (nth-window coll index)
  (if (< -1 index (length coll))
    (list-ref coll index)
    (last coll)))

(def (main n-string)
  (let* ((n               (string->number n-string))
         (ordered-windows (ordered-workspace-windows))
         (window-n        (nth-window ordered-windows n)))
    (displayln (hash-ref window-n 'id))))
