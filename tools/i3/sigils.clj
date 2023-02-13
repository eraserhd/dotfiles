#!/usr/bin/env bb

(def sigils
  ["a" "b" "c" "d" "e" "f" "g" "i" "m" "n" "o" "p" "q" "r" "s" "t" "u"
   "v" "w" "x" "y" "z"])
(def directions
  {"h" "left"
   "j" "down"
   "k" "up"
   "l" "right"})

(defn i3-msg [& args]
  (let [{:keys [out exit], :as result} (apply shell/sh "i3-msg" args)]
    (when-not (zero? exit)
      (throw (ex-info (str "non-zero exit: " exit "\n") result)))
    out))

(defn container-tree []
  (json/parse-string (i3-msg "-t" "get_tree") keyword))
(alter-var-root #'container-tree memoize)

(defn sigil->window-id
  "Returns a map of sigil symbol names to window ids"
  []
  (let [label?   (fn label?* [node]
                   (and (empty? (:nodes node))
                        (= "normal" (:window_type node))))
        to-label (fn to-label* [node]
                   (concat
                     (when (label? node)
                       [node])
                     (mapcat to-label* (:nodes node))))]
    (into {} (map (fn [window sigil]
                    [sigil (:window window)])
                  (to-label (container-tree))
                  sigils))))

(defn interpret-dollar-bracket [text]
  (let [sigil->window-id (sigil->window-id)]
    (str/replace text #"\$\((.)\)" (comp str sigil->window-id second))))

;; ======================================================================

(defmulti command
  (fn [cmd & args]
    (keyword cmd)))

(defmethod command :i3-exec
  [_ msg-text]
  (spit "/tmp/foo.txt" msg-text)
  (i3-msg (interpret-dollar-bracket msg-text)))

(defmethod command :relabel
  [_]
  (let [commands (map (fn [[sigil window-id]]
                        (format "[id=%d] title_format \"[%s] %%title\""
                                window-id
                                sigil))
                      (sigil->window-id))]
    (i3-msg (str/join " ; " commands))))

(apply command *command-line-args*)

nil
