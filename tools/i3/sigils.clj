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
  (let [{:keys [out err exit]} (apply shell/sh "i3-msg" args)]
    (when-not (zero? exit)
      (throw (ex-info (str "non-zero exit: " exit "\n" err))))
    out))

(defn tree []
  (json/parse-string (i3-msg "-t" "get_tree")) keyword)
(alter-var-root #'tree memoize)

(defn label? [node]
  (and (empty? (:nodes node))
       (= "normal" (:window_type node))))

(defn to-label [node]
  (concat
    (when (label? node)
      [node])
    (mapcat to-label (:nodes node))))

(defn labels []
  (into {} (map (fn [window sigil]
                  [sigil (:window window)])
                (to-label (tree))
                sigils)))

(defn i3-exec [command]
  (let [labels       (labels)
        interpolated (str/replace command
                                  #"\$\((.)\)"
                                  (comp str labels second))]
    (i3-msg interpolated)))

(defn relabel []
  (let [commands (map (fn [[sigil window-id]]
                        (format "[id=%d] title_format \"[%s] %%title\""
                                window-id
                                sigil))
                      (labels))]
    (i3-msg (str/join " ; " commands))))

(case (first *command-line-args*)
  "i3-exec" (i3-exec (second *command-line-args*))
  "relabel" (relabel))

nil
