#!/usr/bin/env bb

(def sigils
  ["a" "b" "c" "d" "e" "f" "g" "i" "m" "n" "o" "p" "q" "r" "s" "t" "u"
   "v" "w" "x" "y" "z"])

(defn tree []
  (json/parse-string (:out (shell/sh "i3-msg" "-t" "get_tree")) keyword))

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

(defn- relabel []
  (let [commands (map (fn [[sigil window-id]]
                        (format "[id=%d] title_format \"[%s] %%title\""
                                window-id
                                sigil))
                      (labels))]
    (shell/sh "i3-msg" (str/join " ; " commands))))

(case (first *command-line-args*)
  "relabel" (relabel))
