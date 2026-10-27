#!/usr/bin/env bb
;; Make the windows in the focused window's container the same size.
;;
;; AeroSpace's balance-sizes balances the whole workspace.  Its tree isn't
;; exposed, so the container is found from where `sigils` says the windows are
;; and which way each window's parent lays them out.  Resizing a window takes
;; equal shares from all of its siblings, so one pass is only close: measure
;; again and repeat.

(require '[babashka.process :refer [shell]]
         '[clojure.string :as str])

(def aerospace (or (System/getenv "BALANCE_AEROSPACE") "aerospace"))
(def sigils (or (System/getenv "BALANCE_SIGILS") "sigils"))

(def tolerance 20)   ; pixels; terminals only resize in whole cells
(def overlap 8)      ; pixels; windows closer to touching than this are side by side
(def max-passes 5)
(def settle-ms 100)  ; for AeroSpace to lay windows out after a resize

(defn other [axis] (if (= axis :h) :v :h))

(defn span [[x y width height] axis]
  (if (= axis :h) [x (+ x width)] [y (+ y height)]))

(defn components
  "Groups of windows whose extents along an axis overlap, in order."
  [ids frames axis]
  (->> (sort-by #(span (frames %) axis) ids)
       (reduce (fn [groups id]
                 (let [[start stop] (span (frames id) axis)
                       last-group (dec (count groups))]
                   (if (and (seq groups) (< start (- (:end (peek groups)) overlap)))
                     (-> groups
                         (update-in [last-group :ids] conj id)
                         (update-in [last-group :end] max stop))
                     (conj groups {:ids [id] :end stop}))))
               [])
       (mapv :ids)))

(defn groupings
  "Ways of joining neighbouring groups into kids, the most kids first."
  [groups]
  (let [n (count groups)]
    (for [cuts (sort-by #(- (Long/bitCount %)) (range (bit-shift-left 1 (dec n))))]
      (->> (range 1 n)
           (reduce (fn [blocks i]
                     (if (bit-test cuts (dec i))
                       (conj blocks [(groups i)])
                       (update blocks (dec (count blocks)) conj (groups i))))
                   [[(first groups)]])
           (mapv #(vec (apply concat %)))))))

(declare build)

(defn- kid [ids frames parents axis]
  (if (= 1 (count ids))
    (when (= axis (parents (first ids))) (first ids))
    (build ids frames parents (other axis))))

(defn build
  "The tree of containers which would lay out these windows, or nil.

  A window's parent must lay out along the axis it was found along, which
  tells a grid of columns of rows from a grid of rows of columns.  Windows
  separated along the axis can still be in one kid, when that kid's own
  windows line up, so fewer kids are tried when the most doesn't work out."
  ([ids frames parents axis] (build ids frames parents axis false))
  ([ids frames parents axis wrapper?]
   (if (= 1 (count ids))
     (first ids)
     (let [groups (components ids frames axis)]
       (if (= 1 (count groups))
         ;; Either a container of one container, or no way to cut at all.
         (when-not wrapper? (build ids frames parents (other axis) true))
         (some (fn [kid-ids]
                 (when (< 1 (count kid-ids))
                   (let [kids (mapv #(kid % frames parents axis) kid-ids)]
                     (when (every? some? kids) {:axis axis :kids kids}))))
               (groupings groups)))))))

(defn container-of [tree window]
  (when (map? tree)
    (if (some #{window} (:kids tree))
      tree
      (some #(container-of % window) (:kids tree)))))

(defn windows-under [kid]
  (if (map? kid) (mapcat windows-under (:kids kid)) [kid]))

(defn representative
  "A window to resize so as to resize the kid.

  Resizing a window resizes the nearest ancestor, itself included, whose
  parent lays out along the axis, so it must be the kid itself or one of its
  own windows."
  [kid]
  (if (map? kid) (first (remove map? (:kids kid))) kid))

(defn extent [kid frames axis]
  (let [spans (map #(span (frames %) axis) (windows-under kid))]
    (- (apply max (map second spans)) (apply min (map first spans)))))

(defn find-container [frames parents focused]
  (some #(container-of (build (vec (keys parents)) frames parents %) focused) [:h :v]))

(defn aerospace! [& args]
  (:out (apply shell {:out :string} aerospace args)))

(defn read-parents
  "Which way each tiled window's parent lays out, or nil if we can't tell."
  []
  (let [windows (->> (aerospace! "list-windows" "--workspace" "focused"
                                 "--format" "%{window-id} %{window-parent-container-layout}")
                     str/split-lines
                     (map #(str/split % #" ")))]
    (when-not (some #(str/ends-with? (second %) "_accordion") windows)
      (into {} (for [[id layout] windows
                     :let [axis ({"h_tiles" :h "v_tiles" :v} layout)]
                     :when axis]
                 [(parse-long id) axis])))))

(defn read-frames []
  (into {} (for [line (str/split-lines (:out (shell {:out :string} sigils)))
                 :let [[_ id & frame] (re-find #"^\S (\d+) (-?\d+) (-?\d+) (\d+) (\d+) " line)]
                 :when (and id (not= "0" id))]
             [(parse-long id) (mapv parse-long frame)])))

(defn resize! [window axis delta]
  (aerospace! "resize" "--window-id" (str window)
              (if (= axis :h) "width" "height") (format "%+d" delta))
  (Thread/sleep settle-ms))

(defn balance
  "True if the container is balanced, or false if it can't be improved."
  [focused parents]
  (loop [attempt 0, target nil, previous nil]
    (let [frames (read-frames)]
      (when (every? frames (keys parents))
        (let [container (find-container frames parents focused)
              kids (:kids container)]
          (when (< 1 (count kids))
            (let [axis (:axis container)
                  sizes (map #(extent % frames axis) kids)
                  target (or target (/ (reduce + sizes) (count sizes)))
                  error (apply max (map #(abs (- % target)) sizes))]
              (cond
                (<= error tolerance) true
                (or (= attempt max-passes) (and previous (>= error previous))) false
                :else (do
                        (doseq [kid kids
                                :let [window (representative kid)]
                                :when window
                                :let [current (extent kid (read-frames) axis)]
                                :when (> (abs (- target current)) (/ tolerance 2))]
                          (resize! window axis (Math/round (double (- target current)))))
                        (recur (inc attempt) target error))))))))))

(defn -main []
  (let [parents (read-parents)
        focused (some-> (aerospace! "list-windows" "--focused" "--format" "%{window-id}")
                        str/trim not-empty parse-long)]
    (when (and focused (get parents focused))
      (System/exit (if (balance focused parents) 0 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
