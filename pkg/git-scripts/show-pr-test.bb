(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests]])

(load-file (str (fs/path (fs/parent (fs/absolutize (System/getProperty "babashka.file"))) "show-pr")))
(alias 'sp 'show-pr)

(defn- lines [& ls] (str/join "\n" ls))

(def diff
  (lines "diff --git a/lib/a.ex b/lib/a.ex"
         "index 111..222 100644"
         "--- a/lib/a.ex"
         "+++ b/lib/a.ex"
         "@@ -1,4 +1,5 @@"
         " defmodule A do"
         "-  def f, do: 1"
         "+  def f, do: 2"
         "+  def g, do: 3"
         "   def h, do: 4"
         " end"
         "diff --git a/README.md b/README.md"
         "new file mode 100644"
         "--- /dev/null"
         "+++ b/README.md"
         "@@ -0,0 +1,2 @@"
         "+# A"
         "+text"))

(defn- c
  ([id author body] (c id author body nil))
  ([id author body reply-to]
   {:id id :author author :at (str "2026-11-0" id "T12:00:00Z") :body body :reply-to reply-to}))

(defn- t [id path side line & comments]
  {:id id :resolved? false :outdated? false :path path :line line :side side
   :comments (vec comments)})

(defn- render-with [more & threads]
  (str/split-lines
   (sp/render (merge {:url "https://git.example/o/r/pull/7"
                      :view "title:\tFix it\nstate:\tOPEN\n\n- a bullet\n"
                      :comments ""
                      :diff diff
                      :threads threads}
                     more))))

(defn- render [& threads]
  (apply render-with {} threads))

(defn- after [rendered line]
  (rest (drop-while #(not= line %) rendered)))

(deftest threads-follow-the-added-line-they-are-on
  (is (= ["+  def f, do: 2"
          "#thread id=T1 status=open side=RIGHT line=2 path=lib/a.ex"
          "#comment id=1 author=ann at=2026-11-01T12:00:00Z"
          "# Why 2?"
          "+  def g, do: 3"]
         (take 5 (drop-while #(not= "+  def f, do: 2" %)
                             (render (t "T1" "lib/a.ex" "RIGHT" 2 (c 1 "ann" "Why 2?"))))))))

(deftest threads-follow-the-deleted-line-they-are-on
  (is (= ["-  def f, do: 1"
          "#thread id=T1 status=open side=LEFT line=2 path=lib/a.ex"
          "#comment id=1 author=ann at=2026-11-01T12:00:00Z"
          "# Why remove it?"
          "+  def f, do: 2"]
         (take 5 (drop-while #(not= "-  def f, do: 1" %)
                             (render (t "T1" "lib/a.ex" "LEFT" 2 (c 1 "ann" "Why remove it?"))))))))

(deftest threads-follow-the-context-line-they-are-on
  (is (= [" end" "#thread id=T1 status=open side=RIGHT line=5 path=lib/a.ex"]
         (take 2 (after (render (t "T1" "lib/a.ex" "RIGHT" 5 (c 1 "ann" "x")))
                        "   def h, do: 4")))))

(deftest threads-in-a-new-file-follow-its-lines
  (is (= ["+# A" "#thread id=T1 status=open side=RIGHT line=1 path=README.md"]
         (take 2 (after (render (t "T1" "README.md" "RIGHT" 1 (c 1 "ann" "x")))
                        "@@ -0,0 +1,2 @@")))))

(deftest threads-on-one-line-are-in-the-order-they-were-started
  (let [out (render (t "LATER" "lib/a.ex" "RIGHT" 2 (c 5 "bob" "second"))
                    (t "EARLIER" "lib/a.ex" "RIGHT" 2 (c 3 "ann" "first")))]
    (is (= ["#thread id=EARLIER status=open side=RIGHT line=2 path=lib/a.ex"]
           (filter #(str/starts-with? % "#thread id=EARLIER") out)))
    (is (< (.indexOf out "#thread id=EARLIER status=open side=RIGHT line=2 path=lib/a.ex")
           (.indexOf out "#thread id=LATER status=open side=RIGHT line=2 path=lib/a.ex")))))

(deftest resolved-threads-say-who-resolved-them
  (is (some #{"#thread id=T1 status=resolved resolved_by=jfelice side=RIGHT line=2 path=lib/a.ex"}
            (render (assoc (t "T1" "lib/a.ex" "RIGHT" 2 (c 1 "ann" "x"))
                           :resolved? true :resolved-by "jfelice")))))

(deftest replies-name-the-comment-they-answer
  (is (some #{"#comment id=2 author=bob at=2026-11-02T12:00:00Z reply_to=1"}
            (render (t "T1" "lib/a.ex" "RIGHT" 2 (c 1 "ann" "q") (c 2 "bob" "a" 1))))))

(deftest outdated-threads-are-unplaced-and-say-where-they-were
  (let [out (render (assoc (t "T1" "lib/old.ex" "RIGHT" nil (c 1 "ann" "gone"))
                           :outdated? true :original-line 33))]
    (is (= ["#section unplaced"
            "#thread id=T1 status=open outdated=yes side=RIGHT original_line=33 path=lib/old.ex"
            "#comment id=1 author=ann at=2026-11-01T12:00:00Z"
            "# gone"]
           (take-last 4 out)))
    (is (not-any? #(str/starts-with? % "#thread") (take-while #(not= "#section unplaced" %) out)))))

(deftest threads-whose-line-is-not-in-the-diff-are-unplaced
  (is (some #{"#section unplaced"}
            (render (t "T1" "lib/a.ex" "RIGHT" 99 (c 1 "ann" "x"))))))

(deftest no-unplaced-section-when-all-threads-are-placed
  (is (not-any? #{"#section unplaced"}
                (render (t "T1" "lib/a.ex" "RIGHT" 2 (c 1 "ann" "x"))))))

(deftest comment-bodies-cannot-be-taken-for-markers
  (let [out (render (t "T1" "lib/a.ex" "RIGHT" 2
                       (c 1 "ann" "#thread id=FAKE status=open\n#comment id=9\n> reply")))]
    (is (some #{"# #thread id=FAKE status=open"} out))
    (is (some #{"# #comment id=9"} out))
    (is (some #{"# > reply"} out))
    (is (not-any? #(str/starts-with? % "#thread id=FAKE") out))))

(deftest blank-body-lines-are-a-bare-hash
  (is (= ["# one" "#" "# two"]
         (take 3 (after (render (t "T1" "lib/a.ex" "RIGHT" 2 (c 1 "ann" "one\r\n\r\ntwo\r\n\r\n")))
                        "#comment id=1 author=ann at=2026-11-01T12:00:00Z")))))

(deftest the-view-and-diffstat-are-indented-so-diff-colors-miss-them
  (let [out (render)
        view (take-while #(not= "#section diffstat" %) (after out "#section view"))
        stat (take-while #(not= "#section diff" %) (after out "#section diffstat"))]
    (is (= ["    title:\tFix it" "    state:\tOPEN" "" "    - a bullet"] view))
    (is (every? #(or (= "" %) (str/starts-with? % "    ")) (concat view stat)))))

(deftest the-diffstat-counts-added-and-deleted-lines-per-file
  (is (= ["    lib/a.ex  | 3 ++-"
          "    README.md | 2 ++"
          "    2 files changed, 4 insertions(+), 1 deletion(-)"]
         (take-while #(not= "#section diff" %) (after (render) "#section diffstat")))))

(deftest binary-files-show-as-bin-in-the-diffstat
  (is (= ["pic.png | Bin" "1 file changed"]
         (sp/render-diffstat
          (sp/diffstat-files
           (sp/classify-diff ["diff --git a/pic.png b/pic.png"
                              "Binary files a/pic.png and b/pic.png differ"]))))))

(deftest renamed-files-show-both-names-in-the-diffstat
  (is (= ["a.ex => b.ex | 1 +" "1 file changed, 1 insertion(+)"]
         (map #(str/replace % #"  +" " ")
              (sp/render-diffstat
               (sp/diffstat-files
                (sp/classify-diff ["diff --git a/a.ex b/b.ex"
                                   "similarity index 90%"
                                   "rename from a.ex"
                                   "rename to b.ex"
                                   "--- a/a.ex"
                                   "+++ b/b.ex"
                                   "@@ -1,1 +1,2 @@"
                                   " x"
                                   "+y"])))))))

(deftest diff-lines-that-look-like-file-headers-are-still-diff-lines
  (let [entries (sp/classify-diff ["diff --git a/f b/f" "--- a/f" "+++ b/f" "@@ -1,2 +1,2 @@"
                                   "--- a/not-a-header" "+++ b/not-a-header"])]
    (is (= [:del :add] (map :kind (take-last 2 entries))))))

(deftest comments-have-their-own-section-between-the-view-and-the-diffstat
  (let [out (render-with {:comments "author:\tann\n--\nLooks good\n"})]
    (is (= ["#section view" "#section comments" "#section diffstat" "#section diff"]
           (filter #(str/starts-with? % "#section") out)))
    (is (= ["    author:\tann" "    --" "    Looks good"]
           (take-while #(not= "#section diffstat" %) (after out "#section comments"))))))

(deftest the-first-line-says-what-was-formatted
  (is (= "#show-pr 1 https://git.example/o/r/pull/7" (first (render)))))

(deftest sections-come-in-the-order-view-diffstat-diff
  (is (= ["#section view" "#section diffstat" "#section diff"]
         (filter #(str/starts-with? % "#section") (render)))))

(deftest the-kakoune-config-highlights-markers-and-resolved-threads
  (is (str/includes? sp/kakoune-config "add-highlighter shared/diff/show-pr-markers regex"))
  (is (str/includes? sp/kakoune-config "status=resolved"))
  (is (str/ends-with? sp/kakoune-config "\n")))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
