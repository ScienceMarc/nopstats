(ns nopstats.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clj-http.client :as client]))

(spit "output.txt" "")
(defn log-println
  "Unfortunately, jar files don't show stdout, so this logs and prints"
  [& args]
  (let [clean (str/replace (str/replace (str args) #"^.|.$|\"" "") #"\\n" "\n")] ;Why was it so hard to remove quotes and parentheses?
    (println (str clean))
    (spit "output.txt" (str clean "\n") :append true)))
(defn mean [l] (double (/ (reduce + l) (count l))))


(def http-header {"User-Agent" "JVM:NoPStatBot:v1.2.0 (by /u/ScienceMarc_alt)"}) ;I hope this user-agent is right
(try
  (def JSON (:body (client/get "https://www.reddit.com/user/SpacePaladin15/submitted/.json?limit=200" {:headers http-header})))
  (def success-type "reddit")
  (catch Exception _ (log-println "Reddit API is being fussy, please try again in a little bit, trying to load old data")
         (try
           (def JSON (slurp "nop.json"))
           (log-println "Found old data!")
           (def success-type "cache")
           (catch Exception _ (log-println "No old data found, you'll just have to wait and try again")))))

(def parsed-JSON (json/read-str JSON :key-fn keyword))

;constants
(def words-per-page "arbitrary number of words per book page" 300)
(def dune-word-count "estimated number of words in dune" 188000)

(def children
  "list of all of the metadata for user posts"
  ((parsed-JSON :data) :children))

(defn get-post-text
  "returns the selftext of a given post"
  [n]
  (((children n) :data) :selftext))

(defn get-post-html
  "returns the selftext in HTML of a given post"
  [n]
  ;TODO: Clean this up properly
  (str/replace (str/replace (str/replace (str/replace (str/replace (((children n) :data) :selftext_html) #"&gt;" ">") #"&lt;" "<") #"quot;" "“") #"#39;" "’") #"&amp;" ""))

(defn get-post-title
  "returns the post title (adding a 1 if the post doesn't end in a number)"
  [n]
  (let [title (((children n) :data) :title)]
    (if (re-matches #"\d" (str (last (seq title))))
      title
      (str title " 1"))))


(def all-posts
  "all post data as :title, :text pairs"
  (vec (for [n (range 99 0 -1)]
         (hash-map :title (get-post-title n) :text (get-post-text n) :html (get-post-html n)))))

(def nop-chapters
  "only NoP chapters"
  (filterv #(re-matches #"(?i)The Nature of Predators.*" (% :title)) all-posts))

(def chapter-lengths
  "list of chapter lengths"
  (vec (let [texts (map :text nop-chapters)] 
         (for [text texts] 
           (count (re-seq #"[\w|’]+" text))))))

(def chapter-perspectives
  "get the perspective of a chapter"
  (vec (for [text (map :text nop-chapters)]
         (let [pers (vec (re-find #"\*\*\*((.+)(,| of)|(.+)).+\*\*" text))]
           (last (str/split (or (pers 4) (pers 2)) #" "))))))

(def chapter-stats
  "builds a list of maps collecting each chapter's relavent information"
  (for [idx (range (count nop-chapters))]
    (assoc (nop-chapters idx) :length (chapter-lengths idx) :perspective (chapter-perspectives idx)))) ;TODO: Add chapter dates

(def omnibus (apply str (map #(:html %) nop-chapters)))

(defn -main [& args]
  (spit "nop.json" JSON)
  (let [f (fn [lst] (str/replace (str/replace (str lst) #" " ",") #"[\[|\]]" ""))]
    (spit "totals.csv" (f chapter-lengths))
    (spit "perspectives.csv" (f chapter-perspectives)))
  ;save chapters sorted by perspective
  (dorun (for [chapter chapter-stats]
           (let [path (str "chapters/" (chapter :perspective) "/" (chapter :title))]
             (io/make-parents path)
             (case (some #{"-md" "-html"} args)
               "-md" (spit (str path ".md") (chapter :text))
               "-html" (spit (str path ".html") (chapter :html))
               (dorun
                (spit (str path ".md") (chapter :text))
                (spit (str path ".html") (chapter :html))))


             (log-println (chapter :title) "-" (chapter :length) "words")
             (log-println (chapter :perspective) "\n"))))

  ;print out totals
  (let [total (reduce + chapter-lengths)]
    (log-println (format "Total length: %,d words (%d pages), %.2f%% of Dune" total (int (math/round (/ total words-per-page))) (float (* 100 (/ total dune-word-count)))))
    (let [avg (/ total (count chapter-lengths))]
      (log-println (format "Average per chapter: %,d words (%.1f pages)\n" (int (math/round avg)) (float (/ avg words-per-page))))))
  (dorun
   (for [pers (frequencies chapter-perspectives)]
     (log-println (format "%s %d (%.2f%%)" (first pers) (second pers) (float (* 100 (/ (second pers) (count chapter-perspectives))))))))
  ;TODO: Make this show up in the log
  (pprint/print-table (sort-by :avg-words (for [pers (distinct chapter-perspectives)]
                                            {:perspective pers :avg-words (math/round (mean (map #(% :length) (filter #(= pers (% :perspective)) chapter-stats))))})))
  (spit "omnibus.html" omnibus)
  (log-println (count nop-chapters))
  (log-println (case success-type
                 "reddit" "✅ - Data up to date"
                 "cache" "⛔ - Out of date")))