(ns nopstats.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clj-http.client :as client]
            [clojure.instant :as inst]))

(spit "output.txt" "")
(defn log-println
  "Unfortunately, jar files don't show stdout, so this logs and prints"
  [& args]
  (let [clean (apply str args)]
    (println (str clean))
    (spit "output.txt" (str clean "\n") :append true)))

(defn mean [l] (double (/ (reduce + l) (count l))))


(def http-header {"User-Agent" "JVM:NoPStatBot:v1.4.1 (by /u/ScienceMarc_alt)"}) ;I hope this user-agent is right
(try
  (def JSON (:body (client/get "https://www.reddit.com/user/SpacePaladin15/submitted/.json?limit=99&after=t3_11up3kn" {:headers http-header})))
  (def JSON2 (:body (client/get "https://www.reddit.com/user/SpacePaladin15/submitted/.json?limit=100&before=t3_11rxi6k" {:headers http-header})))
  (catch Exception _ (log-println "Reddit API failure :(\nTry again later")))

(def parsed-JSON (json/read-str JSON :key-fn keyword))
(def parsed-JSON2 (json/read-str JSON2 :key-fn keyword))

;constants
(def words-per-page "arbitrary number of words per book page" 300)
(def dune-word-count "estimated number of words in dune" 188000)

(def children
  "list of all of the metadata for user posts"
  (filterv #(="HFY" (:subreddit (:data %))) (concat ((parsed-JSON2 :data) :children) ((parsed-JSON :data) :children))))

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

(defn get-post-votes
  "returns the number of upvotes of a given post"
  [n]
  (((children n) :data) :ups))



(def all-posts
  "all post data as :title, :text pairs"
  (vec (for [n (range (dec (count children)) 0 -1)]
         (hash-map :title (get-post-title n) :text (get-post-text n) :html (get-post-html n) :ups (get-post-votes n)))))

(def nop-chapters
  "only NoP chapters"
  (filterv #(re-matches #"(?i)The Nature of Predators.*" (% :title)) all-posts))

(def chapter-lengths
  "list of chapter lengths"
  (vec (let [texts (map :text nop-chapters)]
         (for [text texts]
           (count (re-seq #"[\w’]+" (first (first (re-seq #"(?s)(\*\*)(.*)---" text)))))))))

(def chapter-perspectives
  "get the perspective of a chapter"
  (vec (for [text (map :text nop-chapters)]
         (let [pers (vec (re-find #"\*\*\*(?>(?>[a-zA-Z-]+ (?!of))*(\S+)(?>(?>(?> of|,).+\*\*\*)|\*\*\*))" text))]
           (str/replace (pers 1) #"," "")))))

(defn extract-date
  [s]
  (let [extract (first (re-seq #"((January)|(February)|(March)|(April)|(May)|(June)|(July)|(August)|(September)|(October)|(November)|(December))\s(\d{1,2}),\s(\d{4})" (str/replace s #"\*" "")))
        month ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]]
    (inst/read-instant-date (format "%d-%02d-%02d" (Integer/parseInt (extract 15)) (inc (.indexOf month (extract 1))) (Integer/parseInt (extract 14))))))

(def dates (vec (map extract-date (map :text nop-chapters))))

(def chapter-stats
  "builds a list of maps collecting each chapter's relavent information"
  (for [idx (range (count nop-chapters))]
    (assoc (nop-chapters idx) :length (chapter-lengths idx) :perspective (chapter-perspectives idx) :date (dates idx))))

(def omnibus (apply str (map :html nop-chapters)))

(def perspective-average (sort-by :avg-words (for [pers (distinct chapter-perspectives)]
                                               {:perspective pers :avg-words (math/round (mean (map #(% :length) (filter #(= pers (% :perspective)) chapter-stats))))})))

(def vote-average (sort-by :avg-votes (for [pers (distinct chapter-perspectives)]
                                        {:perspective pers :avg-votes (math/round (mean (map #(% :ups) (filter #(= pers (% :perspective)) chapter-stats))))})))

(defn -main [& args]
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

             (log-println (chapter :title) " - " (chapter :length) " words (" (chapter :ups) " upvotes)")
             (log-println (chapter :perspective))
             (log-println (.format (java.text.SimpleDateFormat. "yyyy-MMM-dd") (chapter :date)) "\n"))))

  ;print out totals
  (let [total (reduce + chapter-lengths)]
    (log-println (format "Total length: %,d words (%d pages), %.2f%% of Dune" total (int (math/round (/ total words-per-page))) (float (* 100 (/ total dune-word-count)))))
    (let [avg (/ total (count chapter-lengths))]
      (log-println (format "Average per chapter: %,d words (%.1f pages)\n" (int (math/round avg)) (float (/ avg words-per-page))))))
  (dorun
   (for [pers (frequencies chapter-perspectives)]
     (log-println (format "%s %d (%.2f%%)" (first pers) (second pers) (float (* 100 (/ (second pers) (count chapter-perspectives))))))))
  (log-println (str/replace (with-out-str (pprint/print-table perspective-average)) #"\r" ""))
  (log-println (str/replace (with-out-str (pprint/print-table vote-average)) #"\r" ""))
  (spit "omnibus.html" omnibus)
  (log-println (count nop-chapters)))