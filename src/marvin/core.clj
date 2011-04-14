(ns marvin.core
  (:import [java.text BreakIterator]
           [marvin Bot])
  (:use [clojure.string :only (lower-case, join)]
        [clojure.contrib.string :only (split, replace-str)])
  (:gen-class))

(def non-char-pattern (re-pattern "[\\p{Z}\\p{C}\\p{P}]+"))

(defn log [o] (do (println o) o))

(defn tokenize-line [line]
  (let [tokenize
          (fn tokenize [text bi idx tokens]
            (lazy-seq
              (if (= (.next bi) BreakIterator/DONE)
                tokens
                (tokenize
                  text bi (.current bi)
                  (conj tokens (subs text idx (.current bi)))))))
        line (lower-case line)]
    (filter
      #(nil? (re-matches non-char-pattern %))
      (tokenize
        line (doto (BreakIterator/getWordInstance) (.setText line)) 0 []))))

(defn sentencize-text [text]
  (->>
    text
    (split #"[\.|\?|!]\s+")
    (filter (complement empty?))))

(defn create-training-set [tokens key-size]
  (reduce
    (fn [acc [k v]] (assoc acc k (vec (concat (get acc k []) v))))
    {}
    (map
      (fn [v] (vector (vec (take key-size v)) (drop key-size v)))
      (partition (inc key-size) 1 tokens))))

(defn drop-first [v value]
  (let [match-fn #(not (#{value} %))]
    (vec
      (concat
        (take-while match-fn v)
        (next (drop-while match-fn v))))))

(defn conj-non-nil [coll value]
  (if (nil? value) coll (conj coll value)))

(defn train [trained-map tokens key-size]
  (merge-with
    into
    trained-map
    (create-training-set tokens key-size)))

(defn untrain [trained-map tokens key-size]
  (into {}
    (filter
      (fn [[k v]] (not (zero? (count v))))
      (merge-with
        (fn [v [e]] (drop-first v e))
        trained-map
        (create-training-set tokens key-size)))))

(defn process-line
  [line
   trained-map-atom
   startphrase-list-atom
   endphrase-set-atom
   line-list-atom
   key-size
   history-size]
  (when (= (count @line-list-atom) history-size)
    (let [tokens (tokenize-line (first @line-list-atom))]
      (swap! line-list-atom subvec 1)
      (swap! trained-map-atom untrain tokens key-size)
      (swap! startphrase-list-atom drop-first (vec (take key-size tokens)))
      (swap! endphrase-set-atom disj (last tokens))))
  (let [tokens (tokenize-line line)]
    (swap! line-list-atom conj line)
    (swap! trained-map-atom train tokens key-size)
    (swap! startphrase-list-atom conj-non-nil (vec (take key-size tokens)))
    (swap! endphrase-set-atom conj-non-nil (last tokens))))

(defn create-sentence
  [trained-map
   startphrase-list
   endphrase-set
   min-length
   max-length]
  (let [start (rand-nth startphrase-list)]
    (loop [sentence start current-phrase start]
      (if (and
            (>= (count sentence) min-length)
            (or
              (endphrase-set (last sentence))
              (>= (count sentence) max-length)))
        (join " " sentence)
        (let [next-word (rand-nth (trained-map current-phrase))]
          (recur
            (conj sentence next-word)
            (conj (subvec current-phrase 1) next-word)))))))

(defn make-bot [name on-msg-cb]
  (doto
    (proxy [Bot][]
      (onMessage [channel sender login hostname message]
        (on-msg-cb this channel sender login hostname message)))
    (.setBotName name)))

(defn connect [bot server channel]
  (doto bot
    (.connect server)
    (.joinChannel channel)))

(defn disconnect [bot channel]
  (doto bot
    (.partChannel channel)
    (.disconnect)))

(defn send-message [bot channel message]
  (.sendMessage bot channel message))

(defn create-on-message-callback
  [trained-map-atom
   startphrase-list-atom
   endphrase-set-atom
   line-list-atom
   key-size
   history-size
   speak-interval
   min-sentence-length
   max-sentence-length]
  (let [msg-count (atom 0)]
    (fn [bot channel sender login hostname message]
      (let [create-statement-and-send
              (fn []
                (let [sentence
                        (create-sentence
                          @trained-map-atom @startphrase-list-atom
                          @endphrase-set-atom
                          min-sentence-length max-sentence-length)]
                  (println ">>> Sending message:" sentence)
                  (send-message bot channel sentence)))]
        (try
          (if (= message (str "speak " (.getName bot)))
            (create-statement-and-send)
            (doseq [line (sentencize-text message)]
              (when-not (= 1 (count (tokenize-line line)))
                (do
                  (println sender ":" line)
                  (process-line
                    line
                    trained-map-atom startphrase-list-atom
                    endphrase-set-atom line-list-atom
                    key-size history-size)
                  (swap! msg-count inc)
                  (when (zero? (mod @msg-count speak-interval))
                    (create-statement-and-send))))))
          (catch Exception e (.printStackTrace e)))))))

(defn -main [& args]
  (let [bot (make-bot "marvin"
              (create-on-message-callback
                (atom {}) (atom []) (atom #{}) (atom []) 1 500 10 6 10))]
    (connect bot "A4E.Immortal-Anime.net" "#animestan")))
;filter out links
;switch to pircbotx
;troll users
