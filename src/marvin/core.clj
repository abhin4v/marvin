(ns marvin.core
  (:import [java.text BreakIterator]
           [java.util.regex Pattern]
           [java.io File]
           [marvin Bot])
  (:use [clojure.string :only (lower-case, join, trim)]
        [clojure.java.io :only (file, reader)]
        [clojure.contrib.string :only (split, replace-str)]
        [clojure.contrib.io :only (pwd, write-lines)])
  (:gen-class))

(defn parse-int [s] (Integer/parseInt s))

(def url-pattern
  #"(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))")


(defn tokenize-line [line]
  (let [non-char-pattern (re-pattern "[\\p{Z}\\p{C}\\p{P}]+")
        tokenize
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
  ([start
    trained-map
    startphrase-list
    endphrase-set
    min-length
    max-length]
    (loop [sentence start current-phrase start]
      (if (and
            (>= (count sentence) min-length)
            (or
              (endphrase-set (last sentence))
              (>= (count sentence) max-length)))
        (join " " sentence)
        (let [next-word (rand-nth (trained-map current-phrase))]
          (if (nil? next-word)
            (join " " sentence)
            (recur
              (conj sentence next-word)
              (conj (subvec current-phrase 1) next-word)))))))
  ([trained-map
    startphrase-list
    endphrase-set
    min-length
    max-length]
    (create-sentence
      (rand-nth startphrase-list)
      trained-map
      startphrase-list
      endphrase-set
      min-length
      max-length)))

(defn recall-memory
  [bot
   trained-map-atom
   startphrase-list-atom
   endphrase-set-atom
   line-list-atom
   key-size
   history-size]
  (let [filename (str (pwd) File/separator (.getName bot) ".mem")]
    (when (.exists (file filename))
      (with-open [rdr (reader filename)]
        (doseq [line (line-seq rdr)]
          (process-line
            line
            trained-map-atom
            startphrase-list-atom
            endphrase-set-atom
            line-list-atom
            key-size
            history-size))))))

(defn save-memory [bot line-list]
  (let [filename (str (pwd) File/separator (.getName bot) ".mem")]
    (write-lines filename line-list)))

(defn send-message [bot channel message]
  (do (println ">> Sending message:" message)
    (.sendMessage bot channel message)))

(defn send-action [bot channel action]
  (.sendAction bot channel action))

(defn change-nick [bot nick]
  (.changeNick bot nick))

(defn make-bot
  [bot-name
   on-message-callback
   on-action-callback
   on-connect-callback
   on-disconnect-callback
   on-kick-callback]
  (doto
    (proxy [Bot][]
      (onMessage [channel sender login hostname message]
        (on-message-callback this channel sender login hostname message))
      (onAction [sender login hostname target action]
        (on-action-callback this sender login hostname target action))
      (onConnect [] (on-connect-callback this))
      (onDisconnect [] (on-disconnect-callback this))
      (onKick [channel kicker-nick kicker-login kicker-hostname recipient-nick reason]
        (on-kick-callback
          this channel kicker-nick kicker-login kicker-hostname recipient-nick reason)))
    (.setBotName bot-name)))

(defn create-on-message-callback
  [message-reaction-fns default-fn msg-count]
  (fn [bot channel sender login hostname message]
    (let [message (trim message)]
      (try
        (swap! msg-count inc)
        (loop [message-reaction-fns message-reaction-fns]
          (if (empty? message-reaction-fns)
            (default-fn bot channel sender login hostname message)
            (let [[pred reaction] (first message-reaction-fns)]
              (if (pred bot channel sender login hostname message)
                (reaction bot channel sender login hostname message)
                (recur (next message-reaction-fns))))))
        (catch Exception e (.printStackTrace e))))))

(defn create-message-reaction-fns
  [trained-map-atom
   startphrase-list-atom
   endphrase-set-atom
   line-list-atom
   key-size
   history-size
   save-interval
   speak-interval
   min-sentence-length
   max-sentence-length
   msg-count]
  (let [bot-talking? (atom true)
        message-reaction-fns
          [
            ;shutup handler
            [(fn [bot _ _ _ _ message] (= message (str "shutup " (.getNick bot))))
             (fn [bot channel _ _ _ _]
                (do (println "Shutting up")
                  (reset! bot-talking? false)
                  (send-action bot channel "shuts up")
                  (change-nick bot (str (.getName bot) "|muted"))))]

            ;talk handler
            [(fn [bot _ _ _ _ message] (= message (str "talk " (.getNick bot))))
             (fn [bot channel _ _ _ _]
                (do (println "Talking")
                  (reset! bot-talking? true)
                  (send-action bot channel "can talk now")
                  (change-nick bot (.getName bot))))]

            ;speak handler
            [(fn [bot _ _ _ _ message] (= message (str "speak " (.getNick bot))))
             (fn [bot channel _ _ _ message]
                (do (println "Replying to speak command:" message)
                  (send-message bot channel
                    (create-sentence
                      @trained-map-atom
                      @startphrase-list-atom
                      @endphrase-set-atom
                      min-sentence-length
                      max-sentence-length))))]

            ;speak about handler
            [(fn [bot _ _ _ _ message]
                (not
                  (nil?
                    (re-matches
                      (re-pattern
                        (str "speak about (.*) " (Pattern/quote (.getNick bot))))
                      message))))
              (fn [bot channel _ _ _ message]
                (do (println "Replying to speak about command:" message)
                  (->>
                    message
                    (re-matches
                      (re-pattern
                        (str "speak about (.*) " (Pattern/quote (.getNick bot)))))
                    second
                    (split #"\s+")
                    first
                    lower-case
                    (fn [start]
                      (send-message bot channel
                        (create-sentence
                          [start]
                          @trained-map-atom
                          @startphrase-list-atom
                          @endphrase-set-atom
                          min-sentence-length
                          max-sentence-length))))))]]

        default-fn
          (fn [bot channel sender _ _ message]
            (do
              (doseq [line (sentencize-text message)]
                (let [urls (map first (re-seq url-pattern line))]
                  (if (not (empty? urls))
                    (doseq [url urls] (println "Url Found >" url))
                    (when-not (= 1 (count (tokenize-line line)))
                      (do
                        (println ">" sender ":" line)
                        (process-line
                          line
                          trained-map-atom
                          startphrase-list-atom
                          endphrase-set-atom
                          line-list-atom
                          key-size
                          history-size)
                        (when (and
                                @bot-talking?
                                (<= (rand) (/ 1 speak-interval)))
                          (send-message bot channel
                            (create-sentence
                              @trained-map-atom
                              @startphrase-list-atom
                              @endphrase-set-atom
                              min-sentence-length
                              max-sentence-length))))))))
              (when (zero? (mod @msg-count save-interval))
                (println "Saving memory")
                (save-memory bot @line-list-atom))))]
  [message-reaction-fns default-fn]))

(defn run-bot
  [server
   channel
   bot-name
   key-size
   history-size
   save-interval
   speak-interval
   min-sentence-length
   max-sentence-length]
  (let [key-size (if (or (< key-size 0) (> key-size 2)) 2 key-size)
        trained-map-atom (atom {})
        startphrase-list-atom (atom [])
        endphrase-set-atom (atom #{})
        line-list-atom (atom [])
        msg-count (atom 0)
        [message-reaction-fns default-fn]
          (create-message-reaction-fns
            trained-map-atom
            startphrase-list-atom
            endphrase-set-atom
            line-list-atom
            key-size
            history-size
            save-interval
            speak-interval
            min-sentence-length
            max-sentence-length
            msg-count)
        on-message-callback
          (create-on-message-callback message-reaction-fns default-fn msg-count)
        bot (make-bot bot-name
              on-message-callback
              (fn [bot sender login hostname target action]
                (on-message-callback
                  bot channel sender login hostname (str sender " " action)))
              (fn [bot] (println "Connected to" (.getServer bot)))
              (fn [bot]
                (do (println "Disconnected. Reconnecting.")
                  (doto bot
                    (.connect server)
                    (.joinChannel channel))))
              (fn [bot _ _ _ _ _ _]
                (do (println "Kicked. Rejoining.")
                  (.joinChannel bot channel))))]
    (println "Running" bot-name "on" server channel)
    (println "Configuration:")
    (println "> key size =" key-size)
    (println "> history size =" history-size)
    (println "> save interval =" save-interval)
    (println "> speak interval =" speak-interval)
    (println "> min sentence length =" min-sentence-length)
    (println "> max sentence length =" max-sentence-length)
    (doto bot
      (recall-memory
        trained-map-atom
        startphrase-list-atom
        endphrase-set-atom
        line-list-atom
        key-size
        history-size)
      (.connect server)
      (.joinChannel channel))))

(defn -main
  [& [server
      channel
      bot-name
      key-size
      history-size
      save-interval
      speak-interval
      min-sentence-length
      max-sentence-length]]
  (run-bot
    server
    channel
    bot-name
    (parse-int key-size)
    (parse-int history-size)
    (parse-int save-interval)
    (parse-int speak-interval)
    (parse-int min-sentence-length)
    (parse-int max-sentence-length)))

;;switch to pircbotx
;;pronoun substitution
