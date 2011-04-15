(defproject marvin "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [pircbot/pircbot "1.5.0"]]
  ;:aot [marvin.core]
  :dev-dependencies [[lein-javac "1.2.1-SNAPSHOT"]
                     [swank-clojure "1.2.1"]]
  :java-source-path [["src"]]
  :main marvin.core)
