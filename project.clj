(defproject proxyu "0.1.0-SNAPSHOT"
  :description "hidemyass proxy parser"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "1.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.jsoup/jsoup "1.8.3"]]
  :profiles {:dev {:dependencies [[clj-http-fake "1.0.1"]]}}
  :main proxyu.core)
