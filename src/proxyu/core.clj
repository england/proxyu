(ns proxyu.core
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :refer [split-lines trim join blank?]])
  (:import (org.jsoup Jsoup))
  (:gen-class))

(def countries '("China"
                 "Venezuela"
                 "United States"
                 "Russian Federation"
                 "Netherlands"
                 "Brazil"
                 "Serbia"
                 "Germany"
                 "Thailand"
                 "France"
                 "Indonesia"
                 "Ukraine"
                 "Taiwan"
                 "Romania"
                 "Taiwan"
                 "Peru"
                 "Korea, Republic of"
                 "United Arab Emirates"
                 "Poland"
                 "Argentina"
                 "Hong Kong"
                 "Mongolia"
                 "Spain"
                 "India"
                 "Sweden"
                 "Italy"
                 "Bulgaria"
                 "Iran"
                 "Europe"
                 "Armenia"
                 "Algeria"
                 "Viet Nam"
                 "Cameroon"
                 "Ghana"
                 "Croatia"
                 "Iraq"
                 "Kenya"
                 "Japan"
                 "Switzerland"
                 "Mexico"
                 "Portugal"
                 "Austria"
                 "United Kingdom"
                 "Pakistan"
                 "Ecuador"
                 "Colombia"))

(def ^:const protocols-params-mapping {"HTTP"     0
                                       "HTTPS"    1
                                       "SOCKS4/5" 2})

(def ^:const anonymity-params-mapping {"NONE"    0
                                       "LOW"     1
                                       "MEDIUM"  2
                                       "HIGH"    3
                                       "HIGH+KA" 4})

(def ^:const speed-params-mapping {"SLOW"   1
                                   "MEDIUM" 2
                                   "FAST"   3})

(def ^:const connection-time-params-mapping {"SLOW"   1
                                             "MEDIUM" 2
                                             "FAST"   3})

(def default-headers
  {"User-Agent" "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:36.0) Gecko/20100101 Firefox/36.0"
   "Accept" "application/json, text/javascript, */*; q=0.01"
   "Accept-Language" "en-US,en;q=0.5"
   "Accept-Encoding" "gzip, deflate"
   "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
   "X-Requested-With" "XMLHttpRequest"
   "Referer" "http://proxylist.hidemyass.com/"})

(defn build-form-params
  [conditions]
  (let [{countries :countries
         ports :ports
         protocols :protocols
         anonymity :anonymity
         planetlab :planetlab
         speed :speed
         connection-time :connection-time}
        conditions]
    (merge
     (if (empty? countries)
       {:ac "on"}
       {"c[]" countries})
     (if (empty? ports)
       {:allPorts 1}
       {:p (join "," ports)})
     {"pr[]" (if-not (empty? protocols)
               (map protocols-params-mapping protocols)
               (vals protocols-params-mapping))
      "a[]" (if-not (empty? anonymity)
              (map anonymity-params-mapping anonymity)
              (vals anonymity-params-mapping))
      "sp[]" (if-not (empty? speed)
               (map speed-params-mapping speed)
               (vals speed-params-mapping))
      "ct[]" (if-not (empty? connection-time)
               (map connection-time-params-mapping connection-time)
               (vals connection-time-params-mapping))
      :pp 3 ;; per page 100
      :s 0
      :o 0
      :sortBy "date"})))

(comment
  (build-form-params {:countries ["China"] :ports [8080 10000] :protocols ["HTTP" "HTTPS"] :anonymity ["LOW"] :speed ["MEDIUM"]})
  )

(defn proxies
  [doc]
  (->> (.select doc "tr:has(td):not(#noresult)")
       (map (fn [tr]

              {:ip (let [[span] (.select tr "td:eq(1) > span")
                         [style] (.select span "style")
                         invisible-classes (->> (.data style)
                                                str
                                                split-lines
                                                (filter #(.contains % "{display:none}"))
                                                (map #(re-find #"[-\w]+" %)))]
                     (->> (.childNodes span)
                          (filter (fn [node]
                                    (case (.nodeName node)
                                      "#text" true
                                      ("span" "div") (and
                                                      (not= "display:none" (.attr node "style"))
                                                      (not (some #{(.className node)} invisible-classes)))
                                      false)))
                          (map (fn [node] (re-find #"\d+" (.text node))))
                          (remove blank?)
                          (join ".")))
               :port (-> (.select tr "td:eq(2)") first .text Integer/parseInt)
               :country (-> (.select tr "td:eq(3) > span") first .text)
               :speed (-> (.select tr "td:eq(4) > div") first (.attr "value") Integer/parseInt)
               :connection-time (-> (.select tr "td:eq(5) > div") first (.attr "value") Integer/parseInt)
               :type (-> (.select tr "td:eq(6)") first .text)
               :anonymity (-> (.select tr "td:eq(7)") first .text)}))))

(defn next-page-url
  [doc]
  (let [[link] (.select doc ".pagination a.next[href]")]
    (when link (.attr link "href"))))

(defmulti #^{:private true} hidemyass class)
(defmethod hidemyass clojure.lang.PersistentArrayMap
  [conditions]

  (let [form-params (build-form-params conditions)
        ;; TODO: debug mode like https://github.com/dakrone/clj-http#debugging
        ;; form-params {"c[]" ["China"] :allPorts 1 :p nil "pr[]" [0] "a[]" [1 2 3] "sp[]" [1 2 3] "ct[]" [1 2 3] :s 0 :o 0 :pp 0 :sortBy "date"}
        {body :body} (http/post "http://proxylist.hidemyass.com" {:form-params form-params :headers default-headers :debug true :debug-body true})
        {table "table" pagination "pagination" url "url"} (json/read-str body)]
    (println table)
    {:proxies (proxies (Jsoup/parse (str "<table>" table "</table>")))
     :next-page-url (when-let [next-page-url (next-page-url (Jsoup/parse pagination))]
                      (str "http://proxylist.hidemyass.com/" (when url url) next-page-url))}))

(defmethod hidemyass java.lang.String
  [url]
  (let [doc (-> (http/get url) :body Jsoup/parse)]
    {:proxies (proxies doc)
     :next-page-url (when-let [next-page-url (next-page-url doc)]
                      (str "http://proxylist.hidemyass.com/" next-page-url))}))

(comment
  (map :country
       (take 1000 (hide-seq {:countries ["China"]})))
  )

(defn hide-seq
  [conditions-or-url]
  (let [{proxies :proxies next-page-url :next-page-url} (hidemyass conditions-or-url)]
    (lazy-seq
     (chunk-cons
      (let [buf (chunk-buffer (count proxies))]
        (doseq [proxy proxies] (chunk-append buf proxy))
        (chunk buf))
      (when next-page-url (hide-seq next-page-url))))))


