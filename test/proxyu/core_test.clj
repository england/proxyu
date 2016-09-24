(ns proxyu.core-test
  (:require [clojure.test :refer :all]
            [proxyu.core :refer :all]
            [clojure.data.json :as json])
  (:use clj-http.fake)
  (:import (org.jsoup Jsoup)))

(deftest proxies-test
  (testing "Doc without proxy"
    (let [doc (Jsoup/parse "<h1>Not found</h1>")]
      (is (= [] (proxies doc)))))
  (testing "Doc with single proxy"
    (let [doc (Jsoup/parse (slurp "test/proxyu/fixtures/189.219.112.32:10000.html"))
          proxies (proxies doc)]
      (is (= 1 (count proxies )))
      (let [[{ip :ip port :port country :country speed :speed connection-time :connection-time type :type anonymity :anonymity}] proxies]
        (is (= "189.219.112.32" ip))
        (is (= 10000 port))
        (is (= "Mexico" country))
        (is (= 352 speed))
        (is (= 187 connection-time))
        (is (= "socks4/5" type))
        (is (= "High +KA" anonymity))))))

(deftest next-page-url-test
  (testing "Doc without pagination"
    (let [doc (Jsoup/parse "<h1>Not found</h1>")]
      (is (= nil (next-page-url doc)))))
  (testing "Last page"
    (let [doc (Jsoup/parse (slurp "test/proxyu/fixtures/pagination_last_page.html"))]
      (is (= nil (next-page-url doc)))))
  (testing "Next page available"
    (let [doc (Jsoup/parse (slurp "test/proxyu/fixtures/pagination_middle.html"))]
      (is (= "http://proxylist.hidemyass.com/search-1292982/26#listable" (next-page-url doc))))))
