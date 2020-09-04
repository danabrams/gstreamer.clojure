(ns gstreamer.graph.low-level-test
  (:require [clojure.test :refer :all]
            [gstreamer.graph.low-level :refer :all]))

(deftest create-element-test
  (testing "create-element!"
      (let [graph (create-element! (empty-graph!) :videotestsrc :test)]
        (is (= :videotestsrc (get-in @graph [:test :element])))
        (is (= (class (get-in @graph [:test :reference])) org.freedesktop.gstreamer.elements.BaseSrc)))))

(deftest connect-element-test
  (testing "connect-elements!"
      (let [graph (empty-graph!)
            _ (create-element! graph :videotestsrc :src)
            _ (create-element! graph :autovideosink :sink)]
            _ (connect-elements! graph :src :sink)
        (is (= #{:src} (get-in @graph [:sink :inputs]))))))

(deftest delete-element-test
  (testing "delete-element!"
    (let [graph (empty-graph!)
           _ (create-element! graph :videotestsrc :src)
           _ (create-element! graph :autovideosink :sink)
           _ (connect-elements! graph :src :sink)
           _ (delete-element! graph :src)]
      (is (= #{} (get-in @graph [:sink :inputs])))
      (is (= [:root-bin :sink] (keys @graph)))
      )))
