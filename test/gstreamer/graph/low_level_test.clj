(ns gstreamer.graph.low-level-test
  (:require [clojure.test :refer :all]
            [gstreamer.graph.low-level :refer :all]))

(deftest create-element-test
  (testing "create-element"
    (testing "adds element"
      (let [graph (create-element! (empty-graph!) :videotestsrc :test)]
        (is (= :videotestsrc (get-in @graph [:test :element])))
        (is (= (class (get-in @graph [:test :reference])) org.freedesktop.gstreamer.elements.BaseSrc))))))

(deftest connect-element-test
  (testing "connect-elements"
    (testing "connects elements"
      (let [graph (empty-graph!)
            _ (create-element! graph :videotestsrc :src)
            _ (create-element! graph :autovideosink :sink)]
            _ (connect-elements! graph :src :sink)
        (is (= #{:src} (get-in @graph [:sink :inputs])))))))
