(ns gstreamer.pipeline-test
    (:require
      [clojure.test :refer :all]
      [gstreamer.pipeline :refer :all]
      [clojure.data]))

(deftest el-test
  (testing "el"
    (testing "single arg"
      (let [my-el (el :fake-plugin)]
        (is (= (:element my-el) :fake-plugin))
        (is (clojure.string/starts-with? (name (:named my-el)) (str (name (:element my-el)) "_")))
        (is (= (count (name (:named my-el)))
               (+ (count (name (:element my-el)))
                  1
                  (count (.toString (java.util.UUID/randomUUID))))))))
    (testing "named"
      (let [my-el (el :fake-plugin {:named :my-plugin})]
        (is (= (:element my-el) :fake-plugin))
        (is (= (:named my-el) :my-plugin))))
    (testing "unnamed with options has uuid name"
      (let [my-el (el :fake-plugin {})]
        (is (= (:element my-el) :fake-plugin))
        (is (uuid? (:named my-el)))))
    (testing "with random props"
      (let [named-el (el :fake-plugin {:location "fakesrc"
                                       :pattern 2
                                       :named :my-plugin})
            unnamed-el (el :fake-plugin {:volume 0.5
                                         :pan -0.3})]
        (is (= (:location named-el) "fakesrc"))
        (is (= (:pattern named-el) 2))
        (is (= (:named named-el) :my-plugin))
        (is (= (:volume unnamed-el) 0.5))
        (is (= (:pan unnamed-el) -0.3))
        (is (uuid? (:named unnamed-el)))))
    (testing "another el as an input"
      (let [el1 {:element :fakesrc :named :source}]
        (is (= (el :fakesink {:named :sink :inputs [el1]})
               {:element :fakesink
                :named :sink
                :inputs #{:source}}))))
    (testing "a pipe as an input"
      (let [el1 {:element :fakesrc :named :source}
            el2 {:element :fakefilter :named :filter}]
        (is (= (el :fakesink {:named :sink :inputs [(pipe el1 el2)]})
               {:element :fakesink
                :named :sink
                :inputs #{:filter}}))))
    (testing "an event is segregated to an event field"
      (let [el1 (el :fakesink {:named :sink :on {:error #(println %)}})]
        (is (= el1
               {:element :fakesink
                :named :sink
                :on {:error #(println %)}}))))))

(defn with-inputs [el prev-el & other-inputs]
  (assoc el :inputs
    (set (conj other-inputs (:named prev-el)))))



(deftest pipe-test
  (testing "pipe"
    (testing "results in ordered list of elements with prev elem as input"
      (let [el1 (el :fakesrc)
            el2 (el :fakefilter)
            el3 (el :fakesink)
            piped (pipe el1 el2 el3)]
        (is (= piped {:pipeline [el1
                                 (with-inputs el2 el1)
                                 (with-inputs el3 el2)]}))))
    (testing "appends prev to existing inputs"
      (let [el1 (el :fakesrc)
            el2 (el :fakefilter {:inputs [:other-src]})
            el3 (el :fakesink)
            piped (pipe el1 el2 el3)]
        (is (= piped {:pipeline [el1
                                 (with-inputs el2 el1 :other-src)
                                 (with-inputs el3 el2)]}))))))

(deftest pipe-to-map-test
  (testing "pipe-to-map"
    (testing "Pipe converts to map"
      (is (= (pipe-to-map
               (pipe {:element :fakesrc :named :my-src}
                     {:element :fakesink :named :my-sink :inputs #{:my-src}}))
             {:my-src {:element :fakesrc :named :my-src}
              :my-sink {:element :fakesink :named :my-sink :inputs #{:my-src}}})))))

(deftest connect-test
  (testing
    (let [el1 (el :fakesrc {:named :other :location "/fake"})
          el2 (el :fakesink {:inputs [:othersrc]})
          el3 (el :other-src {:location "/fake"})
          el4 (el :fakefilter)
          el5 (el :othersink)
          el6 (el :soloel)
          el7 (el :othersrc {:named :other :location "/alsofake"})
          el8 (el :other-src {:named :other :location "/fake"})
          pipe1 (pipe el1 el2)
          pipe2 (pipe el3)]
      (testing "connecting two pipes results in one large object"
        (is (= (graph pipe1 pipe2) {(:named el1)   el1
                                    (:named el2) (with-inputs el2 el1 :othersrc)
                                    (:named el3) el3})))
      (testing "connecting a pipe and an el results in one large object"
        (is (= (graph pipe1 el3)
               (graph pipe1 (pipe el3)))))
      (testing "connecting a complex pipe works"
        (let [final (graph (pipe el1 el4 el2) (pipe el3 el5) el6)
              test-final {(:named el1) el1
                          (:named el4) (with-inputs el4 el1)
                          (:named el2) (with-inputs el2 el4 :othersrc)
                          (:named el3) el3
                          (:named el5) (with-inputs el5 el3)
                          (:named el6) el6}]
          (is (= final
                 test-final))))
      (testing "connecting the same element twice overwrites props"
        (is (= (graph el8 el7)
               {:other {:element :othersrc :named :other :location "/alsofake"}})))
      (testing "connecting two elements with same name of different types overwrites the first"
        (is (= (graph el1 el7)
               {:other {:element :othersrc :named :other :location "/alsofake"}}))))))








