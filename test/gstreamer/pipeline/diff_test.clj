(ns gstreamer.pipeline.diff-test
  (:require [clojure.test :refer :all]
            [gstreamer.pipeline.diff :refer :all]
            [gstreamer.pipeline :refer :all]))

(deftest connection-diff-test
  (testing "diff"
    (testing "Identical connection graphs creates two empty sets"
      (is (= (:connections (diff
                             {}
                             {}))
             {:remove {}
              :add {}})))
    (testing "No connections to some connections returns a map of adds"
      (is (= (:connections (diff {}
                                 {:mysrc    {:element :fakesrc :named :mysrc}
                                  :myfilter {:element :fakefilter :named :myfilter
                                             :inputs  #{:mysrc}}
                                  :mysink   {:element :fakesink :named :mysink
                                             :inputs  #{:myfilter}}}))
             {:remove {}
              :add    {:myfilter {:inputs #{:mysrc}}
                       :mysink {:inputs #{:myfilter}}}})))
    (testing "Some Connections to no connects returns a map of removes"
      (is (= (:connections (diff
                             {:mysrc    {:element :fakesrc :named :mysrc}
                              :myfilter {:element :fakefilter :named :myfilter
                                         :inputs  #{:mysrc}}
                              :mysink   {:element :fakesink :named :mysink
                                         :inputs  #{:myfilter}}}
                             {}))
             {:add    {}
              :remove {:myfilter {:inputs #{:mysrc}}
                       :mysink {:inputs #{:myfilter}}}})))
    (testing "Changing connections returns an accurate map of changes"
      (is (= (:connections (diff
                             {:mysrc    {:element :fakesrc :named :mysrc}
                              :myfilter {:element :fakefilter :named :myfilter
                                         :inputs  #{:mysrc :some-src :byway-src}}
                              :mysink   {:element :fakesink :named :mysink
                                         :inputs  #{:myfilter}}}
                             {:mysrc    {:element :fakesrc :named :mysrc}
                              :myfilter {:element :fakefilter :named :myfilter
                                         :inputs  #{:mysrc :other-src}}
                              :mysink   {:element :fakesink :named :mysink
                                         :inputs  #{:myfilter}}}))
             {:add    {:myfilter {:inputs #{:other-src}}}
              :remove {:myfilter {:inputs #{:some-src :byway-src}}}})))))

(deftest element-diff-test
  (testing "diff"
    (testing "Adding an element results in the creation of an element"
      (is (= (:elements
               (diff {}
                     {:myplaybin {:element :playbin :named :myplaybin}}))
             {:add {:myplaybin {:element :playbin :named :myplaybin}}
              :remove {}})))
    (testing "Removing an element results in the removal of an element"
      (is (= (:elements
               (diff {:myplaybin {:element :playbin :named :myplaybin}}
                     {})
               {:remove {:myplaybin {:element :playbin :named :myplaybin}}
                :add {}}))))
    (testing "Inputs are excluded"
      (is (= (:elements
               (diff {}
                     {:mysink {:element :fakesink :named :mysink :inputs #{:mysrc}}}))
             {:add {:mysink {:element :fakesink :named :mysink}}
              :remove {}})))
    (testing "Complex changes work"
      (is (= (:elements
               (diff {:mysrc {:element :fakesrc :named :myplaybin}
                      :myfilter {:element :fakefilter :named :myfilter}
                      :mysink {:element :fakesink :named :mysink}}
                     {:mysrc {:element :fakesrc :named :myplaybin}
                      :otherfilter {:element :difffilter :named :otherfilter}
                      :somefilter {:element :someotherfilter :named :somefilter}
                      :mysink {:element :fakesink :named :mysink}}))
             {:add {:otherfilter {:element :difffilter :named :otherfilter}
                    :somefilter {:element :someotherfilter :named :somefilter}}
              :remove {:myfilter {:element :fakefilter :named :myfilter}}})))
    (testing "Element changes appear as changes, not add and remove"
      (is (= (diff {:mysink {:volume  0.5
                             :element :fakesink
                             :inputs  #{:fakesrc}
                             :named   :mysink}}
                   {:mysink {:volume  1.0
                             :element :fakesink
                             :named   :mysink
                             :inputs  #{:fakesrc}}})
             {:elements {:add    {}
                         :remove {}}
              :props    {:remove {:mysink {:volume 0.5}}
                         :add {:mysink {:volume 1.0}}}
              :connections   {:add {}
                              :remove {}}
              :on {:add {} :remove {}}}))
      (testing "Element which changes element type should be removed and added"
        (is (= (:elements (diff {:mysrc {:location "/fake"
                                         :element  :fakesrc
                                         :named    :mysrc}}
                                {:mysrc {:location "/fake"
                                         :element  :othersrc
                                         :named    :mysrc}}))
               {:add    {:mysrc {:location "/fake"
                                 :element  :othersrc
                                 :named    :mysrc}}
                :remove {:mysrc {:location "/fake"
                                 :element  :fakesrc
                                 :named    :mysrc}}}))))
    (testing "Element which changes name should be removed and added"
      (is (= (:elements (diff {:mysink {:location "/fake"
                                        :element  :fakesrc
                                        :named    :mysink}}
                              {:mysrc {:location "/fake"
                                       :element  :fakesrc
                                       :named    :mysrc}}))
             {:add    {:mysrc {:location "/fake"
                               :element  :fakesrc
                               :named    :mysrc}}
              :remove {:mysink {:location "/fake"
                                :element  :fakesrc
                                :named    :mysink}}})))))

(deftest events-test
  (testing "diff"
    (let [el1 {:sink {:element :fakesink
                      :named   :sink
                      :on      {:error 1}}}]
      (testing "Adding element with event adds event handler"
        (is (= (:on (diff {} el1))
               {:add    {:sink {:error 1}}
                :remove {}})))
      (testing "Removing an element with event removes event handler"
        (is (= (:on (diff el1 {}))
               {:add    {}
                :remove {:sink {:error 1}}})))
      (testing "Removing an event handler removes event handler"
        (is (= (:on (diff {:sink {:element :fakesink
                                  :named   :sink
                                  :on      {:error 1}}}
                           {:sink {:element :fakesink
                                   :named :sink}}))
               {:add    {}
                :remove {:sink {:error 1}}}))))))