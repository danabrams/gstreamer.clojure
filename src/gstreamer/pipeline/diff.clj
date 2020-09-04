(ns gstreamer.pipeline.diff
  (:require [clojure.data]))

(defn prepare-inputs [elems]
  (reduce merge {} (map (fn [[name elem]] {name {:inputs (:inputs elem)}})
                        (filter #(seq (:inputs (second %))) elems))))
(defn prepare-events [elems]
  (reduce merge {} (map (fn [[name elem]]
                          [name (:on elem)])
                        (filter #(seq (:on (second %))) elems))))

(defn diff [old new]
  (let [[removals additions unchanged] (clojure.data/diff old new)
        isChange? (fn [elem]
                    (let [{:keys [named element]} ((key elem) unchanged)]
                      (and named element)))
        [props-removed removed] (split-with isChange? removals)
        [props-added added] (split-with isChange? additions)
        removed-elems (into {} (map (fn [elem]
                                      [(key elem) (dissoc (merge ((key elem) unchanged) (val elem)) :inputs :on)])
                                    removed))
        added-elems (into {} (map (fn [elem]
                                    [(key elem) (dissoc (merge ((key elem) unchanged) (val elem)) :inputs :on)])
                                  added))]
    {:connections
     {:remove (prepare-inputs removals)
      :add    (prepare-inputs additions)}
     :elements
     {:remove removed-elems
      :add    added-elems}
     :props
     {:remove (into {} (map (fn [[name props]]
                              [name (dissoc props :on)]) props-removed))
      :add    (into {} (map (fn [[name props]]
                              [name (dissoc props :on)]) props-added))}
     :on
     {:remove (prepare-events removals)
      :add    (prepare-events additions)}}))


