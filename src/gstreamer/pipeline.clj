(ns gstreamer.pipeline
  (:import (java.util UUID)))

(defn el
  ([elem-name]
   {:element elem-name
    :named   (keyword (str (name elem-name) "_" (.toString (UUID/randomUUID))))})
  ([elem-name options]
   (let [named (or (:named options) (UUID/randomUUID))
         inputs (:inputs options)
         cleaned-inputs (map (fn [input]
                               (cond (:element input) (:named input)
                                     (seq (:pipeline input)) (:named (last (:pipeline input)))
                                     :default input))
                             inputs)
         element (assoc options :element elem-name :named named)]
     (if (empty? cleaned-inputs)
       element
       (assoc element
         :inputs (set cleaned-inputs))))))


(defn pipe
  [& els]
  (let [elements
        (->> els
             (reduce
               (fn [acc {:keys [named element inputs] :as elem}]
                 (let [prev-name (:named (last acc))]
                   (if prev-name
                     (conj acc
                           (assoc elem
                             :inputs (set (conj inputs prev-name))))

                     (conj acc elem)))) []))]
    {:pipeline elements}))

(defn pipe-to-map [pipe]
  (reduce (fn [acc elem]
            (assoc acc (:named elem) elem))
          {} (:pipeline pipe)))

(defn graph
  [& pipes]
  (let [pipe-or-el-to-map
        (fn [pipe-or-el]
          (if (:element pipe-or-el)
            {(:named pipe-or-el) pipe-or-el}
            (pipe-to-map pipe-or-el)))
        all-pipes (map pipe-or-el-to-map pipes)]

    (reduce (fn [acc pipe]
              (merge-with into acc pipe))
            {} all-pipes)))