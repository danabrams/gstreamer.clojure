(ns gstreamer.graph.low-level
  (:require [gstreamer.utils :as gst])
  (:import [org.freedesktop.gstreamer Bin ElementFactory]))

(gst/init)

(defn empty-graph! [] (atom {:root-bin (Bin. "main")}))

(defn create-element! [graph element named]
  (let [factory (ElementFactory/find (name element))
        el (when factory (.create factory (name named)))
        add-element (fn [old-graph elem-object]
                      (.add (:root-bin old-graph) elem-object)
                      (assoc old-graph named {:reference elem-object :element element :named named}))]
    (when el (swap! graph add-element el)))
  graph)

(defn delete-element! [graph el-name]
  (let [remove-as-input (fn [old-graph]
                          (->> (dissoc old-graph el-name)
                               (map (fn [[k elem]]
                                      (if (seq (:inputs elem))
                                        [k (assoc elem :inputs
                                                       (set (remove #{el-name} (:inputs elem))))]
                                        [k elem])))
                               (into {})))]

   (swap! graph remove-as-input))
  graph)

(defn connect-elements! [graph name1 name2]
  (let [old-graph @graph
        el1 (get-in old-graph [name1 :reference])
        el2 (get-in old-graph [name2 :reference])]
     (when (and el1 el2)
       (when (.link el1 el2)
         (swap! graph update-in [name2 :inputs] #(set (conj % name1)))))))


