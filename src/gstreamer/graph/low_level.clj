(ns gstreamer.graph.low-level
  (:require [gstreamer.utils :as gst])
  (:import [org.freedesktop.gstreamer Bin Bus ElementFactory State Bus$MESSAGE Bus$ERROR Bus$ASYNC_DONE Bus$BUFFERING Bus$DURATION_CHANGED Bus$EOS Bus$INFO Bus$SEGMENT_DONE Bus$SEGMENT_START Bus$STATE_CHANGED Bus$TAG Bus$WARNING]))

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
         (swap! graph update-in [name2 :inputs] #(set (conj % name1)))))
     graph))

(defn disconnect-elements! [graph name1 name2]
  (let [old-graph @graph
        el1 (get-in old-graph [name1 :reference])
        el2 (get-in old-graph [name2 :reference])]
    (when (and el1 el2)
      (.unlink el1 el2)
      (swap! graph update-in [name2 :inputs] #(set (remove #{name1} %))))
    graph))

(defn set-state! [graph state]
  (let [st (cond (= state :playing) State/PLAYING
                 (= state :paused) State/PAUSED)]
    (.setState (:root-bin @graph) st)
    graph))

(defn get-pad [pad-query el]
  (let [pads (cond (= :src (first pad-query)) (.getSrcPads el)
                   (= :sink (first pad-query)) (.getSinkPads el)
                   :default (.getPads el))]
    (first (filter #(= (name (last pad-query)) (.getName (.getParentElement (.getPeer %)))) pads))))

(defn set-prop!
  ([graph elem-name prop-name value]
   (let [target (get-in @graph [elem-name :reference])]
     (when target (.set target (name prop-name) value))))
  ([graph target-name target-query prop-name value]
   (let [pad (get-pad target-query (get-in @graph [target-name :reference]))]
     (if pad (.set pad (name prop-name) value)
             (println "No pad")))
   graph))


(defn set-pad-prop! [graph elem-name pad-name prop-name value]
  (let [el (get-in @graph [elem-name :reference])
        pad (or (.getStaticPad el (name pad-name)) (.getRequestPad el (name pad-name)))]
    (.set pad (name prop-name) value)
    graph))

(defn add-signal-listener! [graph el-name msg fn]
  (let [old-graph @graph
        el (get-in old-graph [el-name :reference])
        bus (.getBus el)
        old-callback (get-in old-graph [el-name :on msg])
        msg-callback
        (case msg
          :async-done  (reify Bus$ASYNC_DONE
                         (asyncDone [_ _]
                           (fn {})))
          :buffering  (reify Bus$BUFFERING
                        (bufferingData [_ _ percent]
                          (fn {:percent percent})))
          :duration-changed  (reify Bus$DURATION_CHANGED
                               (durationChanged [_ src]
                                 (fn {:duration (.get src "duration")})))
          :eos  (reify Bus$EOS
                  (endOfStream [_ _]
                    (fn {})))
          :error  (reify Bus$ERROR
                    (errorMessage [_ _ code message]
                      (fn {:code    code
                           :message message})))
          :info  (reify Bus$INFO
                   (infoMessage [_ _ code message]
                     (fn {:code code
                          :message message})))
          :segment-done  (reify Bus$SEGMENT_DONE
                           (segmentDone [_ _ format position]
                             (fn {:format format
                                  :position position})))
          :segment-start  (reify Bus$SEGMENT_START
                            (segmentStart [_ _ format position]
                              (fn {:format format
                                   :position position})))
          :state-changed  (reify Bus$STATE_CHANGED
                            (stateChanged [_ _ old current pending]
                              (fn {:old old
                                   :current current
                                   :pending pending})))
          :tag  (reify Bus$TAG
                  (tagsFound [_ _ tags]
                    (fn {:tags tags})))
          :warning  (reify Bus$WARNING
                      (warningMessage [_ _ code message]
                        (fn {:code code
                             :message message}))))]

    (when old-callback (.disconnect bus msg-callback))
    (.connect bus msg-callback)
    (swap! graph assoc-in [el-name :on msg] msg-callback)
    graph))

(defn remove-signal-listener!
  [graph el-name msg]
  (let [old-graph @graph
        el (get-in old-graph [el-name :reference])
        bus (.getBus el)
        old-callback (get-in old-graph [el-name :on msg])]
    (when old-callback (.disconnect bus old-callback)
                       (swap! graph update-in [el-name :on] dissoc msg))
    graph))
