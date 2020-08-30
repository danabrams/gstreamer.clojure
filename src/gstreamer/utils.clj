(ns gstreamer.utils
  (:import [org.freedesktop.gstreamer
            Gst]))

(defn init [] (Gst/init))

(defn version [] (Gst/getVersion))

(defn version-string [] (Gst/getVersionString))



