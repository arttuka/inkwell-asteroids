(ns user
  (:require clojure.tools.namespace.repl
            inkwell.core))

(clojure.tools.namespace.repl/disable-unload!)
(clojure.tools.namespace.repl/disable-reload!)

(def sketch nil)
(def state nil)

(defn start! []
  (when-not sketch
    (require 'inkwell-asteroids.core)
    (alter-var-root #'sketch (constantly ((ns-resolve 'inkwell-asteroids.core 'make-sketch!))))
    (alter-var-root #'state (constantly (:state sketch)))))

(defn stop! []
  (when sketch
    (alter-var-root #'sketch #(.close %))
    (alter-var-root #'state (constantly nil))))

(defn restart! []
  (stop!)
  (clojure.tools.namespace.repl/refresh :after `start!))

(defn pause! []
  (when sketch
    (inkwell.core/pause! sketch)))

(defn resume! []
  (when sketch
    (inkwell.core/resume! sketch)))
