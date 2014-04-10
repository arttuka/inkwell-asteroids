(defproject inkwell-asteroids "0.1.0-SNAPSHOT"
  :description "A sample project for playing with Inkwell"
  :url "https://github.com/solita/inkwell-playground"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [inkwell "0.1.1"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.4"]]}})
