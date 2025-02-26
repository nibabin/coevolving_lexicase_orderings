(defproject net.clojars.lspector/propeller "0.3.0"
  :description "Yet another Push-based genetic programming system in Clojure."
  :url "https://github.com/lspector/propeller"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/clojurescript "1.11.57"]
                 [org.clojure/test.check "1.1.1"]
                 [net.clojars.schneau/psb2 "1.1.0"]]
  :profiles {:profiling {:dependencies [[com.clojure-goes-fast/clj-async-profiler "0.5.1"]]}}
  :main ^:skip-aot propeller.core
  :repl-options {:init-ns propeller.core}
  :jvm-opts ^:replace [])
