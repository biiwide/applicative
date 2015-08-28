(defproject biiwide/applicative "0.1.0-SNAPSHOT"

  :description "Applicative programming and data transformations"

  :url "http://github.com/biiwide/applicative"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]]

  :profiles {:dev {:dependencies [[org.clojure/test.check "0.7.0"]]}}

  :plugins [[lein-cloverage "1.0.6"]
            [lein-gorilla "0.3.4"]]

;  :jar-exclusions [#"basic.*\.clj" #"^resources/public"]

  :gorilla-options {:load-scan-exclude #{".git" ".svn" "bin"}}
)
