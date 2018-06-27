(defproject biiwide/applicative "0.0.1-SNAPSHOT"

  :description "Applicative programming and data transformations"

  :url "http://github.com/biiwide/applicative"

  :license {:name "Eclipse Public License 2.0"
            :url "https://www.eclipse.org/legal/epl-2.0"}

  :dependencies [[org.clojure/clojure "1.8.0"]]

  :profiles {:dev  {:dependencies [[org.clojure/test.check "0.9.0"]]}}

  :plugins [[lein-cloverage "1.0.6"
             :exclusions [org.clojure/clojure]]
            [lein-gorilla "0.3.6"]]

;  :jar-exclusions [#"basic.*\.clj" #"^resources/public"]

  :gorilla-options {:load-scan-exclude #{".git" ".svn" "bin"}}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]

  )
