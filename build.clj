(ns build
  (:require
   [clojure.string :as string]
   [clojure.tools.build.api :as b]))

;;; See also https://clojure.org/guides/tools_build
;;; For a uber-jar:  clj -T:build all-uber (typically)
;;; To install jar:  clj -T:build all-jar  (for what? Not tested.)

(def lib 'rm-server/rm-server)
(def main-cls "rm_server.core")
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps-uber.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def target-dir "target")
(def uber-file (format "%s/%s-%s-standalone.jar" target-dir (name lib) version))

(defn clean [_]
  (println "Doing the clean")
  (b/delete {:path "target"}))

(defn prep [_]
  (println "Writing the pom.")
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src/lib"]})
  (b/copy-dir {:src-dirs ["src/lib"]
               :target-dir class-dir}))

(defn jar [_]
  (println "writing the jar")
  (b/jar {:class-dir class-dir :jar-file jar-file}))

;;; :basis - required, used for :mvn/local-repo
(defn install [_]
  (println "Installing: class-dir =" class-dir "version = " version)
  (let [opts {:lib lib :basis basis :jar-file jar-file :class-dir class-dir :version version}]
    (b/install opts)))

;;;=================================================================================
(defn prep-uber [_]
  (println "prep-uber: Writing the pom.")
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src/lib" "src/server" "src/app"]})
  (println "prep-uber: copying directories.")
  (b/copy-dir {:src-dirs ["src/lib" "src/server" "src/app" "resources"]
               :target-dir class-dir}))

(defn uber [_]
  (println "uber: Compiling: uber-file = " uber-file)
  (println "uber: Compiling: main = " main-cls "class-dir = " class-dir)
  (b/compile-clj {:basis basis
                  :src-dirs ["src/lib" "src/server" "src/app" "resources" #_"env/prod/resources" #_"env/prod/clj"]
                  :class-dir class-dir})
  (println "uber: Making uberjar...")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :main main-cls
           :basis basis}))


(defn all-uber [_]
  (clean nil) (prep-uber nil) (uber nil))

(defn all-jar [_]
  (clean nil) (prep nil) (jar nil) (install nil))
