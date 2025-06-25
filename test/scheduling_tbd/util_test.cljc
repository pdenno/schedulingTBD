(ns scheduling-tbd.util-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [scheduling-tbd.util :as util]))

(deftest remove-src-markers-test
  (testing "basic removal of src markers"
    (let [input "#+begin_src clojure\n(+ 1 2)\n#+end_src"
          expected "(+ 1 2)"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "multiple src blocks"
    (let [input "Here is some text.\n#+begin_src clojure\n(defn hello []\n  \"Hello world!\")\n#+end_src\nMore text here.\n#+begin_src python\nprint(\"Hello Python\")\n#+end_src\nFinal text."
          expected "Here is some text.\n(defn hello []\n  \"Hello world!\")\nMore text here.\nprint(\"Hello Python\")\nFinal text."]
      (is (= expected (util/remove-src-markers input)))))

  (testing "src markers with leading whitespace"
    (let [input "  #+begin_src python  \nprint('hi')\n  #+end_src  "
          expected "print('hi')"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "src markers with tabs"
    (let [input "\t#+begin_src javascript\nalert('test');\n\t#+end_src"
          expected "alert('test');"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "text without src markers"
    (let [input "Just normal text\nwith multiple lines\nno markers here"
          expected "Just normal text\nwith multiple lines\nno markers here"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "empty string"
    (is (= "" (util/remove-src-markers ""))))

  (testing "only src markers"
    (let [input "#+begin_src bash\n#+end_src"
          expected ""]
      (is (= expected (util/remove-src-markers input)))))

  (testing "partial matches in middle of lines are preserved"
    (let [input "This line has #+begin_src in the middle\n#+begin_src clojure\n(+ 1 2)\n#+end_src\nAnother line with #+end_src in middle"
          expected "This line has #+begin_src in the middle\n(+ 1 2)\nAnother line with #+end_src in middle"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "case sensitivity - uppercase markers are preserved"
    (let [input "#+BEGIN_SRC clojure\n(+ 1 2)\n#+END_SRC"
          expected "#+BEGIN_SRC clojure\n(+ 1 2)\n#+END_SRC"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "different language tokens"
    (let [input "#+begin_src javascript\nalert('hello');\n#+end_src\n#+begin_src shell\necho 'test'\n#+end_src\n#+begin_src sql\nSELECT * FROM table;\n#+end_src"
          expected "alert('hello');\necho 'test'\nSELECT * FROM table;"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "nested content with multiple lines"
    (let [input "Some introduction\n#+begin_src clojure\n(defn complex-fn [x y]\n  (let [result (+ x y)]\n    (if (> result 10)\n      \"big\"\n      \"small\")))\n#+end_src\nSome conclusion"
          expected "Some introduction\n(defn complex-fn [x y]\n  (let [result (+ x y)]\n    (if (> result 10)\n      \"big\"\n      \"small\")))\nSome conclusion"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "markers with various whitespace patterns"
    (let [input "   #+begin_src  clojure   \n(code here)\n\t\t#+end_src\t\t"
          expected "(code here)"]
      (is (= expected (util/remove-src-markers input)))))

  (testing "mixed content with empty lines"
    (let [input "Text before\n\n#+begin_src bash\necho \"hello\"\necho \"world\"\n#+end_src\n\nText after"
          expected "Text before\n\necho \"hello\"\necho \"world\"\n\nText after"]
      (is (= expected (util/remove-src-markers input))))))
