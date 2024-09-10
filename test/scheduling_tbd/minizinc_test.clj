(ns scheduling-tbd.minizinc-test
  (:require
   [clojure.test          :refer [deftest is testing]]
   [scheduling-tbd.minizinc :as mzn]))



(def dur-data
  [#:process{:duration
           #:quantity-range{:low #:quantity{:value-string "1", :units :days}, :high #:quantity{:value-string "3", :units :months}},
           :supply-chain? true,
           :var-name "getRawMaterial",
           :id :super-process--getRawMaterial}
   #:process{:duration #:quantity{:value-string "2", :units :days}, :var-name "materialPrep", :id :super-process--materialPrep}
   #:process{:duration #:quantity{:value-string "5", :units :days}, :var-name "machining", :id :super-process--machining}
   #:process{:duration #:quantity{:value-string "3", :units :days}, :var-name "assembly", :id :super-process--assembly}
   #:process{:duration #:quantity{:value-string "1", :units :hours},
             :duration-comment "(but could go longer)",
             :var-name "qualityCtrl",
             :id :super-process--qualityCtrl}
   #:process{:duration #:quantity{:value-string "1", :units :days}, :var-name "packaging", :id :super-process--packaging}
   #:process{:duration #:box{:string-val "varies"},
             :duration-comment "Varies depending on where it is going",
             :supply-chain? true,
             :var-name "shipping",
             :id :super-process--shipping}])

(deftest selecting-best-uoms
  (testing "Testing that good units of measure are selected."
    (= :days (mzn/best-uom dur-data))))
