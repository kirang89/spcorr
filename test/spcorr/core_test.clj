(ns spcorr.core-test
  (:require [clojure.test :refer :all]
            [spcorr.core :refer :all]))

(def test-cases {"access" "acces"
                 "accessible" "accessble"
                 "account" "accont"
                 "forbidden" "forbiden"
                 "decisions" "deciscions"
                 "supposedly" "supposidly"})

(deftest test-spell-corrector
  (doseq [[k v] test-cases]
    (is (= k (spcorr.core/correct v)))))
