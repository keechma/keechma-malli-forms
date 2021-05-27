(ns keechma.malli-forms.dirty-test
  (:require [clojure.test :refer [deftest is]]
            [keechma.malli-forms.dirty :refer [calculate-dirty-paths]]))

(deftest dirty-fields-calculation
  (let [dirty-fields (calculate-dirty-paths
                      {:username "retro"
                       :password "foo"
                       :social-networks [{:service "twitter"
                                          :test [{:aa "bb"}]
                                          :foo "bar"}]}
                      {:username "mihaelkonjevic"
                       :password "foo"
                       :social-networks [{:service "twitter"
                                          :test [{:aa "bb" :cc "dd"}]
                                          :username "aaa"}
                                         {:service "facebook"}]})]
    (is (= #{[:username]
             [:social-networks 0 :foo]
             [:social-networks 0 :username]
             [:social-networks 0 :test 0 :cc]
             [:social-networks 1 :service]
             [:social-networks]
             [:social-networks 0 :test]}
           dirty-fields))))

(deftest stress-test
  (let [f1 (map (fn [i] {:id i :name (str i "-name") :address {:street (str i "-address") :city i}}) (range 0 1002))
        f2 (map (fn [i] {:id i :name (str i "-name") :address {:street (str i "-address") :city i}}) (range 1 1003))
        result (calculate-dirty-paths f1 f2)]
    (is (= 4008 (count result)))))