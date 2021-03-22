(ns keechma.malli-forms.core-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [malli.core :as m]
            [keechma.malli-forms.core :as c]))

#_(use-fixtures :each
  (fn [f]
    (js/console.clear)
    (f)))

(def schema-registry
  (merge
    (m/default-schemas)
    {::article [:map
                [:title :string]
                [:description [:string {:min 1}]]
                [:body :string]
                [:author ::author]
                [:comments {:optional true} [:vector ::comment]]]
     ::author [:map
               [:id :int]
               [:username :string]]
     ::comment [:map
                [:id :int]
                [:body [:string {:min 1}]]
                [:author ::author]]

     ::registration [:and
                     [:map
                      [:email [:and
                               :string
                               [:re {:error/message "must be a valid email"}
                                #".*@.*$"]]]
                      [:password [:string {:min 8}]]
                      [:password-confirmation [:string {:min 8}]]]
                     [:fn
                      {:error/path [:password-confirmation]
                       :error/message "must match password"}
                      (fn [{:keys [password password-confirmation]}]
                        (= password password-confirmation))]]

     ::foo [:and
            [:map
             [:bar :int]
             [:baz [:and
                    :int
                    [:fn {:error/message "Must be a positive integer"} pos?]]]
             [:qux :int]]
            [:fn
             {:error/path [:bar]
              :error/message "Total must be 2"}
             (fn [{:keys [bar baz]}] (= 2 (+ bar baz)))]]}))

(deftest basic
  (let [state* (atom (c/make-form schema-registry ::article {}))]
    (swap! state* #(-> %
                     (c/assoc-in-data :title "Article title")
                     c/validate))
    (is (=  "Article title" (c/get-in-data @state* :title)))
    (is (c/valid? @state* true))
    (is (nil? (-> @state* c/get-errors c/format-errors)))
    (swap! state* #(-> %
                     (c/assoc-in-data :description "")
                     c/validate))
    (is (= {:title "Article title"
            :description ""}
          (c/get-data @state*)))
    (is (not (c/valid? @state* true)))
    (is (= {[:description] ["should be at least 1 characters"]}
          (-> @state* c/get-errors c/format-errors)))
    (swap! state* c/validate false)
    (is (= {[:description] ["should be at least 1 characters"]
            [:body] ["missing required key"]
            [:author :id] ["missing required key"]
            [:author :username] ["missing required key"]}
          (-> @state* c/get-errors c/format-errors)))
    ;; Decode from string
    (swap! state* #(-> %
                     (c/assoc-in-data [:author :id] "1")
                     c/validate))
    (is (= {[:description] ["should be at least 1 characters"]
            [:body] ["missing required key"]
            [:author :username] ["missing required key"]}
          (-> @state* c/get-errors c/format-errors)))
    ))

(deftest validate-in
  (let [state* (atom (c/make-form schema-registry ::foo {}))]
    (swap! state* #(-> %
                     (c/assoc-in-data :bar 1)
                     (c/validate-in :bar)))
    (is (= {[:bar] ["Total must be 2"]}
          (-> @state* c/get-errors c/format-errors)))
    (swap! state* #(-> %
                     (c/assoc-in-data :baz 1)
                     (c/validate-in :baz)))
    (is (c/valid? @state*))
    (swap! state* c/validate false)
    (is (not (c/valid? @state*)))
    (is (= {[:qux] ["missing required key"]}
          (-> @state* c/get-errors c/format-errors)))
    (swap! state* #(-> %
                     (c/assoc-in-data :qux 1)
                     (c/validate-in :qux)))
    (is (c/valid? @state*))
    (swap! state* c/validate false)
    (is (c/valid? @state*))
    (swap! state* #(-> %
                     (c/assoc-in-data :baz 3)
                     (c/validate-in :baz)))
    (is (= {[:bar] ["Total must be 2"]}
          (-> @state* c/get-errors c/format-errors)))
    (swap! state* #(-> %
                     (c/assoc-in-data :bar -1)
                     (c/validate-in :bar)))
    (is (c/valid? @state*))
    (swap! state* c/validate false)
    (is (c/valid? @state*))))

(deftest validate-in-2
  (let [state* (atom (c/make-form schema-registry ::article {:comments (mapv (constantly {}) (range 0 10))}))]
    (swap! state* #(-> %
                     (c/assoc-in-data [:comments 3 :body] "")
                     (c/validate-in [:comments 3 :body])))
    (is (= {[:comments 3 :body] ["should be at least 1 characters"]}
          (-> @state* c/get-errors c/format-errors)))))

(deftest registration
  (let [state* (atom (c/make-form schema-registry ::registration {}))]
    (swap! state* #(-> %
                     (c/assoc-in-data [:email] "example")
                     (c/validate-in [:email])))
    (is (= {[:email] ["must be a valid email"]}
          (-> @state* c/get-errors c/format-errors)))
    (swap! state* #(-> %
                     (c/assoc-in-data [:email] "example@example.com")
                     (c/validate-in [:email])))
    (is (c/valid? @state*))
    (swap! state* #(-> %
                     (c/assoc-in-data [:password] "1234567890")
                     (c/validate-in [:password])))
    (is (c/valid? @state*))
    (is (not (c/valid? @state* false)))
    (is (= {[:password-confirmation] ["must match password"]}
          (-> @state* (c/get-errors false) c/format-errors)))
    (swap! state* #(-> %
                     (c/assoc-in-data [:password-confirmation] "1234567890")
                     (c/validate-in [:password-confirmation])))
    (is (c/valid? @state*))
    (is (c/valid? @state* false))
    (swap! state* #(-> %
                     (c/assoc-in-data [:password] "12345678")
                     (c/validate-in [:password])))
    (is (not (c/valid? @state*)))
    (is (= {[:password-confirmation] ["must match password"]}
          (-> @state* (c/get-errors false) c/format-errors)))
    (is (= ["must match password"]
          (-> @state* (c/get-in-errors :password-confirmation) c/format-error-messages)))
    (swap! state* #(-> %
                     (c/assoc-in-data [:password] "1234567890")
                     (c/validate-in [:password])))
    (is (c/valid? @state*))
    (swap! state* #(-> %
                     (c/assoc-in-data [:password] "12345678")
                     (c/validate-in [:password])))
    (is (not (c/valid? @state*)))
    (swap! state* #(-> %
                     (c/assoc-in-data [:password] "1234567890")
                     (c/validate-optimistically-in [:password])))
    (is (c/valid? @state*))))

(comment
  (defn measure-performance []
    (let [state (c/make-form schema-registry ::article
                  {:comments (mapv (constantly {:id "1"}) (range 0 25))})
          state (c/validate state false)]
      (js/console.time "p")
      ;(c/validate state false)
      (dotimes [n 100]
        (c/validate-in state [:comments 23 :body]))
      (js/console.timeEnd "p")))
  (measure-performance))