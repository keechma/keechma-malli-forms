(ns keechma.malli-forms.util-test
  (:require [clojure.test :refer [deftest is]]
            [keechma.malli-forms.util :as u]))

(deftest path-starts-with?
  (is (u/path-starts-with? [:foo :bar :baz] [:foo]))
  (is (u/path-starts-with? [:foo :bar :baz] [:foo :bar]))
  (is (u/path-starts-with? [:foo :bar :baz] [:foo :bar :baz]))
  (is (u/path-starts-with? [:foo :bar 1 :baz 2] [:foo :bar 1]))
  (is (not (u/path-starts-with? [:foo] [:bar])))
  (is (not (u/path-starts-with? [:foo] [:foo :bar])))
  (is (not (u/path-starts-with? [:foo 1] [:bar 1]))))