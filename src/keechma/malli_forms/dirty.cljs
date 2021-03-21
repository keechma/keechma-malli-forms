(ns keechma.malli-forms.dirty
  (:require [clojure.data :refer [diff]]))

(defn ^:private analyze-diff
  ([data] (analyze-diff data [] {:results [] :lengths {}}))
  ([data path results]
   (reduce-kv
     (fn [m k v]
       (if (or (vector? v) (map? v))
         (let [{:keys [results lengths]} m
               new-path (conj path k)
               child-diff (analyze-diff v new-path {:results [] :lengths {}})
               new-results (:results child-diff)
               new-lengths (:lengths child-diff)
               lengths-with-current (if (vector? v)
                                      (assoc lengths new-path (count v))
                                      lengths)]
           {:results (into results new-results)
            :lengths (merge new-lengths lengths-with-current)})
         (if v
           (assoc m :results (conj (:results m) (conj path k)))
           m)))
     results
     data)))

(defn calculate-dirty-paths
  "Calculates the key paths that are dirty by diffing the initial and current form data."
  [prev current]
  (let [[p-diff c-diff] (into [] (diff prev current))
        p-report (analyze-diff p-diff)
        c-report (analyze-diff c-diff)
        [p-lengths-diff c-lengths-diff] (diff (:lengths p-report) (:lengths c-report))]
    (set (concat (:results p-report)
           (:results c-report)
           (keys p-lengths-diff)
           (keys c-lengths-diff)))))

(defn calculate-dirty-paths-from-path [path prev current]
  (cond
    (and (or (nil? prev) (coll? prev)) (or (nil? current) (coll? current)))
    (->> (calculate-dirty-paths prev current)
      (map #(into path %))
      set)

    (not= prev current)
    #{path}

    :else nil))