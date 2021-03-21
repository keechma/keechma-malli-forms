(ns keechma.malli-forms.core
  (:require [keechma.malli-forms.validator :as v]
            [keechma.malli-forms.dirty :refer [calculate-dirty-paths
                                               calculate-dirty-paths-from-path]]
            [keechma.malli-forms.util :refer [dissoc-in]]
            [clojure.set :as set]))

(def default-only-dirty-paths true)

(defn ->path [val]
  (if (sequential? val) val [val]))

(defprotocol IForm
  (assoc-initial-data [this initial-data])
  (assoc-data [this data])
  (assoc-in-data [this path data])
  (-update-in-data [this path f args])
  (dissoc-in-data [this path])
  (get-initial-data [this])
  (get-data [this])
  (get-in-data [this path])
  (get-coerced-data [this])
  (get-errors [this] [this only-dirty-paths])
  (get-in-errors [this path] [this path only-dirty-paths])
  (validate [this] [this only-dirty-paths])
  (validate-in [this path])
  (valid? [this] [this only-dirty-paths])
  (valid-in? [this path] [this path only-dirty-paths])
  (reset [this] [this initial-data]))

(defn update-in-data [form path f & args]
  (-update-in-data form path f args))

(defn make-initial-state [initial-data]
  {:initial-data initial-data
   :data initial-data
   :errors {}
   :dirty-paths #{}
   :cached-dirty-paths #{}})

(defn- get-dirty-errors [{:keys [dirty-paths cached-dirty-paths errors]}]
  (let [all-dirty-paths (set/union dirty-paths cached-dirty-paths)]
    (reduce-kv
      (fn [acc path error]
        (if (contains? all-dirty-paths path)
          (assoc acc path error)
          acc))
      nil
      errors)))

(defn- assoc-dirty-paths [{:keys [initial-data data] :as state}]
  (assoc state :dirty-paths (calculate-dirty-paths initial-data data)))

(defn- assoc-cached-dirty-paths [{:keys [cached-dirty-paths errors] :as state}]
  (assoc state :cached-dirty-paths (set/union (set cached-dirty-paths) (-> errors keys set))))

(defn format-error-messages [error-messages]
  (->> error-messages
    vals
    (apply set/union)
    sort
    vec))

(defn format-errors [errors]
  (let [formatted (->> errors
                    (map (fn [[k v]] [k (format-error-messages v)]))
                    (into {}))]
    (when (seq formatted)
      formatted)))

(defrecord Form [validator state]
  IForm
  (assoc-initial-data [this initial-data]
    (update this :state #(-> %
                           (assoc :initial-data initial-data)
                           assoc-dirty-paths)))
  (assoc-data [this data]
    (assoc-in this [:state :data] data))
  (assoc-in-data [this path data]
    (let [path' (->path path)]
      (-> this
        (assoc-in (into [:state :data] path') data)
        (update-in [:state :cached-dirty-paths] conj path'))))
  (-update-in-data [this path f args]
    (let [path' (->path path)
          this' (apply update-in this (into [:state :data] path') f args)]
      (update-in this' [:state :cached-dirty-paths] conj path')))
  (dissoc-in-data [this path]
    (let [path' (->path path)
          this' (dissoc-in this (into [:state :data] path'))]
      (update-in this' [:state :cached-dirty-paths] conj path')))
  (get-initial-data [_]
    (:initial-data state))
  (get-data [_]
    (:data state))
  (get-in-data [_ path]
    (get-in state (into [:data] (->path path))))
  (get-coerced-data [_]
    (v/coerce validator (:data state)))
  (get-errors [this]
    (get-errors this default-only-dirty-paths))
  (get-errors [_ only-dirty-paths]
    (if only-dirty-paths
      (get-dirty-errors state)
      (:errors state)))
  (get-in-errors [this path]
    (get-in-errors this path default-only-dirty-paths))
  (get-in-errors [this path only-dirty-paths]
    (let [errors (get-errors this only-dirty-paths)]
      (get-in errors (->path path))))
  (validate [this]
    (validate this default-only-dirty-paths))
  (validate [this only-dirty-paths]
    (let [errors (->> this get-data (v/errors validator))
          this-with-errors (assoc-in this [:state :errors] errors)]
      (if only-dirty-paths
        (update this-with-errors :state assoc-dirty-paths)
        (-> this-with-errors
          (update :state assoc-dirty-paths)
          (update :state assoc-cached-dirty-paths)))))
  (validate-in [this path]
    (let [path' (->path path)
          path-and-parent-errors (v/errors-for-path validator (:data state) path')
          errors (reduce-kv
                   (fn [acc k v]
                     (let [new-errors (->> (merge (get acc k) v)
                                        (remove (fn [[_ v]] (nil? v)))
                                        (into {}))]
                       (if (seq new-errors)
                         (assoc acc k new-errors)
                         (dissoc acc k))))
                   (-> state :errors (dissoc path'))
                   path-and-parent-errors)

          path-data (get-in this (into [:state :data] path'))
          path-initial-data (get-in this (into [:state :initial-data] path'))
          dirty-paths (calculate-dirty-paths-from-path path' path-initial-data path-data)]
      (-> this
        (assoc-in [:state :errors] errors)
        (update-in [:state :dirty-paths] set/union dirty-paths))))
  (valid? [this]
    (valid? this default-only-dirty-paths))
  (valid? [this only-dirty-paths]
    (if only-dirty-paths
      (empty? (get-errors this))
      (empty? (:errors state))))
  (valid-in? [this path]
    (valid-in? this path default-only-dirty-paths))
  (valid-in? [this path only-dirty-paths]
    (empty? (get-in-errors this path only-dirty-paths)))
  (reset [this]
    (reset this (:initial-data state)))
  (reset [this initial-data]
    (assoc this :state (make-initial-state initial-data))))

(defn make-form [registry schema initial-data]
  (let [validator (v/make-validator registry schema)]
    (->Form
      validator
      (make-initial-state initial-data))))