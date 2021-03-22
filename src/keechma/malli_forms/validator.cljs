(ns keechma.malli-forms.validator
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.util :as mu]
            [malli.transform :as mt]))

(defprotocol IValidator
  (coerce [this data])
  (errors [this data])
  (errors-for-path [this data path])
  (cleared-parent-errors-for-path [this data path]))

(def path-validation-schema-types #{:fn})

(def set-conj (fnil conj #{}))

(defn humanize
  ([explanation]
   (humanize explanation nil))
  ([{:keys [value errors schema]} {f :wrap :or {f :message} :as options}]
   (when errors
     (let [explained
           (if (coll? value)
             (reduce
               (fn [acc error]
                 (let [error-path (me/error-path error options)]
                   (update-in acc [error-path (-> error :schema type)] set-conj (f (me/with-error-message error options)))))
               nil errors)
             (reduce
               (fn [acc error]
                 (update-in acc [:errors (-> error :schema type)] set-conj (f (me/with-error-message error options))))
               nil errors))]
       explained))))

(defn normalization-transformer [opts]
  (let [transform
        {:compile (fn [schema _]
                    (let [opts'         (assoc opts ::m/walk-schema-refs true
                                                    ::m/walk-refs true
                                                    ::m/walk-entry-vals true)
                          subschemas    (mu/subschemas schema opts')
                          sub-map-attrs (reduce
                                          (fn [acc {:keys [in schema]}]
                                            (if (and (contains? #{map? :map} (m/type schema opts))
                                                  (= 1 (count in)))
                                              (conj acc (first in))
                                              acc))
                                          #{}
                                          subschemas)]
                      (fn [val]
                        (reduce
                          (fn [acc attr]
                            (update acc attr #(or % {})))
                          val
                          sub-map-attrs))))}]
    (mt/transformer
      {:decoders {:map transform}
       :encoders {:map transform}})))

(defn path-validator
  ([subschemas-by-path path data]
   (path-validator {:current-path [] :schema-path [] :errors {}} subschemas-by-path path data))
  ([{:keys [current-path schema-path errors]} subschemas-by-path path data]
   (let [schemas (if (nil? path)
                   (get subschemas-by-path schema-path)
                   (->> schema-path
                     (get subschemas-by-path)
                     (filter #(contains? path-validation-schema-types (:type %)))))
         errors' (reduce
                   (fn [acc {:keys [explainer schema]}]
                     (let [schema-errors (-> data explainer :errors)]
                       (if schema-errors
                         (reduce
                           (fn [acc' error]
                             (let [error-path (into current-path (me/error-path error {}))
                                   message    (:message (me/with-error-message error {}))]
                               (update-in acc' [error-path (type schema)] set-conj message)))
                           acc
                           schema-errors)
                         (let [properties      (m/properties schema)
                               error-path      (:error/path properties)
                               full-error-path (if error-path (into current-path error-path) current-path)]
                           (update-in acc [full-error-path (type schema)] #(or % nil))))))
                   errors
                   schemas)]
     (if (seq path)
       (let [[next-current-attr & rest-path] path
             next-current-path (conj current-path next-current-attr)
             next-schema-path  (conj schema-path (if (int? next-current-attr) ::m/in next-current-attr))]
         (recur
           {:errors errors'
            :current-path next-current-path
            :schema-path next-schema-path}
           subschemas-by-path
           rest-path
           (get data next-current-attr)))
       errors'))))

(defn optimistic-path-validator
  ([subschemas-by-path path data]
   (optimistic-path-validator {:current-path [] :schema-path [] :errors {}} subschemas-by-path path data))
  ([{:keys [current-path schema-path errors]} subschemas-by-path path data]
   (if (nil? path)
     errors
     (let [schemas  (->> schema-path
                      (get subschemas-by-path)
                      (filter #(contains? path-validation-schema-types (:type %))))
           errors' (reduce
                     (fn [acc {:keys [explainer schema]}]
                       (let [schema-errors (-> data explainer :errors)]
                         (if-not schema-errors
                           (let [properties      (m/properties schema)
                                 error-path      (:error/path properties)
                                 full-error-path (if error-path (into current-path error-path) current-path)]
                             (update-in acc [full-error-path (type schema)] #(or % nil)))
                           acc)))
                     errors
                     schemas)]
       (if (seq path)
         (let [[next-current-attr & rest-path] path
               next-current-path (conj current-path next-current-attr)
               next-schema-path  (conj schema-path (if (int? next-current-attr) ::m/in next-current-attr))]
           (recur
             {:errors errors'
              :current-path next-current-path
              :schema-path next-schema-path}
             subschemas-by-path
             rest-path
             (get data next-current-attr)))
         errors')))))

(defn get-subschemas-by-path [registry schema]
  (let [opts       {:registry registry}
        subschemas (mu/subschemas schema (assoc opts ::m/walk-schema-refs true
                                                     ::m/walk-refs true
                                                     ::m/walk-entry-vals true))]
    (->> subschemas
      (map
        (fn [{:keys [schema] :as s}]
          (assoc s :type (m/type schema opts)
                   :explainer (m/explainer schema opts))))
      (group-by :in))))

(defn make-validator [registry schema]
  (let [validator                  (m/validator schema {:registry registry})
        explainer                  (m/explainer schema {:registry registry})
        transformer                (mt/transformer
                                     mt/string-transformer
                                     (normalization-transformer {:registry registry}))
        coercer-transformer        (mt/transformer mt/string-transformer)
        decoder                    (m/decoder schema {:registry registry} transformer)
        subschemas-by-path         (get-subschemas-by-path registry schema)
        path-validator'            (partial path-validator subschemas-by-path)
        optimistic-path-validator' (partial optimistic-path-validator subschemas-by-path)]
    (reify IValidator
      (coerce [_ data]
        (coercer-transformer data))
      (errors [_ data]
        (let [normalized-data (decoder data)]
          (when-not (validator normalized-data)
            (-> normalized-data explainer humanize))))
      (errors-for-path [_ data path]
        (let [normalized-data (decoder data)
              errors          (path-validator' path normalized-data)]
          (when (seq errors)
            errors)))
      (cleared-parent-errors-for-path [_ data path]
        (optimistic-path-validator' path (decoder data))))))