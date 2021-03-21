(ns keechma.malli-forms.validator
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.util :as mu]
            [malli.transform :as mt]))

(defprotocol IValidator
  (coerce [this data])
  (errors [this data])
  (errors-for-path [this data path]))

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
                   (update-in acc [error-path schema] set-conj (f (me/with-error-message error options)))))
               nil errors)
             (reduce
               (fn [acc error]
                 (update-in acc [:errors schema] set-conj (f (me/with-error-message error options))))
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

(defn make-subschemas-humanizer [registry schema]
  (let [opts                    {:registry registry}
        subschemas              (mu/subschemas schema (assoc opts ::m/walk-schema-refs true
                                                                  ::m/walk-refs true
                                                                  ::m/walk-entry-vals true))
        subschemas-by-path      (->> subschemas
                                  (map
                                    (fn [{:keys [schema] :as s}]
                                      (assoc s :type (m/type schema opts)
                                               :explainer (m/explainer schema opts))))
                                  (group-by :in))]
    (fn humanizer
      ([path data] (humanizer {:current-path [] :schema-path [] :errors {}} path data))
      ([{:keys [current-path schema-path errors]} path data]
       (let [schemas (if (nil? path)
                       (get subschemas-by-path schema-path)
                       (->> schema-path
                         (get subschemas-by-path)
                         (filter #(contains? path-validation-schema-types (:type %)))))
             errors'  (reduce
                        (fn [acc {:keys [explainer schema]}]
                          (let [schema-errors (-> data explainer :errors)]
                            (if schema-errors
                              (reduce
                                (fn [acc' error]
                                  (let [error-path (into current-path (me/error-path error {}))
                                        message (:message (me/with-error-message error {}))]
                                    (update-in acc' [error-path schema] set-conj message)))
                                acc
                                schema-errors)
                              (let [properties (m/properties schema)]
                                (if-let [error-path (:error/path properties)]
                                  (let [full-error-path (into current-path error-path)]
                                    (update-in acc [full-error-path schema] #(or % nil)))
                                  acc)))))
                        errors
                        schemas)]
         (if (seq path)
           (let [[next-current-attr & rest-path] path
                 next-current-path (conj current-path next-current-attr)
                 next-schema-path  (conj schema-path (if (int? next-current-attr) ::m/in next-current-attr))]
             (recur {:errors errors'
                     :current-path next-current-path
                     :schema-path next-schema-path}
               rest-path
               (get data next-current-attr)))
           errors'))))))

(defn make-validator [registry schema]
  (let [validator            (m/validator schema {:registry registry})
        explainer            (m/explainer schema {:registry registry})
        transformer          (mt/transformer
                               mt/string-transformer
                               (normalization-transformer {:registry registry}))
        coercer-transformer  (mt/transformer mt/string-transformer)
        decoder              (m/decoder schema {:registry registry} transformer)
        subschemas-humanizer (make-subschemas-humanizer registry schema)]
    (reify IValidator
      (coerce [_ data]
        (coercer-transformer data))
      (errors [_ data]
        (let [normalized-data (decoder data)]
          (when-not (validator normalized-data)
            (-> normalized-data explainer humanize))))
      (errors-for-path [_ data path]
        (let [normalized-data (decoder data)
              errors (subschemas-humanizer path normalized-data)]
          (when (seq errors)
            errors))))))

