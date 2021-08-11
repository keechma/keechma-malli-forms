(ns keechma.malli-forms.util)

;; https://github.com/weavejester/medley/blob/master/src/medley/core.cljc
(defn dissoc-in
  "Dissociate a value in a nested associative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  ([m ks]
   (if-let [[k & ks] (seq ks)]
     (if (seq ks)
       (let [v (dissoc-in (get m k) ks)]
         (if (empty? v)
           (dissoc m k)
           (assoc m k v)))
       (dissoc m k))
     m))
  ([m ks & kss]
   (if-let [[ks' & kss] (seq kss)]
     (recur (dissoc-in m ks) ks' kss)
     (dissoc-in m ks))))

(defn path-starts-with? [path path-to-match]
  (loop [path path
         path-to-match path-to-match]
    (cond
      (not (seq path-to-match)) true
      (not (seq path)) false
      :else (let [[first-path & rest-path] path
                  [first-path-to-match & rest-path-to-match] path-to-match]
              (if (= first-path first-path-to-match)
                (recur rest-path rest-path-to-match)
                false)))))