(ns braket.core
  (:require [clojure.walk :as walk]))

(defn binding? [a]
  (and (seq? a)
       (int? (first a))))

(defn split-binding [[var & options]]
  [var options])

(defn quoted-symbol? [a]
  (and (seq? a)
       (= (first a) 'quote)
       (symbol? (second a))))

(defn subtype?
  ([sub type] (subtype? sub type {} {}))
  ([sub type bindings assumptions]
   (cond (= sub type) true
         (binding? sub) (every? identity
                                (for [sub' (rest sub)]
                                  (subtype? sub' type bindings
                                            (assoc assumptions (first sub) type))))
         (and (int? sub)
              (= (assumptions sub) type)) true
         (binding? type) (let [bindings' (assoc bindings (first type) (rest type))]
                           (or (some identity (for [type' (rest type)]
                                                (subtype? sub type' bindings' assumptions)))
                               false))
         (int? type) (subtype? sub (conj (bindings type) type) bindings assumptions)
         (quoted-symbol? type) false
         (and (seq? sub)
              (seq? type)
              (= (count sub)
                 (count type))) (every? identity
                                        (map #(subtype? %1 %2 bindings assumptions) sub type))
         :else false)))

(defn extract-bindings [expr]
  (cond
    (quoted-symbol? expr) [expr {}]
    (binding? expr) (let [[var options] (split-binding expr)
                          [options' bindings] (extract-bindings options)]
                      [var (-> bindings (merge {var options'}))])
    (seq? expr) (let [pairs (map extract-bindings expr)]
                  [(map first pairs)
                   (->> pairs (map second) (apply merge-with concat))])
    :else [expr {}]))

(defn embed-bindings [expr bindings]
  (cond
    (and
     (int? expr)
     (contains? bindings expr)) (conj (embed-bindings (bindings expr)
                                                      (-> bindings (dissoc expr)))
                                      expr)
    (seq? expr) (map #(embed-bindings % bindings) expr)
    :else expr))

(defn dedup-bindings* [expr]
  (cond
    (empty? expr) (list)
    :else (let [dedupped (dedup-bindings* (rest expr))]
            (cond
              (some identity (for [d dedupped]
                               (subtype? (first expr) d))) dedupped
              :else (conj dedupped (first expr))))))

(defn dedup-bindings [bindings]
  (->> bindings
       (map (fn [[var expr]]
              [var (->> expr
                        (filter #(not= % var))
                        (map #(embed-bindings % bindings))
                        reverse
                        dedup-bindings*
                        reverse
                        dedup-bindings*
                        extract-bindings first)]))
       (into {})))

(defn nat-gen [init]
  (let [c (atom init)]
    (partial swap! c inc)))

(defn unify [gen a b]
  (let [a (cond (binding? a) a
                :else (list (gen) a))
        b (cond (binding? b) b
                :else (list (gen) b))
        [a a-bindings] (extract-bindings a)
        [b b-bindings] (extract-bindings b)
        b-bindings (walk/postwalk-replace {b a} b-bindings)
        b-bindings (walk/postwalk-replace {b a} b-bindings)
        bindings (merge-with concat a-bindings b-bindings)
        bindings (dedup-bindings bindings)]
    (embed-bindings a bindings)))