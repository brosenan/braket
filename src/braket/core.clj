(ns braket.core
  (:require [clojure.walk :as walk]))

(defn binding? [a]
  (and (list? a)
       (int? (first a))))

(defn quoted-symbol? [a]
  (and (list? a)
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
         (and (list? sub)
              (list? type)
              (= (count sub)
                 (count type))) (every? identity
                                        (map #(subtype? %1 %2 bindings assumptions) sub type))
         :else false)))

(defn unify [fresh a b]
  (cond
    (= a b) a
    (and (binding? a)
         (binding? b)) (concat a (walk/prewalk-replace {(first b) (first a)} (rest b)))
    (binding? a) (concat a (list b))
    (binding? b) (concat b (list a))
    (quoted-symbol? a) (list (fresh) a b)
    (and (list? a)
         (list? b)
         (= (count a) (count b))) (map (partial unify fresh) a b)
    :else (list (fresh) a b)))

(defn nat-gen [init]
  (let [c (atom init)]
    (partial swap! c inc)))