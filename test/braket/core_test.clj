(ns braket.core-test
  (:require [midje.sweet :refer :all]
            [braket.core :refer :all]))

(fact
 (let [g (nat-gen 0)]
   (g) => 1
   (g) => 2
   (g) => 3)
 (let [g (nat-gen 7)]
   (g) => 8
   (g) => 9
   (g) => 10))

(fact
 (subtype? ''a ''a) => true
 (subtype? ''a ''b) => false
 (subtype? ''a '(1 'a 'b)) => true
 (subtype? ''b '(1 ('a 1) 'b)) => true
 (subtype? '('a ('a 'b)) '(1 ('a 1) 'b 'c)) => true
 (subtype? '('a ('b 'b)) '(1 ('a 1) 'b 'c)) => false
 (subtype? '(2 'b ('a 2)) '(1 ('a 1) 'b 'c)) => true
 (subtype? '(2 'b ('a 2) 'c) '(1 ('a 1) 'b)) => false
 (subtype? '('a (2 'b ('a 2))) '(1 ('a 1) 'b 'c)) => true)

(fact
 (extract-bindings ''a) => '['a {}]
 (extract-bindings '('a 'b)) => '[('a 'b) {}]
 (extract-bindings '(1 'a)) => '[1 {1 ('a)}]
 (extract-bindings '((1 'a) (2 'b))) => '[(1 2) {1 ('a)
                                                 2 ('b)}]
 (extract-bindings '((1 'a) (1 'b))) => '[(1 1) {1 ('a 'b)}]
 (extract-bindings '(1 'a (2 'b))) => '[1 {1 ('a 2)
                                           2 ('b)}])

(fact
 (embed-bindings ''a {}) => ''a
 (embed-bindings 1 '{1 ('a)}) => '(1 'a)
 (embed-bindings '(1 2) '{1 ('a)
                          2 ('b)}) => '((1 'a) (2 'b))
 (embed-bindings 1 '{1 ('a 2)
                     2 ('b)}) => '(1 'a (2 'b))
 (embed-bindings 1 '{1 ('a ('b 1))}) => '(1 'a ('b 1)))

(fact
 (dedup-bindings {1 '('a 'b)}) => {1 '('a 'b)}
 (dedup-bindings {1 '('a 'a)}) => {1 '('a)}
 (dedup-bindings {1 '('a)
                  2 '('a 1)}) => {1 '('a)
                                  2 '('a)}
 (dedup-bindings {1 '('a)
                  2 '(1 'a)}) => {1 '('a)
                                  2 '(1)}
 (dedup-bindings {1 '('a)
                  2 '('a 'b)
                  3 '(1 2)}) => {1 '('a)
                                 2 '('a 'b)
                                 3 '(2)}
 (dedup-bindings {1 '('a ('b 1) 2)
                  2 '('a ('b 2) 'c)}) => {1 '(2)
                                          2 '('a ('b 2) 'c)}
 (dedup-bindings {1 '(2 'a ('b 1))
                  2 '('a ('b 2) 'c)}) => {1 '(2)
                                          2 '('a ('b 2) 'c)}
 (dedup-bindings {1 '('a ('b 1) 1)}) => {1 '('a ('b 1))})

(fact
 (unify (nat-gen 0) ''a ''a) => '(1 'a)
 (unify (nat-gen 0) ''a ''b) => '(1 'a 'b)
 (unify (nat-gen 3) '(1 'a ('b 1)) ''c) => '(1 'a ('b 1) 'c)
 (unify (nat-gen 3) ''a '(1 'b ('c 1))) => '(4  'a 'b ('c 4))
 (unify (nat-gen 3) '(1 'a ('b 1)) '(2 'c ('d 2))) => '(1 'a ('b 1) 'c ('d 1)))