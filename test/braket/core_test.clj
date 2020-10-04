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
 (unify (nat-gen 0) ''a ''a) => ''a
 (unify (nat-gen 0) ''a ''b) => '(1 'a 'b)
 (unify (nat-gen 0) '('a 'b) '('c 'b)) => '((1 'a 'c) 'b)
 (unify (nat-gen 3) '(1 'a ('b 1)) ''c) => '(1 'a ('b 1) 'c)
 (unify (nat-gen 3) ''a '(1 'b ('c 1))) => '(1 'b ('c 1) 'a)
 (unify (nat-gen 3) '(1 'a ('b 1)) '(2 'c ('d 2))) => '(1 'a ('b 1) 'c ('d 1)))
