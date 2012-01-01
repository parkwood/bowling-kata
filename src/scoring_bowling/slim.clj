(ns scoring-bowling.slim
  (:use [clojure.test]))

(defn bowl
  "2 impls:
      -version taking one arg [history] returns score of game so far. 
      -version with two args [history pins] records the new pins bowls and returns game"
  ([history]
    (loop [[first & tail-minus-first :as bowls] history 
           score 0]      
      (let [[second & tail-minus-second] tail-minus-first
            [third  & tail-minus-third]  tail-minus-second]            
        (if (nil? tail-minus-third)
          (apply + score (filter identity bowls))
          (cond
            (= 10 first)            (recur tail-minus-first  (+ score 10 second third))
            (= 10 (+ first second)) (recur tail-minus-second (+ score 10 third))
            :else                   (recur tail-minus-second (+ score first second)))))))
  ([history pins]
    (partial bowl (conj history pins))))

(defn new-game []
  (partial bowl []))

(defn do-bowls [pins-bowled]
  (reduce (fn [game pins] (game pins)) (new-game) pins-bowled))

(deftest score-starts-at-0
  (is (= 0 ((do-bowls [0])))))

(deftest perfect-game
  (is (= 300 ((do-bowls (take 12 (repeat 10)))))))

(deftest heartbreak-game
  (is (= 299 (((do-bowls (take 11 (repeat 10)))9)))))  

(deftest can-only-hit-one-pin-game
  (is (= 20 ((do-bowls (take 20 (repeat 1)))))))


