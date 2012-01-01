(ns scoring-bowling.test.core
  (:use [scoring-bowling.core])
  (:use [clojure.test])
  (:import scoring_bowling.JavaGameImpl))

(defn do-bowls [game score times]
   (reduce (fn [game _] (bowl game score))  game (range times))
  )

(deftest bowl-zeros  
    (is  (= 0 (total-score(do-bowls (game) 0 20)))
      ))

(deftest bowl-a-strike-puts-you-into-next-frame
  (is (=  2 (count (:frames (bowl(bowl(game)10) 1) ))))

(deftest bowl-2-balls-non-strike-puts-into-next-frame
  (is (= 2 (count (:frames (bowl (bowl(bowl(game)9) 10)1)))))))

(deftest  bowl-strike-before-end-gives-bonus-of-next-two-points
  (is
    (=  26 (total-score(do-bowls 
                        (bowl(bowl (bowl (game) 10) 4)4)
                        0 16)))
    ))

(deftest  bowl-spare-before-end-gives-bonus-of-next-point
  (is
    (=  20 (total-score(do-bowls 
                        (bowl(bowl (bowl (game) 9) 1)5)
                        0 17)))
    ))

(deftest bowl-non-spare-or-strike-in-frame-10-is-end-of-game
  (=  "game has been played out" (try(do-bowls (game)4 21) (catch Exception e (.getMessage e)))))

(deftest bowl-strike-in-last-frame-gives-exactly-2-more-goes
  (is
    (=  30 (total-score  (bowl(bowl (bowl (do-bowls(game)0 18) 10) 10)10)
                        )))  
  (is
    (=  15 (total-score  (bowl(bowl (bowl (do-bowls(game)0 18) 10) 1)4)
                        )))
    (is
    (=  "game has been played out" (try (bowl (bowl(bowl (bowl (do-bowls(game)0 18) 10) 1)4)1
                        ) (catch Exception e (.getMessage e)))))
    )

(deftest bowl-spare-in-last-frame-gives-exactly-1-more-go
   (is
    (=  15 (total-score  (bowl(bowl (bowl (do-bowls(game)0 18) 9) 1)5)
                        )))
   (is
    (=  "game has been played out" (try (bowl (bowl(bowl (bowl (do-bowls(game)0 18) 9) 1)4)1
                        ) (catch Exception e (.getMessage e)))))
  )

(deftest request-score-before-end-of-game-gives-minus-one
  (is (= -1 (total-score (game)))))

(deftest call-java-game
  (is (= 1 (JavaGameImpl.))))
