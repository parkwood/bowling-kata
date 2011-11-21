(ns scoring-bowling.core)

(defprotocol Game
  (total-score [this])
  (bowl [this points])
  )

(defprotocol Frame
  (next-frame? [this] )
  (score [this future-scores] )
  (bowl-point [this frames points]
        )
  )
  
(defn both-rolls-taken? [{:keys [roll1 roll2] :as frame}] (every? identity [roll1 roll2]))
(defn strike? [{:keys [roll1]}] (= 10 roll1))
(defn spare? [{:keys [roll1 roll2] :as frame}] 
  (and 
    (both-rolls-taken? frame)
    (= 10 (+ roll1 roll2))))  

(defrecord tenth-frame [roll1 roll2 roll3]
  Frame
  (next-frame? [this] 
           (if 
             (or
               (identity roll3)
               (and (identity roll2) (not (or (strike? this) (spare? this)))))
             true false)           
           )
  (score [this future-scores] 
         ;(prn (vals this))
         (apply + (filter identity (vals this)))
         )
  (bowl-point [this frames points]
              (if (next-frame? this)
                (throw (java.lang.Exception. "game has been played out"))
                (conj (pop frames)
                      (if (nil? roll2)
                        (assoc this :roll2 points)
                        (assoc this :roll3 points))) 
                ) 
              )
  )



(defrecord not-tenth-frame [roll1 roll2]
  Frame
  (next-frame? [this]
               (if (or 
                     (strike? this) 
                     (both-rolls-taken? this)) true false ))
  (score [this future-scores]
         (cond
           (strike? this) (apply + (take 3  future-scores)) 
           (spare? this)   (apply + (take 3 future-scores))
           :else  (apply + (take 2 future-scores))
           ))
  (bowl-point [this frames points]
        (if (next-frame? this)
          (if (= 9 (count frames))
            (conj frames  (tenth-frame. points nil nil)) 
            (conj frames  (not-tenth-frame. points nil))) 
          (conj (pop frames) (assoc this :roll2 points)))  )
  )


(defn still-playing? [frames]
  (let [frame-count (count frames)
        recent (peek frames)]
   ; (prn recent frame-count)
    (if
      (or           
          (and 
            (= 10 frame-count)
            (not (next-frame? recent))
          )
          (< frame-count 10)
          ) true false)
  ))

(defn tot-up-scores [[current-frame & others :as frames] total-score]
 ; (prn current-frame score)
  (let [future-scores (filter identity (flatten(map vals (take 3 frames)))) ; 3 because strike could be followed by 2 strikes
        frame-score (score current-frame future-scores)
        new-score (+ total-score frame-score)]
    (if (not(seq others)) 
      new-score
      (tot-up-scores others new-score)       
      )    
    )  
  )

;blatting defrecords in a repl
; seancorfield

(defrecord GameImpl [frames ]
  Game
  (total-score [this] 
               (if (still-playing? frames)
                 (do (prn "please keep playing") -1)
                 (tot-up-scores frames 0))) 
  (bowl [this points]
        ;(prn (seq frames))
        (if (not(seq frames))
          (GameImpl. [(not-tenth-frame. points nil)])
          (GameImpl. (bowl-point (peek frames) frames points))        
)))

(defn game [] 
  (GameImpl. []))

