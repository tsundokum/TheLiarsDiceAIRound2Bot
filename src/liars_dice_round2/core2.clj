(ns liars-dice-round2.core2
  (:gen-class)
  (:require [clojure.test :refer :all]))

(def dices ["1" "2" "3" "4" "5" "*"])
(def liar "liar")
(def bets (for [n [1 2] d dices] (str n d)))

(defn str-split [input re-delimiter]
  (if (nil? input)
    '()
    (clojure.string/split input re-delimiter)))

(defn action-join [actions]
  (clojure.string/join "," (reverse actions)))

(defn action-split [str-actions]
  (if (empty? str-actions)
    '()
    (reverse (clojure.string/split str-actions #","))))

(deftest actions-parser-test
  (is (= "11,15,2*" (action-join '("2*" "15" "11"))))
  (is (= '("2*" "15" "11") (action-split "11,15,2*"))))


(defn create-new-state []
  {:actions nil
   :simple-tree {}})

(defn game-finished [state input]
  (let [[result rival games-left timeout-left] (str-split input #" ")
        simple-tree (state :simple-tree)
        actions (state :actions)
        new-simple-tree (update-in simple-tree actions )]
    [{:actions nil
      :simple-tree new-simple-tree} nil]))

(defn choose-action [state input]
  (let [[my-dice actions-str] (str-split input #" ")
        actions (action-split actions-str)
        new-state (update-in state :actions (constantly actions))
        response (if (empty? actions)
                   (str "1" my-dice)
                   "liar")]
    [new-state, response]))

(defn append-my-action [old-state response]
  (let [old-actions (old-state :action)
        new-actions (-> old-actions
                        action-split
                        (concat [response])
                        action-join)
        new-state (assoc old-state :actions new-actions)]
    [new-state response]))

(deftest append-my-action-test
  (is (= [{:actions "11",
           :simple-tree          {}} "11"]
         (append-my-action (create-new-state) "11"))
      (= [{:actions "11,2*,liar",
           :simple-tree          {}} "liar"]
         (append-my-action {:actions "15,2*",
                           :simple-tree          {}} "liar"))))

(defn inc0
  "inc if given a number or (inc 0) if NaN"
  [n]
  (inc (if (nil? n) 0 n)))

(defn grow-expectation-trees [state input]
  (let [actions-but-mine (-> state
                             :actions
                             action-split
                             butlast
                             action-join)
        [_ rival-dice _ _] (str-split input #" ")]
    (update-in state [:simple-tree actions-but-mine rival-dice] inc0)))

;(deftest grow-expectation-trees-test
;  (is (= {"11,2*" {"1" 1}}
;         (:simple-tree (grow-expectation-trees {:actions "11,2*,liar", :simple-tree {}}
;                                               "win 1 100 100")))
;      (= {"11,2*" {"1" 2}}
;         (:simple-tree (grow-expectation-trees {:actions "11,2*,liar", :simple-tree {"11,2*" {"1" 1}}}
;                                               "win 1 100 100")))))

(defn destructure-bet [str-action]
  (let [n (Character/getNumericValue (first str-action))
        d (str (second str-action))]
    [n d]))

(defn get-winner [[checker-name c-dice] [last-better-name lb-dice] bet]
  (let [[n d] (destructure-bet bet)]
    (if (<= n (count (filter #(or (= % d)
                                  (= % "*"))
                             [c-dice lb-dice])))
      last-better-name
      checker-name)))

(deftest get-winner-test
  (is (= :a
         (get-winner [:b "1"] [:a "1"] "21")))
  (is (= :b
         (get-winner [:b "1"] [:a "3"] "21")))
  (is (= :b
         (get-winner [:b "1"] [:a "3"] "1*")))
  (is (= :a
         (get-winner [:b "1"] [:a "*"] "21"))))

(defn get-possible-actions [last-bet-value]
  (if (nil? last-bet-value) bets
                            (cons liar (rest (drop-while #(not= last-bet-value %)
                                                         bets)))))

(defn all-outcomes-with-probabilies [[checker-name checker-dice-e]
                                     [bet-name bet-dice-e bet-real-dice]
                                     bet-value]
  (for [my-d dices
        rival-d dices
        :let [prob-choosing (* (checker-dice-e my-d)
                   (bet-dice-e rival-d))]
        :when (pos? prob-choosing)]
    [prob-choosing (get-winner [checker-name my-d]
                               [bet-name (if (nil? bet-real-dice) rival-d bet-real-dice)]
                               bet-value)]))

(defn probability-of-winning [[me & others]
                              [[my-last-action _] & prev-actions :as actions]
                              [[my-dice-e rival-dice-e rival-real-dice] & next-e]]
  (let [[bet rival] (first prev-actions)]
    (if (= liar my-last-action)
      (let [outcomes (all-outcomes-with-probabilies [me my-dice-e] [rival rival-dice-e rival-real-dice] bet)]
        (->> outcomes
             (filter #(= (second %) me))
             (map first)
             (apply +)))
      (let [possible-actions (get-possible-actions my-last-action)
            probs (map #(probability-of-winning others
                                                (cons [% rival] actions)
                                                next-e) possible-actions)
            [_ loss-prob] (apply max-key second (map vector possible-actions probs))]
        (- 1 loss-prob)))))

(defn duniform [_]
  1/6)

(defn ddetermined [determined-dice]
  (fn [dice]
    (if (= dice determined-dice) 1 0)))


(deftest probability-of-winning-test
  (is (= 35/36
         (probability-of-winning (cycle [:marat :anton])
                                 (list ["liar" :marat] ["2*" :anton])
                                 (cycle [[duniform duniform]]))))
  (is (= 1
         (probability-of-winning (cycle [:marat :anton])
                                 (list ["liar" :marat] ["33" :anton])
                                 (cycle [[duniform duniform]]))))
  (is (= 4/9
         (probability-of-winning (cycle [:marat :anton])
                                 (list ["liar" :marat] ["15" :rival] ["11" :marat])
                                 (cycle [[duniform duniform]]))))
  (is (= 1/9
         (probability-of-winning (cycle [:anton :marat])
                                 (list ["25" :anton] ["11" :marat])
                                 (cycle [[duniform duniform]])))))

;(let [m-dice "5"]
;  (println "my dice =" m-dice)
;  (doseq [m-move bets]
;    (apply println m-move
;           (probability-of-winning (cycle [:m :a])
;                                   (list [m-move :m] [])
;                                   (cycle [[(ddetermined m-dice) duniform]
;                                           [duniform duniform]]))
;           (for [a-dice dices]
;             [a-dice
;              (probability-of-winning (cycle [:m :a])
;                                      (list [m-move :m])
;                                      (cycle [[(ddetermined m-dice) duniform]
;                                              [(ddetermined a-dice) duniform]]))
;              (probability-of-winning (cycle [:m :a])
;                                      (list [m-move :m])
;                                      (cycle [[(ddetermined m-dice) duniform a-dice]
;                                              [(ddetermined a-dice) (ddetermined m-dice) m-dice]]))]))))

;(defn best-move [state input]
;  )
;
;(deftest best-move-test
;  (is (= "liar"
;         (best-move {:actions "", :simple-tree {}} "1 25"))))

(defn communicate [state input]
  (if (or (.startsWith input "win")
          (.startsWith input "loss"))
    (game-finished state input)
    (append-my-action state (choose-action state input))))

(defn -main2
  [& args]
  (loop [state (create-new-state)
         input (read-line)]
    (let [[new-state response] (communicate state input)]
      (when response
        (println response))
      (recur new-state (read-line)))))
