(ns liars-dice-round2.core
  (:require [clojure.test :refer :all])
  (:use clojure.pprint))


(def priors-file "priors.txt")
(def grand-prior [10 10])
(def prior-pseudosize 10)

(def dices ["1" "2" "3" "4" "5" "*"])
(def liar "liar")
(def bets (for [n [1 2] d dices] (str n d)))

(def new-state
  {
    :actions '()
    :my-dice nil
    :outcomes {:1 {} :2 {} :3 {} :4 {} :5 {} :* {}}
    })

(defn err [& texts]
  (binding [*out* *err*]
    (apply println texts)))

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

(defn get-possible-actions [last-bet-value]
  (if (nil? last-bet-value) bets
                            (cons liar (rest (drop-while #(not= last-bet-value %)
                                                         bets)))))

(defn MAP [positives negatives]
  (/ positives (+ positives negatives)))

(defn inc-outcome [result]
  (fn [cell-value]
    (let [[wins losses] (if (nil? cell-value)
                          [0, 0]
                          cell-value)]
      (if (= "win" result)
        [(inc wins) losses]
        [wins (inc losses)]))))
(defn update-state [old-state finilizing-input]
  (let [my-dice (:my-dice old-state)
        [result rival-dice _ _] (clojure.string/split finilizing-input #" ")
        new-state (loop [s old-state
                         as (:actions old-state)]
                    (if (empty? as)
                      s
                      (recur (update-in s [:outcomes (keyword my-dice) as] (inc-outcome result))
                             (drop 2 as))))]
    (-> new-state
        (assoc :my-dice nil)  ; reset :my-dce and :actions
        (assoc :actions nil))))
(deftest update-state-test
  (is (= {
           :actions nil
           :my-dice nil
           :outcomes {:1 {} :2 {} :3 {} :4 {} :5 {} :* {'("11") [1 0]
                                                        '("*" "15" "11") [1 0]}}
           }
         (update-state {:actions '("*" "15" "11") :my-dice "*" :outcomes {:1 {} :2 {} :3 {} :4 {} :5 {} :* {}}}
                       "win 2 100 100"))))
(defn prob-win [state action-to-check]
  (let [prior-actions (drop 2 action-to-check)
        my-dice (keyword (:my-dice state))
        [wins losses] (get-in state [:outcomes my-dice action-to-check] [0 0])
        [prior-wins prior-losses] (get-in state [:outcomes my-dice prior-actions] grand-prior)
        k (min 1 (/ prior-pseudosize (+ prior-wins prior-losses)))]
    (/ (+ wins (* k prior-wins))
       (+ wins losses (* k prior-wins) (* k prior-losses)))))

(deftest prob-win-test
  (is (= 1/8
         (prob-win {:my-dice "*" :outcomes {:* {'("15") [10 100]}}} '("15")))))

(defn choose-bet-action [state]
  (let [actions (:actions state)
        possibles (get-possible-actions (first actions))
        with-win-probs (for [a possibles]
                         [a (prob-win state (cons a actions))])
        max-win-prob (apply max (map second with-win-probs))
        max-prob-acts (map first (filter #(= (second %) max-win-prob) with-win-probs))]
    (rand-nth max-prob-acts)))

(defn write-priors [filename priors-map]
  (err "writing priors to" filename)
  (spit filename (with-out-str (pprint priors-map))))

(defn get-all-keys [two-folded-map]
  (apply concat (for [[dice outcomes] two-folded-map]
                  (for [[action oucomes] outcomes]
                    [dice action]))))

(defn reset-cell [cell]
  (let [[W L] cell]
    (if (< (+ W L) prior-pseudosize)
      cell
      (let [n (+ W L)
            k (min 1 (/ prior-pseudosize n))]
        [(* k W) (* k L)]))))

(defn reset-priors [priors-map]
  (loop [m priors-map
         [k & ks] (get-all-keys priors-map)]
    (if (nil? k)
      m
      (recur (update-in m k reset-cell) ks))))

(defn read-priors [filename]
  (if (.exists (clojure.java.io/as-file filename))
    (read-string (slurp filename))
    {}))

(defn decide [state input]
  (if (or (.startsWith input "win")
          (.startsWith input "loss"))
    (let [[result rival games-left timeout-left] (str-split input #" ")]
      [(update-state state input) nil])
    (let [[my-dice str-actions] (str-split input #" ")
          actions (action-split str-actions)
          state-with-dice (if (nil? (:my-dice state))
                            (assoc state :my-dice my-dice)
                            state)
          state-with-last-action (assoc state-with-dice :actions actions)
          my-action (choose-bet-action state-with-last-action)
          state-with-my-last-action (update-in state-with-last-action [:actions] #(cons my-action %))]
      [state-with-my-last-action my-action])))

(defn -main
  [& args]
  (loop [state (assoc new-state :outcomes (read-priors priors-file))
         input (read-line)]
    (if (nil? input)
      nil #_(write-priors priors-file (reset-priors (:outcomes state)))
      (let [[updated-state my-action] (decide state input)]
        (when my-action  ; don't answer if response is nil
          (println my-action)
          (flush))
        (recur updated-state (read-line))))))