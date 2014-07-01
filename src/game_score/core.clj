(ns game-score.core
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string])
  (:gen-class))

(def TEAMS
  ["TOR" "NYY" "BAL" "BOS" "TBR"
   "DET" "KCR" "CLE" "MIN" "CHW"
   "OAK" "LAA" "TEX" "SEA" "HOU"
   "ATL" "WSN" "MIA" "PHI" "NYM"
   "MIL" "CIN" "PIT" "STL" "CHC"
   "SFG" "LAD" "COL" "SDP" "ARI"])

(defn starters
  [team]
  (let [doc (html/html-resource (java.net.URL. (str "http://www.baseball-reference.com/teams/" team "/staff.shtml")))
        cells (html/select doc [:table.stats_table :.YR :td])
        rows (partition 10 cells)]
    (distinct (for [row rows
                    starter (drop-last 4 (rest row))
                    :let [starter (-> starter :content first :attrs)
                          year (string/replace (-> row first html/text) #"(\d{4}).*" "$1")]
                    :when (not (nil? starter))]
                {:name (:title starter)
                 :year year
                 :player-id (string/replace (:href starter) #"/players/./(.*).shtml" "$1")}))))

(defn calc-score
  [{:keys [ip h r er bb k]}]
  (-> 50
      (+ (* 3 (Math/floor ip)) (* 10 (- ip (Math/floor ip))))
      (+ (if (> ip 4.2) (* 2 (- (Math/floor ip) 4)) 0))
      (+ k)
      (- (* 2 h))
      (- (* 4 er))
      (- (* 2 r))
      (- (* 2 bb))))

(defn games
  [player-id year]
  (let [doc (html/html-resource (java.net.URL. (str "http://www.baseball-reference.com/players/gl.cgi?id=" player-id "&t=p&year=" year)))
        rows (html/select doc [:table#pitching_gamelogs :tbody :tr])]
    (for [row rows
          :when (not= "thead" (string/trim (-> row :attrs :class)))
          :while  (not= "partial_table bold_text" (string/trim (-> row :attrs :class)))
          :let [tds (html/select row [:td])
                row-fn #(-> tds (nth %) html/text bigdec)
                game {:ip (row-fn 11)
                      :h  (row-fn 12)
                      :r  (row-fn 13)
                      :er (row-fn 14)
                      :bb (row-fn 15)
                      :k  (row-fn 16)}]]
      (assoc game :score (calc-score game)))))

(defn average
  [s]
  (/ (reduce + s) (if (zero? (count s)) 1 (count s))))

(defn -main
  [& args]
  (doseq [team    TEAMS
          starter (starters team)
          :let [gs (games (:player-id starter) (:year starter))
                scores (map :score gs)]]
    (println (str team "," (average scores) "," (:name starter) "," (:player-id starter) "," (:year starter) "," (count scores)))))
