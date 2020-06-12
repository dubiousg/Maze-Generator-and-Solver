(ns mazepractice.core
  (:require [clojure.data.json :as json]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :refer [site]]
            [ring.util.response :as response]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :as jetty]))

(do (use 'ring.adapter.jetty)
    (use 'mazepractice.core)
    (run-jetty handler {:port 3000}))

;START OF MAZE FUNCTIONS

;all Cell methods
(defrecord Cell [north south east west
                 links id])

(defn cells-linked? [c1 c2]
  (and (.contains @(:links c1) (:id c2))
       (.contains @(:links c2) (:id c1))))

(defn link-cells
  ([c1 c2] (link-cells c1 c2 true))
  ([c1 c2 bidi]
   ;(println "linking")
   (if (nil? c2)
     nil
     (do
       (swap! (:links c1) conj (:id c2))
       (when bidi
         (link-cells c2 c1 false))))))

;grid methods
(defrecord Maze [grid start-end rows columns])

(defn prepare-grid [rows columns] ;includes the original configure cells function
  (vec (for [i (range rows)]
         (vec (for [j (range columns)]
                (Cell.
                  [(dec i) j] [(inc i) j] [i (inc j)] [i (dec j)] ;north south east west
                  (atom []) ;links
                  (+ (* columns i) j))))))) ;id

(defn def-start-end [row col]
  (let [entrance (let [num (rand-int 4)]
                   (cond
                     (= num 0) (rand-int col)
                     (= num 1) (* (rand-int row) col)
                     (= num 2) (+ (rand-int col) (* (dec row) col))
                     (= num 3) (+ (* (rand-int row) col) (dec col))
                     )) exit entrance]
    (loop [ext exit ent entrance]
      (if (not= ext ent)
        [ent ext]
        (recur (let [num (rand-int 4)]
                 (cond
                   (= num 0) (rand-int col)
                   (= num 1) (* (rand-int row) col)
                   (= num 2) (+ (rand-int col) (* (dec row) col))
                   (= num 3) (+ (* (rand-int row) col) (dec col))
                   )) entrance)))))

(defn within-bounds [row col grid]
  (and (< -1 row)
       (< row (count grid))
       (< -1 col)
       (< col (count (grid row)))))

(defn coords->cell [grid coords]
  (let [row (coords 0) col (coords 1)]
    (when (within-bounds row col grid)
      ((grid row) col))))

;cell.north = self[row-1, col]
;cell.south = self[row+1, col]
;cell.west = self[row, col-1]
;cell.east = self[row, col+1]

;grid for the maze, start-end is a vector 0 is the start 1 is the end using
;ids of the cells.

(defn binary-tree [maze]
  (let [grid maze]
    (for [row (range (count grid))]
      (for [col (range (count (grid row)))]
        ;(;(println row)
        ;(println col)
        (link-cells
          ((grid row) col)
          (cond
            (and (< (dec row) 0) (= (count (grid row)) (inc col))) nil ;neither north or east possible
            (and (< (inc col) (count (grid row))) (< (dec row) 0)) ((grid row) (inc col)) ;east since no north
            (and (< -1 (dec row)) (<= (count (grid row)) (inc col))) ((grid (dec row)) col) ;north since no east
            :else
            (if (= 1 (rand-int 2)) ;randomly link north or east
              ((grid (dec row)) col)
              ((grid row) (inc col))))
          )))))

(defn draw-maze [maze]
  (conj
    (vec
      (apply concat
             (let [grid maze m-string []]
               (for [row grid]
                 (conj m-string
                       (str (apply str
                                   (for [cell row]
                                     (if (cells-linked? cell (coords->cell grid (:north cell)))
                                       "+   "
                                       "+---"))) "+")
                       (str "|" (apply str
                                       (for [cell row]
                                         (if (cells-linked? cell (coords->cell grid (:east cell)))
                                           "    "
                                           "   |")))))))))
        (str (apply str (for [cell (last maze)]
                          "+---"
                          )) "+")))

;work for printing maze to text file
(defn de-atomise-maze [maze]
  (vec (for [row maze]
         (vec (for [cell row]
                (Cell. (:north cell) (:south cell) (:east cell) (:west cell)
                       @(:links cell) (:id cell)))))))

(defn write-maze [maze file]
  (spit (str "resources/public/" file) (json/write-str (de-atomise-maze (:grid maze)))))

;END OF MAZE FUNCTIONS

(defn create-maze [size file]
  (println "creating the maze")
  (let [maze (Maze. (prepare-grid size size) (def-start-end size size) size size)]
      (println (:grid maze))
      (println (binary-tree (:grid maze)))
      (write-maze maze file)))

(defn generate-maze [maze-request]
  (println "Generating Maze")
  (let [size (Integer. maze-request)
        file (str size "x" size ".json")
        dir (str "/public/" file)]
   (create-maze size file)
   (println "Sending response")
   (response/resource-response dir)))

(defroutes handler
           (GET "/" [] "Connected")
           (GET "/:maze-request" [maze-request] (generate-maze maze-request))
           (route/not-found "Resource not found"))


(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3000}))
;to access
;http://127.0.0.1:3000/
(-main)

