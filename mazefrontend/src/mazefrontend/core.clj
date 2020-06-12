(ns mazefrontend.core
    (:require [quil.core :as q]
      [quil.middleware :as m]
      [clj-http.client :as client]
      [clojure.data.json :as json]))

(defrecord Cell [north south east west
                 links id])

(defn id->cell [id maze]
  (let [grid (:grid maze) width (:columns maze)
        row (quot id width) col (rem id width)]
    ((grid row) col)))

(defn str->maze-grid [maze-str]
      (json/read-str maze-str))

(defn atomise-maze [maze]
      (vec (for [row maze]
                (vec (for [cell row]
                          (Cell. (cell "north")
                                 (cell "south")
                                 (cell "east")
                                 (cell "west")
                                 (atom (cell "links"))
                                 (cell "id")))))))

(defn cells-linked? [c1 c2]
      (and (.contains @(:links c1) (:id c2))
           (.contains @(:links c2) (:id c1))))

(defn within-bounds [row col grid]
      (and (< -1 row)
           (< row (count grid))
           (< -1 col)
           (< col (count (grid row)))))

(defn coords->cell [grid coords]
      (let [row (coords 0) col (coords 1)]
           (when (within-bounds row col grid)
                 ((grid row) col))))

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

(defn distance [start end maze]
  (loop [distances {(:id start) 0}
         frontier [(:id start)]]
    (let [turn (vec
                 (for [cell frontier]
                   (remove nil? (for [link @(:links (id->cell cell maze))]
                                  (when (not (contains? distances link))
                                    {:distance     {link (inc (distances cell))},
                                     :new-frontier link})))))
          clean-turn (reduce #(apply conj %1 %2) [] turn)]
      (if (contains? distances (:id end))
        distances
        (recur
          (conj distances (apply merge (reduce #(conj %1 (:distance %2)) [] clean-turn)))
          (reduce #(conj %1 (:new-frontier %2)) [] clean-turn))))))

;returns path as id-distance pairs in order of start to end
(defn path [start end maze]
  (let [distances (distance start end maze)]
    (loop [current end path {(:id end), (distances (:id end))}]
      (if (= (:id current) (:id start))
        (sort-by #(second %) path)
        (let [linked-cells @(:links current)
              link-dists (map #(hash-map % (distances %)) linked-cells)
              cleaned-links (remove #(nil? (apply second %)) link-dists)
              smaller-links (filter #(< (distances (apply first %)) (distances (:id current))) cleaned-links)
              next-cell (first smaller-links)
              ]
          (recur (id->cell (apply first next-cell) maze) (conj path next-cell)))))))

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

(defrecord Maze [grid start-end rows columns])

(def length 25)
(def btn-length 50)
(defrecord Button [hidden selected x y text])

(defn setup []
  (println (:body (client/get "http://127.0.0.1:3000/")))
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {:x-pos 25
   :y-pos 25
   :current-cell -1
   :path []
   :maze []
   :point -1
   :buttons {:5         (Button. false false  400 50 "5x5")
             :10        (Button. false false  400 110 "10x10")
             :15        (Button. false false  400 170 "15x15")
             :make-path (Button. true  false  400 250 "Make Path")}})

(defn update-maze? [state]
  (let [buttons (:buttons state)
        five    (:5 buttons)
        ten     (:10 buttons)
        fifteen (:15 buttons)]
    (or (:selected five) (:selected ten) (:selected fifteen))))

(defn update-maze [state]
  (let [cell-id (inc (:current-cell state))
        maze (:maze state)
        maze-size    (* (count (:grid (:maze state))) (count (:grid (:maze state))))
        col     (mod cell-id (:rows maze))
        row     (quot cell-id (:columns maze))
        x-pos   (* 25 (inc col))
        y-pos   (* 25 (inc row))
        buttons (if (< maze-size cell-id)
                  (assoc (state :buttons) :make-path  (assoc (:make-path (:buttons state)) :hidden false)
                                          :5  (assoc (:5 (:buttons state)) :selected false)
                                          :10  (assoc (:5 (:buttons state)) :selected false)
                                          :15  (assoc (:5 (:buttons state)) :selected false))
                  (:buttons state))]
    (assoc state
     :x-pos x-pos
     :y-pos y-pos
     :current-cell cell-id
     :buttons buttons)))

(defn update-path? [state]
  (let [buttons (:buttons state)
        path    (:make-path buttons)]
    (:selected path)))

(defn update-path [state]
  (let [update-path? (< (:point state) (count (:path state)))
        point (if update-path?
                (inc (:point state))
                (:point state))
        buttons (if update-path?
                  (:buttons state)
                  (assoc (:buttons state) :make-path (assoc (:make-path (:buttons state)) :selected false)))]
    (assoc state :point point :buttons buttons)))

(defn update-state [state]
  (let
    [update-maze? (update-maze? state)
     update-path? (update-path? state)]
    ;(println "update-path" update-path?)
    (cond
      update-maze? (update-maze state)
      update-path? (update-path state)
      :else
        state)))

(defn get-maze [btn]
  (println "getting maze")
  (let [size (Integer. (re-find (re-matcher #"\d+" (:text btn))))
        maze-str (:body (client/get (str "http://127.0.0.1:3000/" size)))
        maze (atomise-maze (str->maze-grid maze-str))]
    (Maze. maze (def-start-end (count maze) (count maze)) (count maze) (count maze))))


(defn get-path [maze]
  (println "getting path")
  (let [start (id->cell ((:start-end maze) 0) maze)
        end (id->cell ((:start-end maze) 1) maze)]
    (vec (path start end maze))))


(get-path maze)

(defn btn-clicked [btn state]
  (let [def-btns  {:5         (Button. true   false   400 50  "5x5")
                   :10        (Button. true   false   400 110 "10x10")
                   :15        (Button. true   false   400 170 "15x15")
                   :make-path (Button. true   false   400 250 "Make Path")}
        maze (if
               (= (:text btn) "Make Path")
               []
               (get-maze btn))
        path (if (= [] maze)
               []
                 (get-path maze))]
   (cond
    (= (:text btn) "Make Path") (assoc state :buttons (assoc def-btns :make-path (assoc (:make-path def-btns) :selected true :hidden true)))
    (= (:text btn) "5x5")   (assoc state :buttons (assoc def-btns :5 (assoc (:5 def-btns) :selected true))
                                         :maze maze :path path)
    (= (:text btn) "10x10") (assoc state :buttons (assoc def-btns :10 (assoc (:10 def-btns) :selected true))
                                         :maze maze :path path)
    (= (:text btn) "15x15") (assoc state :buttons (assoc def-btns :15 (assoc (:15 def-btns) :selected true))
                                         :maze maze :path path)
    :else
      state)))

(defn btn-clicked? [btn event]
  (let [x  (:x event)
        y  (:y event)
        left (:x btn) ;left-right boundaries
        right (+ btn-length (:x btn))
        top (:y btn)
        bottom (+ btn-length (:y btn))];top-bottom boundaries
    (and (< left x) (< x right) (< top y) (< y bottom))))

(defn mouse-clicked [state event]
  (let [five      (:5 (:buttons state))
        ten       (:10 (:buttons state))
        fifteen   (:15 (:buttons state))
        make-path (:make-path (:buttons state))]
    (cond
      (btn-clicked? five event) (btn-clicked five state)
      (btn-clicked? ten event) (btn-clicked ten state)
      (btn-clicked? fifteen event) (btn-clicked fifteen state)
      (btn-clicked? make-path event) (btn-clicked make-path state)
      :else
         state)))

(defn draw-point [state]
  (let [point ((:path state) (:point state))
        maze (:maze state)
        path (:path state)
        id  (point 0)
        dist (point 1)
        path-size (dec (count path))
        ;start? (= id ((first path) 0))
        ;end? (= id ((last path) 0))
        red (+ (* dist (/ (- 255 100) path-size)) 100)
        green (+ (* dist (/ (- 200 255) path-size)) 255)
        blue (+ (* dist (/ (- 200 100) path-size)) 100)
        row (quot id (:rows maze))
        col (mod id (:columns maze))
        x (- (* length (inc col)) (/ length 2))
        y (- (* length (inc row)) (/ length 2))
        width 15
        height 15]
    (q/fill red green blue)
    (q/ellipse x y width height)
    (q/fill 255)))

(defn hide-btn [button]
  (q/fill 205)
  (q/stroke 205)
  (q/rect (:x button) (:y button) btn-length btn-length)
  (q/stroke 0))

(defn draw-btn [button]
  (q/fill 255)
  (q/rect (:x button) (:y button) btn-length btn-length)
  (q/fill 0)
  (if (= (:text button) "Make Path")
    (q/text (:text button) (+ 10 (:x button)) (+ 10 (:y button)) (/ btn-length 1.5) btn-length)
    (q/text (:text button) (+ 10 (:x button)) (+ (/ btn-length 2) (:y button)))))

(defn draw-btns [state]
  (let [five      (:5 (:buttons state))
        ten       (:10 (:buttons state))
        fifteen   (:15 (:buttons state))
        make-path (:make-path (:buttons state))]
    (if (:hidden five)
      (hide-btn five)
      (draw-btn five))
    (if (:hidden ten)
      (hide-btn ten)
      (draw-btn ten))
    (if (:hidden fifteen)
      (hide-btn fifteen)
      (draw-btn fifteen))
    (if (:hidden make-path)
      (hide-btn make-path)
      (draw-btn make-path))))

(defn draw [state]
  (let [x (:x-pos state)
        y (:y-pos state)
        tl [(- x length) (- y length)]
        tr [x (- y length)]
        bl [(- x length) y]
        br [x y]
        maze (:maze state)
        maze-size  (* (count (:grid (:maze state))) (count (:grid (:maze state))))
        draw-maze? (and (update-maze? state) (< (:current-cell state)  maze-size))
        draw-path? (and (update-path? state) (< (:point state) (count (:path state))))
        cell (if draw-maze?
               (id->cell (:current-cell state) maze)
               nil)]
    (draw-btns state)
    (when draw-maze?
      (when
        (not (cells-linked? cell (coords->cell (:grid maze) (:north cell))))
        (q/line tl tr)) ;top
      (when
        (not (cells-linked? cell (coords->cell (:grid maze) (:east cell))))
        (q/line tr br));right
      (when (= 0 (mod (:id cell) (:columns maze)))
        (q/line bl tl));left
      (when (= (dec (:rows maze)) (quot (:id cell) (:rows maze)))
        (q/line br bl))
      )
    (when (and (not draw-maze?) draw-path?)
      (draw-point state))))

(q/defsketch quilproject
             :title "You spin my circle right round"
             :size [500 500]
             ; setup function called only once, during sketch initialization.
             :setup setup
             ; update-state is called on each iteration before draw-state.
             :update update-state
             :mouse-clicked mouse-clicked
             :draw draw
             :features [:keep-on-top]
             ; This sketch uses functional-mode middleware.
             ; Check quil wiki for more info about middlewares and particularly
             ; fun-mode.
             :middleware [m/fun-mode])

