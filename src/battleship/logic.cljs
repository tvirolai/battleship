(ns battleship.logic
  (:require [clojure.set :as s]))

(defrecord Cell [x y clicked? ship?])

(defrecord Point [x y])

(defn make-grid []
  "This initializes an empty grid. The grid is sized 10 x 10. Each cell stores
  its coordinates, whether it has been clicked or not and if it contains a ship."
  (vec (for [y (range 10)]
    (vec (for [x (range 10)]
      (->Cell x y false false))))))

(defn get-cell [grid {:keys [x y]}]
  (nth (nth grid y) x))

(def ships
  (concat
    (repeat 1 4) ;; One aircraft carrier
    (repeat 2 3) ;; 2 cruisers
    (repeat 3 2) ;; 3 destroyers
    (repeat 4 1))) ;; 4 submarines

(defn point-in-grid? [{:keys [x y]}]
  (->> [x y]
    (map #(<= 0 % 9))
    (every? true?)))

(defn get-neighboring-points [{:keys [x y]}]
  (filter point-in-grid?
    (for [newx (range (dec x) (+ 2 x))
          newy (range (dec y) (+ 2 y))
          :when (or (not= newx x)
                    (not= newy y))]
      (->Point newx newy))))

(defn get-directly-neighboring-points
  "Takes a point and returns the points that surround it vertically and horizontally,
  not diagonally. The returned points are guaranteed to be in the grid, since this
  is checked in `get-neighboring-points`"
  [p]
  (filter (fn [{:keys [x y]}]
            (or (= (:x p) x) (= (:y p) y)))
          (get-neighboring-points p)))

(defn get-random-point []
  (->Point (rand-int 10) (rand-int 10)))

(defn get-new-ship-coords
  "Returns a sequence of coordinates representing the ship. Takes a starting point,
  the size of the ship and direction, which is either true (= horizontal) or false (= vertical)."
  [{:keys [x y]} size horizontal?]
  (if horizontal?
    (mapv (fn [x] (->Point x y)) (range x (+ x size)))
    (mapv (fn [y] (->Point x y)) (range y (+ y size)))))

(defn ship-fits-grid?
  "Takes a grid and a ship (a sequence of points), returns true or false.
  Checks that the ship fits the grid and that it doesn't touch another ship."
  [grid ship]
  (if (empty? ship)
    true
    (let [curr (first ship)
          points (conj (get-neighboring-points curr) curr)]
      (if-not (every? true? (map point-in-grid? points))
        false
        (let [cells (map (partial get-cell grid) points)
              point-ok? (->> cells (map :ship?) (every? false?))]
          (if-not point-ok?
            false
            (recur grid (rest ship))))))))

(defn insert-ship
  "Takes a grid and a ship, inserts the ship without performing any checks."
  [grid ship]
  (if (empty? ship)
    grid
    (let [{:keys [x y]} (first ship)]
      (recur (update-in grid [y x] assoc :ship? true)
             (rest ship)))))

(defn add-ship
  "Takes a grid and a size of a ship, returns a new grid with the ship entered
  (i.e. :ship? set to true for the given squares).
  The logic: select a random point in the grid and see if the ship fits in it
  horizontally (to the right from this point), then vertically (downwards).
  If not, recur to a new point."
  [grid size]
  (let [p (get-random-point)
        horiz-ship (get-new-ship-coords p size true)
        horiz-fits? (ship-fits-grid? grid horiz-ship)
        vert-ship (get-new-ship-coords p size false)
        vert-fits? (ship-fits-grid? grid vert-ship)]
    (cond
      (and horiz-fits? vert-fits?) (insert-ship grid (rand-nth [horiz-ship vert-ship]))
      horiz-fits? (insert-ship grid horiz-ship)
      vert-fits? (insert-ship grid vert-ship)
      :default (recur grid size))))

(defn every-ship-sunk?
  "Check if a player has lost."
  [grid]
  (->> grid
       flatten
       (filter :ship?)
       (map :clicked?)
       (every? true?)))

(defn click [grid {:keys [x y] :as p}]
  (if (:clicked? (get-cell grid p))
    grid
    (update-in grid [y x] assoc :clicked? true)))

(defn is-definitely-not-a-ship?
  "There can't be ships in squares diagonally opposite a square that contains
  a ship, since ships can't touch each other from corners. It is thus stupid
  for the machine opponent to try these points. This predicate function can
  be used to rule out these from the candidates for clicking."
  [grid {:keys [x y] :as p}]
  (let [directly-neighboring (get-directly-neighboring-points p)
        diagonally-opposing (-> p
                                get-neighboring-points
                                set
                                (s/difference (set directly-neighboring)))]
    (->> diagonally-opposing
         (map (partial get-cell grid))
         (some #(and (:clicked? %)
                     (:ship? %)))
         boolean)))

(defn computer-clicks
  "The opponent (i.e. 'computer') clicks on its turn. Returns the point on which it wants to click."
  [grid]
  (let [clicked (mapcat (fn [row] (filterv #(true? (:clicked? %)) row)) grid)
        next-to-hits (->> clicked
                          (filter #(true? (:ship? %)))
                          (mapcat get-directly-neighboring-points)
                          distinct
                          (map (partial get-cell grid))
                          (remove (partial is-definitely-not-a-ship? grid))
                          (filter #(false? (:clicked? %))))]
    (if (seq next-to-hits)
      (rand-nth next-to-hits)
      (rand-nth (mapcat (fn [row] (filterv #(false? (:clicked? %)) row)) grid)))))

(defn initialize-starting-situation!
  "Does what is says: initializes a grid and sets the ships randomly into it."
  []
  (loop [g (make-grid)
         s ships]
    (if (empty? s)
      g
      (recur (add-ship g (first s))
             (rest s)))))
