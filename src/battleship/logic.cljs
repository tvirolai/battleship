(ns battleship.logic)

(defrecord Cell [x y clicked? ship?])

(defrecord Point [x y])

;; This initializes an empty grid. The grid is sized 10 x 10. Each cell stores
;; its coordinates, whether is has been clicked or not and if it contains a ship.

(defn make-grid []
  (vec (for [y (range 10)]
    (vec (for [x (range 10)]
      (Cell. x y false false))))))

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
      (Point. newx newy))))

(defn get-random-point []
  (Point. (rand-int 10) (rand-int 10)))

(defn get-new-ship-coords
  "Returns a sequence of coordinates representing the ship. Takes a starting point,
  the size of the ship and direction, which is either :horizontal or :vertical."
  [{:keys [x y]} size horizontal?]
  (if horizontal?
    (mapv (fn [x] (Point. x y)) (range x (+ x size)))
    (mapv (fn [y] (Point. x y)) (range y (+ y size)))))

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
  (i.e. :ship? set to true for the gived squares).
  The logic: select a random point in the grid and see if the ship fits in it
  horizontally (to the right from this point), then vertically (downwards).
  If not, recur to a new point."
  [grid size]
  (let [p (get-random-point)
        horiz-ship (get-new-ship-coords p size true)
        vert-ship (get-new-ship-coords p size false)]
    (cond
      (ship-fits-grid? grid horiz-ship) (insert-ship grid horiz-ship)
      (ship-fits-grid? grid vert-ship) (insert-ship grid vert-ship)
      :default (recur grid size))))

(defn initialize-starting-situation!
  "Does what is says: initializes a grid and sets the ships randomly into it."
  []
  (loop [g (make-grid)
         s ships]
    (if (empty? s)
      g
      (recur (add-ship g (first s))
             (rest s)))))
