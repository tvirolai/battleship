(ns battleship.core
  (:require [reagent.core :as r]))

(def state
  (r/atom {:player {}
           :enemy {}}))

;; ----
;; UI Components

(defn square
  ([] (square :normal false))
  ([size] (square size false))
  ([size filled]
   (let [c (str "square"
                (if (= :normal size)
                  ""
                  (str " square--" (name size)))
                (when filled " has-background-grey-light"))]
     ^{:key (str "key-" (gensym))} [:div {:class c}])))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Laivanupotus"]
   (for [i (range 10)]
     (square :normal))])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
