(ns battleship.core
  (:require [battleship.logic :as l]
            [reagent.core :as r]))

(defonce state
  (r/atom {:player {}
           :enemy {}}))

;; ----
;; UI Components

(defn square
  ([] (square :normal false))
  ([size] (square size false))
  ([size filled]
   (let [c (str "square is-vertical-center"
                (if (= :normal size)
                  ""
                  (str " square--" (name size)))
                (when filled " has-background-grey-light"))]
     ^{:key (str "key-" (gensym))} [:div {:class c} [:div {:class "delete centered"}]])))

(defn row [r]
  ^{:key (str "row-" (gensym))}
  [:div.row.horizontal-flex
   (for [i r]
     (if (:ship? i)
       (square :big true)
       (square :big false)))])

(defn draw-grid [grid]
  (take 10 (mapv row grid)))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Laivanupotus"]
   [:div.content (draw-grid (:player @state))]
   [:div.content (draw-grid (:enemy @state))]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (swap! state assoc :player (l/initialize-starting-situation!)
         :enemy (l/initialize-starting-situation!))
  (mount-root))
