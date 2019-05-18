(ns battleship.core
  (:require [battleship.logic :as l]
            [reagent.core :as r]))

(def state
  (r/atom {:player {}
           :enemy {}
           :situation "Aloita peli klikkaamalla vasemmanpuolesta ruudukkoa.
                      Vasemmalla näkyy sinun pelilappusi."
           :winner ""}))

;; ----
;; UI Components

(defn click-handler [{:keys [x y clicked? ship?] :as p}]
  (let [enemy-state (:enemy @state)
        cell-to-click (l/get-cell enemy-state p)]
    (swap! state assoc :enemy (l/click (:enemy @state) p))
    (if (l/every-ship-sunk? (:enemy @state))
      (swap! state assoc :winner :player)
      (when (false? (:ship? cell-to-click))
        (let [player-state (:player @state)]
          (swap! state assoc :player (->> player-state
                                          l/computer-clicks
                                          (l/click player-state)))
          (when (l/every-ship-sunk? (:player @state))
            (swap! state assoc :winner :enemy)))))))

(defn square
  [{:keys [x y clicked? ship?] :as p} enemy?]
   (let [c (str "square is-vertical-center square--big"
                (when (and ship? (or (false? enemy?) clicked?)) " has-background-primary"))]
     ^{:key (str "key-" (gensym))}
     [:div {:class c
            :on-click #(click-handler p)}
      (when clicked? [:div {:class "delete is-medium centered"}])]))

(defn row [enemy? r]
  ^{:key (str "row-" (gensym))}
  [:div.row.horizontal-flex
   (for [i r]
     (square i enemy?))])

(defn draw-grid [grid enemy?]
  (take 10 (mapv (partial row enemy?) grid)))

;; -------------------------
;; Views

(defn home-page []
  [:div
   [:h2.title.is-2 "Laivanupotus"]
   [:div.tile
    [:div.content.tile.is-vertical (draw-grid (:player @state) false)]
    [:div.content (draw-grid (:enemy @state) true)]]
   [:footer {:class "footer"}
    [:div {:class "content has-text-centered"}
     (let [{:keys [winner]} @state]
       (if (string? winner)
         (:situation @state)
         [:p "Peli ohi! Voittaja: "
          (if (= :player winner)
            [:span {:class "tag is-success"} "SINÄ!"]
            [:span {:class "tag is-danger"} "TIETOKONE :("])]))]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (swap! state assoc :player (l/initialize-starting-situation!)
         :enemy (l/initialize-starting-situation!))
  (mount-root))
