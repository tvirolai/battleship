(ns battleship.core
  (:require [battleship.logic :as l]
            [reagent.core :as r]))

(def state
  (r/atom {:player {}
           :enemy {}
           :description "Aloita peli klikkaamalla vasemmanpuolesta ruudukkoa.
                        Vasemmalla näkyy sinun pelilappusi."
           :winner false}))

(defn reset-state! []
  (swap! state assoc :player (l/initialize-starting-situation!)
         :enemy (l/initialize-starting-situation!)
         :winner false))

;; ----
;; UI Components

(defn click-handler [{:keys [x y clicked? ship?] :as p}]
  (let [cell-to-click (-> @state :enemy (l/get-cell p))]
    (swap! state assoc :enemy (l/click (:enemy @state) p))
    (if (l/every-ship-sunk? (:enemy @state))
      (swap! state assoc :winner :player)
      (when-not (:ship? cell-to-click)
        ;; The computer gets its turn when the player's missile misses (and vice versa).
        (loop [player-state (:player @state)
               next-cell (->> player-state
                              l/computer-clicks
                              (l/get-cell player-state))]
          (let [next-state (l/click player-state next-cell)
                _ (swap! state assoc :player next-state)]
            (if (l/every-ship-sunk? next-state)
              (swap! state assoc :winner :enemy)
              (if-not (:ship? next-cell)
                nil
                (recur next-state
                       (->> next-state
                            l/computer-clicks
                            (l/get-cell next-state)))))))))))

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
       (if-not winner
         [:p (:description @state)]
         [:div
          [:p "Peli ohi! Voittaja: "]
          (if (= :player winner)
            [:span {:class "tag is-success"} "SINÄ!"]
            [:span {:class "tag is-danger"} "TIETOKONE :("])
          [:br]
          [:br]
          [:button {:on-click #(reset-state!)
                    :class "button is-info"} "Uusi erä?"]]))]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (reset-state!)
  (mount-root))
