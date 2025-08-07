; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/azelusMK4.scad in openScad

(ns scad-keyboards.invader-box
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))



(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))


(def config {:wood-thickness 2
             :card {:x 63.5 :y 88 :sleeve-extra 2}
             :invader-card {:x 44 :y 68 :sleeve-extra 1}
             :fear-token {:x 12 :y 20 :z 5}
             :fear-deck 20
             :event-deck 30
             :blight-deck 10})


(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn bottom [config]
  (let [{wood-thickness :wood-thickness
         {card-x :x} :card
         {card-y :y} :card
         {card-sleeve :sleeve-extra} :card} config]
    (translate [0 0 0]
               (cube (-> card-x (+ card-sleeve) (* 4))
                     (-> card-y (+ card-sleeve))
                     wood-thickness))))

(defn fear-token [config]
  (let [{{fear-x :x} :fear-token
         {fear-y :y} :fear-token
         {fear-z :z} :fear-token} config
        fx (/ fear-x 2)
        fy (/ fear-y 2)
        fz (/ fear-z 2)]
    (color [0.5 0.5 0.5 1]
           (extrude-linear {:height fz}
                           (polygon (map (fn [[x y]] [(* x fx) (* y fy)])
                                         [[1 1] [0.66,0.75] [0.5,0.8] [0.33,0.75] [0,1]
                                          [0.25, 0.4]
                                          [0 0] [0.5 0.2] [1 0]
                                          [0.75 0.4]
                                          ]))))))



(defn cards [config]
  (let [{wood-thickness :wood-thickness
         {card-x :x} :card
         {card-y :y} :card
         {card-sleeve :sleeve-extra} :card
         blight-deck :blight-deck
         event-deck :event-deck
         fear-deck :fear-deck} config
        card-x (+ card-x card-sleeve)
        card-y (+ card-y card-sleeve)]
    (union
     (translate [(- (+ card-x wood-thickness)) 0 (/ blight-deck 2)]
                (cube (-> card-x)
                      (-> card-y)
                      blight-deck))
     #_Event
     (translate [0 0 (/ event-deck 2)]
                (cube (-> card-x)
                      (-> card-y)
                      event-deck))
     #_Fear
     (translate [(+ card-x wood-thickness) 0 (/ fear-deck 2)]
                (cube (-> card-x)
                      (-> card-y)
                      fear-deck)))))



(defn create-model [config]
  (let [{wood-thickness :wood-thickness
         {card-x :x} :card
         {card-y :y} :card
         {card-sleev :sleeve-extra} :card} config]
    (union
     (bottom config)
     (cards config)
     (fear-token config))))

(defn run []
  (spit "things/InvaderBox.scad"
        (write-scad (create-model config))))

(run)
