; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/azelusMK4.scad in openScad

(ns scad-keyboards.azelusMK4
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))



(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(def base-variant {:cutout-dimensions {:x 14 :y 14}
                   :keycap-kerf 1
                   :keycap-dimensions {:x 18 :y 18 :z 11}
                   :key-distance {:x 19 :y 19}
                   :plate-thickness 1.5
                   :plate-border 4
                   :case-thickness 3

                   :screwhole-radius 0.6
                   :screwhole-positions [[4 2.8] [4 -0.8] [1.2 4.4]]
                   :strut-positions [[4 2.9] [4 -0.9]]
                   :thumb-screwhole-positions [[3.8 0] [-0.75 -0.15]]
                   :thumb-strut-positions [[3.8 0] [-0.75 -0.2]]

                   :fingers {:row-number 3
                             :col-number 6
                             ; x, y, z
                             :offset [40 0 5]
                             :staggers [8 12 20 15 3 0]
                             :angles {:opening (deg2rad 18) ; like a door
                                      :gable (deg2rad 10)  ; like a key 
                                      :slope (deg2rad 0)}  ; like a laptop monitor
                             :excluded-grid-positions #{}
                             :additional-grid-positions #{}}

                   :thumbs {:row-number 1
                            :col-number 3
                            :offset [10 -30 -15]
                            :staggers [0 0 0 4]
                            :angles {:opening (deg2rad 26)
                                     :gable (deg2rad -14)
                                     :slope (deg2rad 12)}
                            :excluded-grid-positions #{}
                            :additional-grid-positions #{}}})

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(def variant2 (deep-merge base-variant
                          {:fingers {:excluded-grid-positions #{[5 0]}}
                           :thumbs {:col-number 4}}))

(defn place-shape-at [config
                      shape
                      [row col]]
  (let [{staggers :staggers
         offsets :offset
         {opening :opening gable :gable slope :slope} :angles
         {dist-x :x} :key-distance
         {dist-y :y} :key-distance} config]
    #_(clojure.pprint/pprint config)
    (->> shape
         (translate [(* row dist-x) 
                     (+ (* col dist-y) 
                        (get staggers row 0))
                     0])
         (translate offsets)
         (rotate slope [1 0 0])
         (rotate gable [0 1 0])
         (rotate opening [0 0 1])
         )))

(defn place-shape [config shape]
  (let [{row-number :row-number
         col-number :col-number
         excluded :excluded-grid-positions
         additional :additional-grid-positions} config]
    (concat
     (->> (for [col-idx (range col-number)
                row-idx (range row-number)]
            [col-idx row-idx])
          (remove excluded)
          (concat additional)
          (map (partial place-shape-at config shape))))))

(defn place-at-key-positions [config shape]
  (let [{finger-config :fingers
         thumb-config :thumbs} config] 
    ; flatten config structure, i.e. make finger/thumb config top-level attributes .
    (concat (place-shape (merge config finger-config) shape)
            (place-shape (merge config thumb-config) shape))))

(defn keycap [cap-x cap-y cap-z]
  (color [0.3 0.3 0.3 1]
         (translate [0 0 5]
                    (hull (cube cap-x 
                                cap-y 
                                2)
                          (translate [0 1.5 (- cap-z 2)] 
                                     (cube (* cap-x 0.7) 
                                           (* cap-y 0.7) 
                                           2)))
                    )))

(defn average [& vals]
  (/ (reduce + vals) (count vals)))

(defn case-shape [border height]
  ;; produce high quality mesh
  (binding [scad-clj.model/*fa* 2
            scad-clj.model/*fn* 10  ;; low 10 (instantaneous), medium 30 (3mins), high 50 (??? mins)
            scad-clj.model/*fs* 0.1]
    (let [radius (/ border 2)]
      (->> (sphere radius)
           (union (translate [0 0 (/ height -2)]
                             (cylinder radius height)))))))

(defn difference-last [subtracted shape]
  (difference shape subtracted))

(defn kb-case-raw [config plate-border keycap-kerf]
  (let [{{cap-x :x} :keycap-dimensions
         {cap-y :y} :keycap-dimensions} config
        height 30]
    (->> (case-shape plate-border height)
         (minkowski (cube (+ plate-border cap-x) (+ plate-border cap-y) 1))
         (place-at-key-positions config)
         (union)
         (hull)
         (difference-last (place-at-key-positions config
                                                  (translate [0 0 (/ height 2)]
                                                             (cube (+ cap-x (* 2 keycap-kerf))
                                                                   (+ cap-y (* 2 keycap-kerf))
                                                                   height))))
         )))

(defn kb-case [config]
  (let [{plate-border :plate-border
         case-thickness :case-thickness
         kerf :keycap-kerf
         {cap-x :x} :keycap-dimensions
         {cap-y :y} :keycap-dimensions
         {cut-x :x} :cutout-dimensions
         {cut-y :y} :cutout-dimensions} config
        height 30]
    (-> (kb-case-raw config plate-border kerf)
        (difference (translate [0 0 (- case-thickness)] 
                               (kb-case-raw config 
                                            (- plate-border case-thickness)
                                            (+ kerf case-thickness))))
        (difference (place-at-key-positions config
                     (cube  cut-x
                            cut-y
                            height)))
        (difference (translate [0 0 -80] (cube 500 500 100)))))
  )

(defn mirror-halves [shape]
  (union shape
         (mirror [1 0 0] shape)))

(defn create-model [config]
  (let [{plate-thickness :plate-thickness
         {cap-x :x} :keycap-dimensions
         {cap-y :y} :keycap-dimensions
         {cap-z :z} :keycap-dimensions
         border :plate-border} config
        explode 0
        keycaps (place-at-key-positions config (keycap cap-x cap-y cap-z))]
    (union
     (mirror-halves (union
                     (translate [0 0 (* 5 explode)] keycaps)
                     (translate [0 0 (* plate-thickness explode)] (kb-case config)))))
    #_(plate-layer-upper)
    #_(translate [0 0 (- (* 1 plate-thickness explode))] (plate-layer-lower))
    #_(translate [0 0 (- (* 2 plate-thickness explode))] (frame-layer))
    #_(translate [0 0 (- (* 3 plate-thickness explode))] (bottom-layer))
    #_(->> (cube 200 200 5)
         (rotate (/ PI 4) [0 0 1])
         (translate [0 45 (- -24 (* 4 plate-thickness explode))])
         (color [0.8 0.8 0.8 0.5]))
    ))

(defn create-multi-model []
  (union (create-model base-variant)
         #_(translate [0 150 0] (create-model variant2))
         ))

(defn run [] 
  (spit "things/azelusMK4.scad"
        (write-scad (create-multi-model)))
  )

(run)
