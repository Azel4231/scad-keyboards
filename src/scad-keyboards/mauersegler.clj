; check if nrepl version in .lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/left.scad in openScad

(ns mauersegler
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [translate rotate mirror cube sphere cylinder difference union minkowski hull color]]))


(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(def base-variant {:cutout-dimensions {:x 14 :y 14}
                   :keycap-kerf 1
                   :keycap-dimensions {:x 18 :y 18 :z 11}
                   :key-distance {:x 20 :y 20}
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
                             :offset [110 0 5]
                             :staggers [{:y 6 :z -5} 
                                        {:y 9 :z -5}
                                        {:y 15 :z -8}
                                        {:y 10 :z -6}
                                        {:y 2 :z -2}
                                        {:y 0 :z -2}]
                             :angles {:opening (deg2rad 18) ; like a door
                                      :gable (deg2rad 10)  ; like a key 
                                      :slope (deg2rad 0)}  ; like a laptop monitor
                             :curvature {:row (deg2rad -6) :col (deg2rad -24)
                                         :row-offset (deg2rad 35) :col-offset (deg2rad 35)}
                             :excluded-grid-positions #{}
                             :additional-grid-positions #{}}

                   :thumbs {:row-number 1
                            :col-number 3
                            :offset [-50 -30 -5]
                            :staggers [{:y 0 :z 0}
                                       {:y 0 :z 0}
                                       {:y 0 :z 0}]
                            :angles {:opening (deg2rad 26)
                                     :gable (deg2rad -14)
                                     :slope (deg2rad 12)}
                            :curvature {:row (deg2rad -28) :col (deg2rad -6)
                                        :row-offset (deg2rad -38) :col-offset (deg2rad 0)}
                            :excluded-grid-positions #{}
                            :additional-grid-positions #{[1 -1]}}})

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(def variant2 (deep-merge base-variant
                          {:fingers {:excluded-grid-positions #{[5 0]}}
                           :thumbs {:col-number 4}}))

(defn place-shape-at [config
                      shape
                      [col row]]
  (let [{staggers :staggers
         offsets :offset
         {dist-x :x 
          dist-y :y} :key-distance
         {curve-row :row
          curve-row-offset :row-offset
          curve-col :col
          curve-col-offset :col-offset} :curvature} config
        {stagger-y :y stagger-z :z}  (get staggers col)
        col-radius (- (/ dist-y (Math/sin curve-col)))
        row-radius (- (/ dist-x (Math/sin curve-row)))]
    #_(clojure.pprint/pprint config)
    #_(println "col-radius " col-radius ", stagger-y " stagger-y ", stagger-z " stagger-z)
    (->> shape
         (translate [0 0 (- col-radius)])
         (rotate (+ (* curve-col row) curve-col-offset) [1 0 0])
         (translate [0 0 col-radius])

         (translate [0 0 (- row-radius)])
         (rotate (+ (* curve-row col) curve-row-offset) [0 1 0])
         (translate [0 stagger-y (+ row-radius stagger-z)])

         #_(translate [(* row dist-x) 
                     (+ (* col dist-y) 
                        (get staggers row 0))
                     0])
         (translate offsets)
         #_(rotate slope [1 0 0])
         #_(rotate gable [0 1 0])
         #_(rotate opening [0 0 1])
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
  (let [{{cap-x :x
          cap-y :y} :keycap-dimensions} config
        height 30]
    (->> (case-shape plate-border 10)
         (minkowski (cube (+ plate-border cap-x) (+ plate-border cap-y) 1))
         (place-at-key-positions config)
         (union)
         #_(hull)
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
         {cut-x :x
          cut-y :y} :cutout-dimensions} config
        height 20]
    (-> (kb-case-raw config plate-border kerf)
        (difference (translate [0 0 (- case-thickness)] 
                               (kb-case-raw config 
                                            (- plate-border case-thickness)
                                            (+ kerf case-thickness))))
        (difference (place-at-key-positions config
                                            (translate [0 0 (/ height 4)]
                                                       (cube  cut-x
                                                              cut-y
                                                              height))))
        (difference (translate [0 0 -50] (cube 500 500 100)))
        )))

(defn mirror-halves [shape]
  shape
  #_(union shape
         (mirror [1 0 0] shape)))

(defn create-model [config]
  (let [{plate-thickness :plate-thickness
         {cap-x :x 
          cap-y :y 
          cap-z :z} :keycap-dimensions} config
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
  (spit "things/mauersegler/mauersegler.scad"
        (write-scad (create-multi-model)))
  )

(run)
