; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/redpoll.scad in openScad

(ns redpoll-split
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [translate rotate intersection minkowski union hull difference cube color mirror cylinder project]]))

(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(def redpoll {:keycap-dimensions {:x 18 :y 18 :z 11}
              :cutout-dimensions {:x 13.96 :y 13.96}
              :key-distance {:x 19.0 :y 19.0}
              :keycap-kerf 0.75
              :mirror false

              :opening-angle (deg2rad 15)
              :plate-thickness 1.5
              :layer-thickness 2
              :plate-border 2
              ;; :plate-mirror-edge {:min 22 :max 101}  ;; Obere Kante gerade weiter führen
              :plate-mirror-edge {:min 22 :max 95.5}  ;; eine Linie mit anderer Hälfte
              ;; :plate-mirror-edge {:min 22 :max 84}  ;; parallel zur Unterkante
              :controller-position {:x 16 :y 65 :z 0}
              :controller-dimensions {:x 18 :y 23 :z 1.5} ;; Seeed XIAO BLE
              ;; :controller-dimensions {:x 18 :y 33.5 :z 1.5}  ;; Nice!Nano
              :battery-position {:x 18 :y 9 :z 3}
              :battery-dimensions {:x 20 :y 35 :z 6.5}
              ;; {:x 20 :y 35 :z 6.5}  https://www.mylipo.de/Lipo-Akku-250mAh-37V-25C-50C-GENIUS-CP-WALKERA
              ;; {:x 20 :y 35 :z 5}  https://www.mylipo.de/Lipo-Akku-180mAh-37V-25C-50C
              ;; {:x 11 :y 42 :z 5.8}  https://www.mylipo.de/Lipo-Akku-150mAh-37V-25C-50CJST-PH125-2-Pin-Stecker
              
              :screwhole-radius 0.6
              :screwhole-positions [[4 2.8] [4 -0.8] [1.2 4.4]]
              :screwhole-matrix []
              :strut-positions [[4 2.9] [4 -0.9]]
              
              :key-offset [36 0]
              :matrix {:far-index {:positions [[0 0] [0 1] [0 2]]
                                   :stagger [0 8]}
                       :index {:positions [[1 0] [1 1] [1 2] [1 3]]
                               :stagger [0 10]}
                       :middle {:positions [[2 0] [2 1] [2 2]]
                                :stagger [0 16]}
                       :ring {:positions [[3 0] [3 1] [3 2]]
                              :stagger [0 12]}
                       :pinkey {:positions [[4 0] [4 1] [4 2]]
                                :stagger [0 4]}
                       :far-pinkey {:positions [[5 0] [5 1] [5 2]]
                                    :stagger [0 3]}

                       :thumb {:positions [[-1 -1] [0 -1] [1 -1]]
                               :stagger [0 4]}
                       ;; :far-thump {:positions [[2 -1]] :stagger [0 11]}
                       }
              
              
              ;; a hull part defines the key positions (cols, rows) based on which to create a hull
              ;; all hull parts are then unioned together
              ;; this allows using hull but still have convex shapes without using cutouts
              :hull-parts [[:far-index]
                           [:thumb]
                           [:index :middle :ring :pinkey :far-pinkey :far-thump]]}
                       )

(def config redpoll)

(defn tprint
  ([param]
   (println param)
   param)
  ([msg param]
   (println msg param)
   param))

(defn place-shape [config shape stagger pos]
  (let [{{dist-x :x
          dist-y :y} :key-distance
         angle :opening-angle
         [offset-x offset-y] :key-offset} config
        [row col] pos
        [stagger-x stagger-y] stagger]
    (->> shape
         ;; row, col unit: 1u (one key)
         ;; stagger unit: mm
         (translate [(+ (* row dist-x)
                        offset-x
                        stagger-x) 
                     (+ (* col dist-y) 
                        offset-y
                        stagger-y)
                     0])
         (rotate angle [0 0 1])
         )))

(defn place-shape-at [config shape matrix-def]
  (let [[_ {poss :positions
            stagger :stagger}] matrix-def]
    (map (partial place-shape config shape stagger)
         poss)))

(defn place-at-key-positions [config shape]
  (let [{matrix :matrix} config]
    (->> matrix
         (mapcat (partial place-shape-at config shape))
         )))

(defn single-cutout [config]
  (let [{{cutout-width :x
          cutout-height :y} :cutout-dimensions} config]
    (cube cutout-width cutout-height 25)))

(defn plate-layer [config]
  (let [{matrix :matrix
         hull-parts :hull-parts
         {dist-x :x
          dist-y :y} :cutout-dimensions
         plate-border :plate-border
         plate-thickness :plate-thickness} config
        dummy (cube (+ dist-x (* 0 plate-border))
                    (+ dist-y (* 0 plate-border)) 
                    0.01)]
    (difference
     (minkowski
      (->> hull-parts
           (map (fn [part-list]
                  (->> part-list
                       (select-keys matrix)
                       (map (partial place-shape-at config dummy))
                       (apply concat)
                       (apply hull))))
           (apply union))
      (cylinder (* 1 plate-border) plate-thickness))
     (place-at-key-positions config (single-cutout config)))))

(defn average [& vals]
  (/ (reduce + vals) (count vals)))

(defn base-plate [config]
  (let [{layer-thickness :plate-thickness
         plate-border :plate-border
         {cap-x :x
          cap-y :y} :keycap-dimensions
         {mirror-y-min :min
          mirror-y-max :max} :plate-mirror-edge} config
        mirror-edge-helper (translate [0 (average mirror-y-max mirror-y-min) 0]
                                      (cube 0.1
                                            (- mirror-y-max mirror-y-min)
                                            layer-thickness))]
    (->> (cube (+ cap-x (* 2 plate-border))
               (+ cap-y (* 2 plate-border))
               layer-thickness)
         (place-at-key-positions config)
         (cons mirror-edge-helper)
         (apply hull)
         (color [0.98 0.92 0.6 1]))))

(defn screw-holes [config]
  (let [{screw-poss :screwhole-matrix
         screwhole-radius :screwhole-radius} config
        hole (color [0 0 0 1] (cylinder screwhole-radius 15))]
    (concat (map (partial place-shape-at config hole)
                 screw-poss))))

(defn top-layer [config]
  (let [{{cap-x :x
          cap-y :y} :keycap-dimensions
         kerf :keycap-kerf} config
        base-plate (base-plate config)
        cap-cutouts (place-at-key-positions config (cube (+ cap-x (* 2 kerf))
                                                         (+ cap-y (* 2 kerf))
                                                         25))]
    (difference base-plate
                cap-cutouts)))



(defn keycap [config]
  (let [{{cap-x :x
          cap-y :y
          cap-z :z} :keycap-dimensions} config]
    (color [0.3 0.3 0.3 1]
           (translate [0 0 5]
                      (hull (cube cap-x
                                  cap-y
                                  2)
                            (translate [0 1.5 (- cap-z 2)]
                                       (cube (* cap-x 0.7)
                                             (* cap-y 0.7)
                                             2)))))))
(defn mirror-halves
  ([shape]
   (union shape (mirror [1 0 0] shape)))
  ([mirror? shape]
   (if mirror? (mirror-halves shape) shape)))

(defn frame-layer [config]
  (let [{layer-thickness :layer-thickness
         strut-poss :strut-positions} config
        base-plate (base-plate config)
        cutout-right (apply hull (place-at-key-positions config (single-cutout config)))
        cutout (hull (mirror-halves cutout-right))
        cutout-controller (translate [0 65 0] (cube 28 30 5))
        cutout-switch (translate [18 75 0] (cube 12 8 5))
        cutout-reset (translate [-18 75 0] (cube 10 8 5))
        #_struts-right #_(map (partial place-at-key-positions config (cylinder 7 plate-thickness)) strut-poss)]
    (difference (union
                 #_(map (partial intersection base-plate) struts-right)
                 (difference base-plate
                             (mirror-halves cutout)
                             cutout-controller
                             cutout-switch
                             cutout-reset))
                (screw-holes config))))

(defn create-model [config]
  (let [{plate-thickness :plate-thickness
         layer-thickness :layer-thickness
         o-angle :opening-angle
         {mcu-x :x
          mcu-y :y
          mcu-z :z} :controller-position
         {mcu-w :x 
          mcu-d :y
          mcu-h :z} :controller-dimensions
         {bat-x :x
          bat-y :y
          bat-z :z} :battery-position
         {bat-w :x
          bat-d :y
          bat-h :z} :battery-dimensions
         do-mirror :mirror} config
        keycaps (place-at-key-positions config (keycap config))
        mcu (cube mcu-w mcu-d mcu-h)
        battery (cube bat-w bat-d bat-h)
        explode 1]
    (mirror-halves
     do-mirror
     (translate [1 0 0]
                (union
                 (translate [0 0 (* 2 explode)] (->> mcu
                                                     (rotate o-angle [0 0 1])
                                                     (translate [mcu-x mcu-y mcu-z])
                                                     (color [0.3 0.3 0.8 1])))
                 (translate [0 0 (* 2 explode)] (->> battery
                                                     (rotate o-angle [0 0 1])
                                                     (translate [bat-x bat-y bat-z])
                                                     (color [0.3 0.8 0.3 1])))
                 (translate [0 0 explode] keycaps)
                 (translate [0 0 (* plate-thickness explode)] (plate-layer config)) 
                 #_(translate [0 0 (* layer-thickness explode)] (top-layer config)) 
                 #_(plate-layer-upper config)
                 #_(translate [0 0 (- (* 1 plate-thickness explode))] (plate-layer-lower config))
                 #_(translate [0 0 (- (* 2 plate-thickness explode))] (frame-layer config))
                 #_(translate [0 0 (- (* 3 plate-thickness explode))] (bottom-layer config))
                 #_(->> (cube 200 200 5)
                        (rotate (/ PI 4) [0 0 1])
                        (translate [0 45 (- (* 4 plate-thickness explode))])
                        (color [0.8 0.8 0.8 0.5])))))) 
  )

(defn create-multi-model []
  (let [redpoll (create-model config)]
    (union redpoll
           )))
#_(defn all-layers []
    (union
     (translate [0 0 0] (top-layer config))
     (translate [0 -150 0] (plate-layer-lower config))
     (translate [0 -300 0] (bottom-layer config))
     (translate [-220 -210 0] (rotate (* PI 3/2) [0 0 1] (plate-layer-upper config)))
     (translate [-250 -40 0] (rotate (* PI 1/2) [0 0 1] (frame-layer config)))
     #_(color [0.5 0.5 0.5] (translate [-100 150 -20] (cube 495 1000 1.5)))))
  

(defn run [] 
  (spit "things/redpoll-split/redpoll-split.scad"
        (write-scad (create-multi-model)))
  (spit "things/redpoll-split/top.scad" (write-scad (project (top-layer config))))
  #_(spit "things/redpoll-split/plate-upper.scad" (write-scad (project (plate-layer-upper config))))
  #_(spit "things/redpoll-split/plate-lower.scad" (write-scad (project (plate-layer-lower config))))
  (spit "things/redpoll-split/frame.scad" (write-scad (project (frame-layer config))))
  #_(spit "things/redpoll-split/bottom.scad" (write-scad (project (bottom-layer config))))
  #_(spit "things/redpoll-split/all.scad" (write-scad (project (all-layers))))
  )

(run)
