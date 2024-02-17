; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/redpoll.scad in openScad

(ns redpoll-split
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [translate rotate intersection union hull difference cube color mirror cylinder project]]))

(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(def redpoll {:keycap-dimensions {:x 18 :y 18 :z 11}
              :cutout-dimensions {:x 13.96 :y 13.96}
              :key-distance {:x 19.0 :y 19.0}
              :keycap-kerf 0.75

              :opening-angle (deg2rad 16)
              :plate-thickness 1.5
              :layer-thickness 2
              :plate-border 3
              :plate-mirror-edge {:min 22 :max 73}
              :controller-position {:x 16 :y 65 :z -5}
              :controller-dimensions {:x 18 :y 23 :z 1.5} ;; Seeed XIAO BLE
              ;;  {:x 18 :y 33.5 :z 1.5}  ;; Nice!Nano
              :battery-position {:x 18 :y 9 :z 0}
              :battery-dimensions {:x 20 :y 35 :z 6.5}
              ;; {:x 20 :y 35 :z 6.5}  https://www.mylipo.de/Lipo-Akku-250mAh-37V-25C-50C-GENIUS-CP-WALKERA
              ;; {:x 20 :y 35 :z 5}  https://www.mylipo.de/Lipo-Akku-180mAh-37V-25C-50C
              ;; {:x 11 :y 42 :z 5.8}  https://www.mylipo.de/Lipo-Akku-150mAh-37V-25C-50CJST-PH125-2-Pin-Stecker
              
              :screwhole-radius 0.6
              :screwhole-positions [[4 2.8] [4 -0.8] [1.2 4.4]]
              :strut-positions [[4 2.9] [4 -0.9]]
              :thumb-screwhole-positions [[3.8 0] [-0.75 -0.15]]
              :thumb-strut-positions [[3.8 0] [-0.75 -0.2]]

              :row-number 3
              :col-number 6
              :col-staggers [8 12 18 14 6 3]
              :col-offset [40 -11]
              :excluded-grid-positions #{}
              :additional-grid-positions #{[1 3]}

              :thumb-row-number 1
              :thumb-col-number 4
              :thumb-offset [21 -25]
              :thumb-staggers [0 0 0 6]})

(def config redpoll)


(defn place-shape [config shape [offset-x offset-y] staggers [row col]]
  (let [{{dist-x :x
          dist-y :y} :key-distance
         angle :opening-angle} config]
    (->> shape
         (translate [(* row dist-x) 
                     (+ (* col dist-y) 
                        (get staggers row 0))
                     0])
         (translate [offset-x offset-y 0])
         (rotate angle [0 0 1])
         )))

(defn single-hole [config kerf]
  (let [{{cutout-width :x
          cutout-height :y} :cutout-dimensions} config]
    (cube (+ cutout-width kerf) cutout-height 25)))

(defn place-at-finger-position [config shape position]
  (let [{col-staggers :col-staggers
         offset :col-offset} config]
    (place-shape config shape offset col-staggers position)))

(defn place-at-thumb-position [config shape position]
  (let [{thumb-staggers :thumb-staggers
         thumb-offset :thumb-offset} config]
    (place-shape config shape thumb-offset thumb-staggers position)))

(defn place-at-key-positions [config shape]
  (let [{row-number :row-number
         col-number :col-number
         thumb-row-number :thumb-row-number
         thumb-col-number :thumb-col-number
         excluded :excluded-grid-positions
         additional :additional-grid-positions} config]
    (concat 
           (->> (for [col-idx (range col-number)
                      row-idx (range row-number)]
                  [col-idx row-idx])
                (remove excluded)
                (concat additional)
                (map (partial place-at-finger-position config shape)))
           (->> (for [col-idx (range thumb-col-number)
                      row-idx (range thumb-row-number)]
                  [col-idx row-idx])
                (map (partial place-at-thumb-position config shape))))))


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

(defn mirror-halves [shape]
  (union shape (mirror [1 0 0] shape)))

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
         #_(mirror-halves) 
         (color [0.98 0.92 0.6 1]))))
  
(defn screw-holes [config]
  (let [{hole-poss :screwhole-positions
         thumb-hole-poss :thumb-screwhole-positions
         screwhole-radius :screwhole-radius} config
        hole (color [0 0 0 1] (cylinder screwhole-radius 15))]
    (mirror-halves (concat (->> hole-poss
                                (map (partial place-at-finger-position config hole)))
                           (->> thumb-hole-poss
                                (map (partial place-at-thumb-position config hole)))))))

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

(defn plate-layer [config cutout-switch cutout-controller]
  (let [base-plate (base-plate config)
        cutouts (place-at-key-positions config cutout-switch)
        cutout-battery (hull (translate [0 14 0] (cube 10 47 5))
                             (translate [0 2 0] (cube 28 16 5)))]
    (union (difference base-plate
                       cutouts
                       cutout-battery
                       cutout-controller
                       (screw-holes config)))))

(defn plate-layer-upper [config]
  (plate-layer config 
               (single-hole config 0)
               (union (translate [0 65 0] (cube 18 33.5 5))
                      (translate [0 66 0] (cube 9 33 5)))))

(defn plate-layer-lower [config]
  (let [cutout-controller (hull (translate [0 65 0] (cube 28 27 5))
                                (translate [0 65 0] (cube 18 33.5 5)))
        cutout-usb-c (translate [0 66 0] (cube 9 33 5))
        cutout-power-switch (translate [20.4 80.3 0] (cube 7.2 10 5))
        cutout-reset-button (translate [-19.7 80.3 0] (cube 6.2 11 5))]
    (plate-layer config
                 (single-hole config 1.2)
                 (union cutout-controller
                        cutout-usb-c
                        cutout-power-switch
                        cutout-reset-button))))

(defn test-layer [config]
  (intersection (plate-layer-upper config)
                (translate [18 50 0] (cube 60 80 5))))

    
(defn frame-layer [config]
  (let [{plate-thickness :plate-thickness
         strut-poss :strut-positions
         thumb-strut-poss :thumb-strut-positions} config
        base-plate (base-plate config)
        cutout-right (apply hull (place-at-key-positions config (single-hole config 0)))
        cutout (hull (mirror-halves cutout-right))
        cutout-controller (translate [0 65 0] (cube 28 30 5))
        cutout-switch (translate [18 75 0] (cube 12 8 5))
        cutout-reset (translate [-18 75 0] (cube 10 8 5))
        struts-right (concat (map (partial place-at-finger-position config (cylinder 7 plate-thickness)) strut-poss)
                             (map (partial place-at-thumb-position config (cylinder 7 plate-thickness)) thumb-strut-poss))]
    (difference (union 
                 (map (partial intersection base-plate) struts-right)
                 (difference base-plate
                             (mirror-halves cutout)
                             cutout-controller
                             cutout-switch
                             cutout-reset
                             ))
                 (screw-holes config))))

(defn bottom-layer [config]
  (difference (base-plate config)
              (screw-holes config)))



(defn create-model [config]
  (let [{plate-thickness :plate-thickness
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
         bat-h :z} :battery-dimensions} config
        keycaps (place-at-key-positions config (keycap config))
        mcu (cube mcu-w mcu-d mcu-h)
        battery (cube bat-w bat-d bat-h)
        explode 1]
    (mirror-halves
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
                 (translate [0 0 (* 2 explode)] keycaps)
                 (translate [0 0 (* plate-thickness explode)] (top-layer config)) 
                 (plate-layer-upper config)
                 (translate [0 0 (- (* 1 plate-thickness explode))] (plate-layer-lower config))
                 (translate [0 0 (- (* 2 plate-thickness explode))] (frame-layer config))
                 (translate [0 0 (- (* 3 plate-thickness explode))] (bottom-layer config))
                 #_(->> (cube 200 200 5)
                        (rotate (/ PI 4) [0 0 1])
                        (translate [0 45 (- (* 4 plate-thickness explode))])
                        (color [0.8 0.8 0.8 0.5])))))) 
  )

(defn create-multi-model []
  (let [redpoll (create-model config)]
    (union redpoll
           )))

(defn all-layers []
  (union
   (translate [0 0 0] (top-layer config))
   (translate [0 -150 0] (plate-layer-lower config))
   (translate [0 -300 0] (bottom-layer config))
   (translate [-220 -210 0] (rotate (* PI 3/2) [0 0 1] (plate-layer-upper config)))
   (translate [-250 -40 0] (rotate (* PI 1/2) [0 0 1] (frame-layer config)))
   #_(color [0.5 0.5 0.5] (translate [-100 150 -20] (cube 495 1000 1.5)))))

(defn run [] 
  (spit "things/redpoll-split/redpoll.scad"
        (write-scad (create-multi-model)))
  (spit "things/redpoll-split/top.scad" (write-scad (project (top-layer config))))
  (spit "things/redpoll-split/plate-upper.scad" (write-scad (project (plate-layer-upper config))))
  (spit "things/redpoll-split/plate-lower.scad" (write-scad (project (plate-layer-lower config))))
  (spit "things/redpoll-split/frame.scad" (write-scad (project (frame-layer config))))
  (spit "things/redpoll-split/bottom.scad" (write-scad (project (bottom-layer config))))
  (spit "things/redpoll-split/all.scad" (write-scad (project (all-layers))))
  )

(run)
