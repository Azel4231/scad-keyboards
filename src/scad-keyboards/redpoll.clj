; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/redpoll.scad in openScad

(ns redpoll
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(def redpoll {:opening-angle (deg2rad 18)
              :cutout-dimensions {:x 13.85 :y 13.85}
              :key-distance {:x 18.0 :y 17.0}
              :keycap-kerf 0.75
              :plate-thickness 1.5
              :plate-border 4
              :mirror-offset [30 0]
              :keycap-dimensions {:x 17.5 :y 16.5 :z 3}
              :plate-mirror-edge {:min -37 :max 85}
              :screwhole-radius 0.6
              :screwhole-positions [[4 2.8] [4 -0.8] [1.2 4.4]]
              :strut-positions [[4 2.9] [4 -0.9]]
              :thumb-screwhole-positions [[3.8 0] [-0.75 -0.15]]
              :thumb-strut-positions [[3.8 0] [-0.75 -0.2]]

              :row-number 3
              :col-number 6
              :col-staggers [8 12 20 15 3 0]
              :excluded-grid-positions #{}
              :additional-grid-positions #{[1 3]}

              :thumb-row-number 1
              :thumb-col-number 4
              :thumb-offset [12 -20]
              :thumb-staggers [0 0 0 4]})

(def goldcrest {:opening-angle (deg2rad 16) 
                :excluded-grid-positions #{[5 0]}
                :additional-grid-positions #{[1 3] [3 -1]}})

(def config redpoll)


(defn place-shape [shape [offset-x offset-y] staggers [row col]]
  (let [{{dist-x :x
          dist-y :y} :key-distance
         angle :opening-angle} config]
    (->> shape
         (translate [(* row dist-x) 
                     (+ (* col dist-y) 
                        (get staggers row 0))
                     0])
         (rotate angle [0 0 1])
         (translate [offset-x offset-y 0])
         )))

(defn single-hole [kerf]
  (let [{{cutout-width :x
          cutout-height :y} :cutout-dimensions} config]
    (cube (+ cutout-width kerf) cutout-height 25)))

(defn place-at-finger-position [shape position]
  (let [{col-staggers :col-staggers
         offset :mirror-offset} config]
    (place-shape shape offset col-staggers position)))

(defn place-at-thumb-position [shape position]
  (let [{thumb-staggers :thumb-staggers
         thumb-offset :thumb-offset} config]
    (place-shape shape thumb-offset thumb-staggers position)))

(defn place-at-key-positions [shape]
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
                (map (partial place-at-finger-position shape)))
           (->> (for [col-idx (range thumb-col-number)
                      row-idx (range thumb-row-number)]
                  [col-idx row-idx])
                (map (partial place-at-thumb-position shape))))))

(defn choc-keycap []
  (let [{{cap-x :x 
          cap-y :y 
          cap-z :z} :keycap-dimensions} config]
    (color [0.3 0.3 0.3 1]
           (cube cap-x cap-y cap-z))))

(defn mirror-halves [shape]
  (union shape (mirror [1 0 0] shape)))

(defn average [& vals]
  (/ (reduce + vals) (count vals)))

(defn base-plate []
  (let [{plate-thickness :plate-thickness
         plate-border :plate-border
         {cap-x :x 
          cap-y :y} :keycap-dimensions
         {mirror-y-min :min 
          mirror-y-max :max} :plate-mirror-edge} config
        mirror-edge-helper (translate [0 (average mirror-y-max mirror-y-min) 0]
                                     (cube 0.1
                                           (- mirror-y-max mirror-y-min)
                                           plate-thickness))]
    (->> (cube (+ cap-x (* 2 plate-border)) 
               (+ cap-y (* 2 plate-border)) 
               plate-thickness)
         (place-at-key-positions)
         (cons mirror-edge-helper)
         (apply hull)
         (mirror-halves) 
         (color [0.98 0.92 0.6 1]))))
  
(defn screw-holes []
  (let [{hole-poss :screwhole-positions
         thumb-hole-poss :thumb-screwhole-positions
         screwhole-radius :screwhole-radius} config
        hole (color [0 0 0 1] (cylinder screwhole-radius 15))]
    (mirror-halves (concat (->> hole-poss
                                (map (partial place-at-finger-position hole)))
                           (->> thumb-hole-poss
                                (map (partial place-at-thumb-position hole)))))))

(defn top-layer []
  (let [{{cap-x :x
          cap-y :y} :keycap-dimensions
         kerf :keycap-kerf} config
        base-plate (base-plate)
        cap-cutouts (place-at-key-positions (cube (+ cap-x (* 2 kerf)) 
                                                  (+ cap-y (* 2 kerf)) 
                                                  25))]
    (difference base-plate 
                (mirror-halves cap-cutouts))))

(defn plate-layer [cutout-switch cutout-controller]
  (let [base-plate (base-plate)
        cutouts (mirror-halves (place-at-key-positions cutout-switch))
        cutout-battery (hull (translate [0 14 0] (cube 10 47 5))
                             (translate [0 2 0] (cube 28 16 5)))]
    (union (difference base-plate
                       cutouts
                       cutout-battery
                       cutout-controller
                       (screw-holes)))))

(defn plate-layer-upper []
  (plate-layer (single-hole 0) 
               (union (translate [0 65 0] (cube 18 33.5 5))
                      (translate [0 66 0] (cube 9 33 5)))))

(defn plate-layer-lower []
  (let [cutout-controller (hull (translate [0 65 0] (cube 28 27 5))
                                (translate [0 65 0] (cube 18 33.5 5)))
        cutout-usb-c (translate [0 66 0] (cube 9 33 5))
        cutout-power-switch (translate [20.4 80.3 0] (cube 7.2 10 5))
        cutout-reset-button (translate [-19.7 80.3 0] (cube 6.2 11 5))]
    (plate-layer (single-hole 1.2) 
                 (union cutout-controller
                        cutout-usb-c
                        cutout-power-switch
                        cutout-reset-button))))

(defn test-layer []
  (intersection (plate-layer-upper)
                (translate [18 50 0] (cube 60 80 5))))

    
(defn frame-layer []
  (let [{plate-thickness :plate-thickness
         strut-poss :strut-positions
         thumb-strut-poss :thumb-strut-positions} config
        base-plate (base-plate)
        cutout-right (apply hull (place-at-key-positions (single-hole 0)))
        cutout (hull (mirror-halves cutout-right))
        cutout-controller (translate [0 65 0] (cube 28 30 5))
        cutout-switch (translate [18 75 0] (cube 12 8 5))
        cutout-reset (translate [-18 75 0] (cube 10 8 5))
        struts-right (concat (map (partial place-at-finger-position (cylinder 7 plate-thickness)) strut-poss)
                             (map (partial place-at-thumb-position (cylinder 7 plate-thickness)) thumb-strut-poss))]
    (difference (union 
                 (mirror-halves (map (partial intersection base-plate) struts-right))
                 (difference base-plate
                             (mirror-halves cutout)
                             cutout-controller
                             cutout-switch
                             cutout-reset
                             ))
                 (screw-holes))))

(defn bottom-layer []
  (difference (base-plate)
              (screw-holes)))



(defn create-model [config]
  (let [{plate-thickness :plate-thickness} config
        keycaps (mirror-halves (place-at-key-positions (choc-keycap)))
        explode 1]
    (union
     (translate [0 0 (* 5 explode)] keycaps)
     (translate [0 0 (* plate-thickness explode)] (top-layer)) 
     (plate-layer-upper)
     (translate [0 0 (- (* 1 plate-thickness explode))] (plate-layer-lower))
     (translate [0 0 (- (* 2 plate-thickness explode))] (frame-layer))
     (translate [0 0 (- (* 3 plate-thickness explode))] (bottom-layer))
     #_(->> (cube 200 200 5)
            (rotate (/ PI 4) [0 0 1])
            (translate [0 45 (- (* 4 plate-thickness explode))])
            (color [0.8 0.8 0.8 0.5])))) 
  )

(defn create-multi-model []
  (let [goldcrest (create-model (merge redpoll goldcrest))
        redpoll (create-model config)]
    (union redpoll
           #_(translate [0 150 0] goldcrest)
           )))

(defn all-layers []
  (union
   (translate [0 0 0] (top-layer ))
   (translate [0 -150 0] (plate-layer-lower ))
   (translate [0 -300 0] (bottom-layer))
   (translate [-220 -210 0] (rotate (* PI 3/2) [0 0 1] (plate-layer-upper)))
   (translate [-250 -40 0] (rotate (* PI 1/2) [0 0 1] (frame-layer)))
   #_(color [0.5 0.5 0.5] (translate [-100 150 -20] (cube 495 1000 1.5)))))

(defn run [] 
  (spit "things/redpoll.scad"
        (write-scad (create-multi-model)))
  (spit "things/top.scad" (write-scad (project (top-layer))))
  (spit "things/plate-upper.scad" (write-scad (project (plate-layer-upper))))
  (spit "things/plate-lower.scad" (write-scad (project (plate-layer-lower))))
  (spit "things/frame.scad" (write-scad (project (frame-layer))))
  (spit "things/bottom.scad" (write-scad (project (bottom-layer))))
  (spit "things/all.scad" (write-scad (project (all-layers))))
  )

(run)
