; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/mk5/all.scad in openScad

(ns azelusMK5
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [translate rotate intersection union hull difference cube color mirror cylinder project]]))

(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

;; TODOs
;; * resize Battery cutout
;; * Round screw holes 
;; * Battery Hatch (top layer)

(def base-config {:quality {:fa 2
                            :fn 40  ;; low 10 (instantaneous), medium 30 (3mins), high 50 (??? mins)
                            :fs 0.1}
                  :opening-angle (deg2rad 14)
                  :cutout-dimensions {:x 13.98 :y 13.98}
                  :key-distance {:x 19.0 :y 19.0}
                  :keycap-kerf 0.75
                  :plate-thickness 1.5
                  :plate-border 3
                  :mirror-offset [27.5 0]
                  :keycap-dimensions {:x 18 :y 18 :z 10}
                  ;; Dimensions for the MacBook Pro keyboard:
                  ;; The top feet are positioned in the gap between F-keys and number-row (thus x-distance between them can be arbitrary but less than 270mm).
                  ;; The bottom feet are positioned in the the gap between option+command (left) and command+option (right), x-distance = 141.5 mm.
                  ;; y-distance between top and bottom feet (centers): min 78, max 87 (due to location of the respective gaps (min = 73 max 92), and the 10mm height of the bottom feet). 
                  ;; The pair of bottom feet is shifted 6mm to the left. That way the keyboard sits slightly off-center to the left and allows access to the MacBook's touch-id key.
                  :rubber-feet [{:w 10 :d 2 :h 3 :x 112 :y 70}
                                {:w 10 :d 2 :h 3 :x -112 :y 70}
                                {:w 2 :d 10 :h 3 :x 67.75 :y -11}
                                {:w 2 :d 10 :h 3 :x -73.75 :y -10}]
                  :plate-mirror-edge {:min -38 :max 88 :top-width 50}
                  :controller {:w 18 :d 33.5 :h 1.5}
                  :controller-top-wall 2

                  :screwhole-radius 0.6
                  :screwhole-positions [[4.8 2.25] [4.5 -1.05] [1.2 3.8]]
                  :strut-positions [[4.8 2.75] [4.5 -1.05]]
                  :thumb-screwhole-positions [[3.01 -0.2] [-0.81 -0.15]]
                  :thumb-strut-positions [[3.01 -0.2] [-0.81 -0.15]]

                  :battery-cutout-positions [[-0.9 -0.35] [-0.9 1.8]]

                  :row-number 3
                  :col-number 6
                  :col-staggers [0 2 10 5 -5 -8]
                  :excluded-grid-positions #{}
                  :additional-grid-positions #{[1 3]}

                  :thumb-row-number 1
                  :thumb-col-number 4
                  ;; multiples of key-distance
                  :thumb-offset-units [-1 -1.31]
                  :thumb-staggers [0 0 0 13]})

(def config-variant {:plate-mirror-edge {:min -29 :max 95 :top-width 30}

                     :col-staggers [8 12 20 15 3 -1]
                     :additional-grid-positions #{[1 3] [5 3]}})

(let [{{key-dist :y} :key-distance
       [_ y-offset] :thumb-offset-units
       [_ _ _ thumb-stagger] :thumb-staggers
       [_ _ col-stagger _ _ _] :col-staggers} base-config]
  (prn "key-dist=" key-dist ", y-offset=" y-offset ", thumb-stagger=" thumb-stagger ", col-stagger=" col-stagger)
  (prn "Distance from middle finger bottom key to forth thumb key: " (+ thumb-stagger
                                                                        (* key-dist y-offset)
                                                                        key-dist
                                                                        (- col-stagger))))

(defn place-shape [config shape [offset-x offset-y] staggers [row col]]
  (let [{{dist-x :x
          dist-y :y} :key-distance
         angle :opening-angle} config]
    (->> shape
         (translate [offset-x offset-y 0])
         (translate [(* row dist-x)
                     (+ (* col dist-y)
                        (get staggers row 0))
                     0])
         (rotate angle [0 0 1]))))

(defn single-hole [config x-kerf y-kerf]
  (let [{{cutout-width :x
          cutout-height :y} :cutout-dimensions} config]
    (cube (+ cutout-width x-kerf) (+ cutout-height y-kerf) 25)))

(defn place-at-finger-position [config shape position]
  (let [{col-staggers :col-staggers
         offset :mirror-offset} config]
    (place-shape config shape offset col-staggers position)))

(defn place-at-thumb-position [config shape position]
  (let [{thumb-staggers :thumb-staggers
         base-offset :mirror-offset
         key-distance :key-distance
         thumb-offset-units :thumb-offset-units} config
        thumb-offset (->> (map * (vals key-distance) thumb-offset-units)
                          (map + base-offset))]
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

(defn rubber-feet [config]
  (let [{feet-config :rubber-feet} config]
    (map (fn [{x :x
               y :y
               w :w
               d :d
               h :h}]
           (translate [x y -2]
                      (color [0.3 0.3 0.3 1]
                             (cube w d h))))
         feet-config)))

(defn mirror-halves [shape]
  (union shape (mirror [1 0 0] shape)))

(defn average [& vals]
  (/ (reduce + vals) (count vals)))

(defn base-outline [config]
  (let [{plate-thickness :plate-thickness
         plate-border :plate-border
         {cap-x :x
          cap-y :y} :keycap-dimensions
         {mirror-y-min :min
          mirror-y-max :max
          top-width :top-width} :plate-mirror-edge} config
        mirror-edge-helper (translate [0 (average mirror-y-max mirror-y-min) 0]
                                      (cube 0.01
                                            (- mirror-y-max mirror-y-min)
                                            plate-thickness))
        mirror-top-helper (translate [0 mirror-y-max 0]
                                     (cube  top-width
                                            0.01
                                            plate-thickness))]
    (->> (cube (+ cap-x (* 2 plate-border))
               (+ cap-y (* 2 plate-border))
               plate-thickness)
         (place-at-key-positions config)
         (cons mirror-edge-helper)
         (cons mirror-top-helper)
         (apply hull)
         (mirror-halves)
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
        base-outline (base-outline config)
        cap-cutouts (place-at-key-positions config
                                            (cube (+ cap-x (* 2 kerf))
                                                  (+ cap-y (* 2 kerf))
                                                  25))]
    (difference base-outline
                (mirror-halves cap-cutouts))))

(defn plate-layer [config cutout-switch cutout-controller]
  (let [{{max-y :max} :plate-mirror-edge
         {controller-height :d} :controller
         controller-wall :controller-top-wall
         battery-cutout-poss :battery-cutout-positions} config
        top (- max-y controller-wall (/ controller-height 2))
        base-outline (base-outline config)
        cutouts (mirror-halves (place-at-key-positions config cutout-switch))
        cutout-battery (hull (mirror-halves
                              (map (partial place-at-finger-position config (single-hole config -2 0))
                                   battery-cutout-poss)))]
    (union (difference base-outline
                       cutouts
                       cutout-battery
                       (translate [0 top 0] cutout-controller)
                       (screw-holes config)))))

(defn usb-c-cutout [contr-w contr-d]
  (translate [0 1 0] (cube (/ contr-w 2) (dec contr-d) 5)))

(defn pcb-cutout [contr-w contr-d]
  (cube contr-w contr-d 5))

(defn controller-cutout [config]
  (let [{{contr-w :w
          contr-d :d} :controller} config]
    (union (pcb-cutout contr-w contr-d)
           (usb-c-cutout contr-w contr-d))))

(defn plate-layer-upper [config]
  (plate-layer config
               (single-hole config 0 0)
               (controller-cutout config)))

(defn plate-layer-lower [config additional-cutout]
  (let [{{contr-w :w
          contr-d :d} :controller} config
        cutout-controller (hull (cube contr-w contr-d 5)
                                (cube (+ contr-w 10) (- contr-d 6) 5))
        cutout-usb-c (usb-c-cutout contr-w contr-d)]
    (plate-layer config
                 (single-hole config 0 1.2)
                 (union cutout-controller
                        cutout-usb-c
                        additional-cutout))))

(defn plate-layer-lower1 [config]
  (let [{{contr-d :d} :controller} config]
    (plate-layer-lower config (translate [20 (/ contr-d 2) 0]
                                         (cube 7.2 10 5)))))

(defn plate-layer-lower2 [config]
  (let [{{contr-d :d} :controller
         controller-wall :controller-top-wall} config]
    (plate-layer-lower config (translate [20
                                          (- (/ contr-d 2)
                                             (* 2 controller-wall))
                                          0]
                                         (cube 7.2 5 5)))))

(defn frame-layer [config]
  (let [{{fa :fa
          fn :fn
          fs :fs} :quality} config]
    ;; produce high quality mesh
    (binding [scad-clj.model/*fa* fa
              scad-clj.model/*fn* fn  ;; low 10 (instantaneous), medium 30 (3mins), high 50 (??? mins)
              scad-clj.model/*fs* fs]
      (let [{plate-thickness :plate-thickness
             strut-poss :strut-positions
             thumb-strut-poss :thumb-strut-positions} config
            base-outline (base-outline config)
            cutout-right (apply hull (place-at-key-positions config (single-hole config 0 0)))
            cutout (hull (mirror-halves cutout-right))
            cutout-controller (translate [0 65 0] (cube 28 30 5))
            cutout-switch (translate [18 75 0] (cube 12 8 5))
            cutout-reset (translate [-18 75 0] (cube 10 8 5))
            struts-right (concat (map (partial place-at-finger-position config (cylinder 5 plate-thickness)) strut-poss)
                                 (map (partial place-at-thumb-position config (cylinder 5 plate-thickness)) thumb-strut-poss))]
        (difference (union
                     (mirror-halves (map (partial intersection base-outline) struts-right))
                     (difference base-outline
                                 (mirror-halves cutout)
                                 cutout-controller
                                 cutout-switch
                                 cutout-reset))
                    (screw-holes config))))))

(defn bottom-layer [config]
  (difference (base-outline config)
              (screw-holes config)))

(defn create-model [config]
  (let [{plate-thickness :plate-thickness} config
        keycaps (mirror-halves (place-at-key-positions config (keycap config)))
        rubber-feet (rubber-feet config)
        explode 1
        layers [keycaps
                (top-layer config)
                (plate-layer-upper config)
                (plate-layer-lower1 config)
                (plate-layer-lower2 config)
                (frame-layer config)
                (frame-layer config)
                (bottom-layer config)
                rubber-feet]]
    (union
     (->> layers
          (map-indexed #(translate [0 0 (- (* %1 plate-thickness explode))] %2))))))

(defn all-layers [config]
  (union
   (translate [275 0 0] (rubber-feet config))
   (translate [-275 0 0] (top-layer config))
   (translate [-275 -150 0] (plate-layer-upper config))
   (translate [0 -150 0] (plate-layer-lower1 config))
   (translate [275 -150 0] (plate-layer-lower2 config))
   (translate [-275 -300 0] (frame-layer config))
   (translate [0 -300 0] (frame-layer config))
   (translate [275 -300 0] (bottom-layer config))
   #_(translate [-220 -210 0] (rotate (* PI 3/2) [0 0 1] (plate-layer-upper config)))
   #_(color [0.5 0.5 0.5] (translate [-100 150 -20] (cube 495 1000 1.5)))))

(defn create-multi-model []
  (let [variant (create-model (merge base-config config-variant))
        base-model (create-model base-config)]
    (union base-model
           (translate [0 0 0] (all-layers base-config)))))


(defn run []
  (spit "things/mk5/azelusmk5.scad"
        (write-scad (create-multi-model)))
  (spit "things/mk5/top.scad" (write-scad (project (top-layer base-config))))
  (spit "things/mk5/plate-upper.scad" (write-scad (project (plate-layer-upper base-config))))
  (spit "things/mk5/plate-lower1.scad" (write-scad (project (plate-layer-lower1 base-config))))
  (spit "things/mk5/plate-lower2.scad" (write-scad (project (plate-layer-lower1 base-config))))
  (spit "things/mk5/frame.scad" (write-scad (project (frame-layer base-config))))
  (spit "things/mk5/bottom.scad" (write-scad (project (bottom-layer base-config))))
  (spit "things/mk5/all.scad" (write-scad (project (all-layers base-config)))))

(run)