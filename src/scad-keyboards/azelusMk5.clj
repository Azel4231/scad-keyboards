; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl
; connect to nrepl from IDE/Calva
; eval file
; open ./things/mk5/all.scad in openScad

(ns azelusMk5
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [translate rotate scale intersection union hull difference cube color mirror cylinder project]]))

(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

;; TODOs
;;   Varianten probieren: 
;;      schmal braucht für Nice!Nano mehr Höhe
;;      breit ist grenzwertig breit für Lasern
;;   

(def base-config {:quality {:fa 2
                            :fn 40  ;; low 10, medium 30, high 50
                            :fs 0.1}
                  :opening-angle (deg2rad 14)
                  :cutout-dimensions {:x 13.98 :y 13.98}
                  :key-distance {:x 19.0 :y 19.0}
                  :keycap-kerf 0.75
                  :plate-thickness 1.5
                  :plate-border 3
                  :mirror-offset [27.5 0]
                  :keycap-dimensions {:x 18 :y 18 :z 10}
                  ;; Rubber feet dimensions that match the MacBook Pro keyboard (ISO-DE):
                  ;; The top feet are aligned horizontally and located in the gap between F-keys and number-row (thus x-distance between them can be arbitrary but less than 270mm).
                  ;; The bottom feet are oriented vertically and located in the the gaps between option+command (left) and command+option (right), x-distance = 141.5 mm.
                  ;; y-distance between top and bottom feet (centers): min 78, max 87 (due to location of the respective gaps (min = 73 max 92) and the length of the bottom feet (10mm)). 
                  ;; The pair of bottom feet is shifted 6mm to the left (of the keyboard's axis of symmetry). That way the keyboard sits slightly off-center to the left of the laptop keyboard and allows access to the MacBook's touch-id key.
                  ;; Width of the feet must be less than 2mm at the bottom. A trapezoid cross section is ideal so the top can be wider for more glued surface.
                  ;; Height of the feet must be more than 1.5mm
                  ;; Ideal base material are 10mm x 10mm x 2mm rubber feet, that can then be cut to dimension. This even leaves room for shortening individual feet to eliminate wobble.
                  :rubber-feet [{:w 10 :d 2 :h 2 :x 112 :y 70}
                                {:w 10 :d 2 :h 2 :x -112 :y 70}
                                {:w 2 :d 10 :h 2 :x 67.75 :y -11}
                                {:w 2 :d 10 :h 2 :x -73.75 :y -10}]
                  :plate-mirror-edge {:min -40.2 :max 87 :top-width 26}
                  :controller {:w 18 :d 33.5 :h 1.5}
                  :controller-top-wall 2

                  :screwhole-radius 0.6
                  :screwhole-positions [[4.99 2.25] [4.5 -1.05] [0.98 3.75]]
                  :strut-positions [[4.99 2.25 ] [4.5 -1.05] [0.98 3.75]]
                  :thumb-screwhole-positions [[3.01 -0.2] [-0.88 -0.28]]
                  :thumb-strut-positions [[3.01 -0.2] [-0.88 -0.23]]
                  :strut-radius 5
                  :magnet{:positions [[3.95 2.5] [3.99 -1]]
                          :radius 3}

                  :battery-cutout-positions [[-0.91 -0.35] [-0.91 1.8]]

                  :row-number 3
                  :col-number 6
                  :col-staggers [-1 2 10 5 -5 -8]
                  :excluded-grid-positions #{}
                  :additional-grid-positions #{[1 3]}

                  :thumb-row-number 1
                  :thumb-col-number 5
                  ;; multiples of key-distance
                  :thumb-offset-units [-1 -1.42]
                  :thumb-staggers [0 0 0 19 10]})

(def config-variant {:thumb-col-number 4
                     })

                     

(let [{{key-dist :y} :key-distance
       [_ y-offset] :thumb-offset-units
       [th-stagger-inner _ _ th-stagger-outer] :thumb-staggers
       [col-stagger-1st _ col-stagger-middle _ _ _] :col-staggers} base-config]
  (prn "key-dist=" key-dist ", y-offset=" y-offset ", thumb-stagger=" th-stagger-outer ", col-stagger-outer=" col-stagger-middle)
  (prn "4th thumb key: Distance from middle finger bottom key to forth thumb key: " (+ th-stagger-outer
                                                                                       (* key-dist y-offset)
                                                                                       (- col-stagger-middle)))
  (prn "Thumb cluster: Distance innermost column:" (+ th-stagger-inner
                                                      (* key-dist y-offset)
                                                      (- col-stagger-1st))))

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

(defn rubber-feet [config]
  (let [{feet-config :rubber-feet} config] 
    (map (fn [{x :x
               y :y
               w :w
               d :d
               h :h}]
           (translate [x y 0]
                      (color [0.3 0.3 0.3 1]
                             (cube w d h))))
         feet-config)))

(defn rubber-feet-outline-layer [config]
  (let [layer-outline (base-outline config)
        rubber-feet-indicator (->> (rubber-feet config)
                                   (scale [1 1 10]))]
    (difference layer-outline rubber-feet-indicator)))


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
                              (map (partial place-at-finger-position config (single-hole config -6 0))
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

(defn magnet-cutouts [config]
  (let [{{radius :radius
          poss :positions} :magnet} config] 
    (mirror-halves (map (partial place-at-finger-position config (cylinder radius 10))
                        poss))))

(defn plate-layer-upper [config]
  (union #_(translate [0 5 0](cube 17.5 31 4.3))
         (difference (plate-layer config
                                  (single-hole config 0 0)
                                  (controller-cutout config))
                     (magnet-cutouts config))))

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
    (difference (plate-layer-lower config (translate [20 (/ contr-d 2) 0]
                                                     (cube 7.2 10 5)))
                (magnet-cutouts config))))

(defn plate-layer-lower2 [config]
  (let [{{contr-d :d} :controller
         controller-wall :controller-top-wall} config]
    (plate-layer-lower config (translate [20
                                          (- (/ contr-d 2)
                                             (* 2 controller-wall))
                                          0]
                                         (cube 7.2 5 5)))))

(defn frame-layer [config]
  (let [{plate-thickness :plate-thickness
         strut-poss :strut-positions
         strut-radius :strut-radius
         thumb-strut-poss :thumb-strut-positions} config
        base-outline (base-outline config)
        cutout-right (apply hull (place-at-key-positions config (single-hole config 0 0)))
        cutout (hull (mirror-halves cutout-right))
        cutout-controller (translate [0 65 0] (cube 28 30 5))
        cutout-switch (translate [18 75 0] (cube 12 8 5))
        cutout-reset (translate [-18 75 0] (cube 10 8 5))
        struts-right (concat (map (partial place-at-finger-position config (cylinder strut-radius plate-thickness)) strut-poss)
                             (map (partial place-at-thumb-position config (cylinder strut-radius plate-thickness)) thumb-strut-poss))]
    (difference (union
                 (mirror-halves (map (partial intersection base-outline) struts-right))
                 (difference base-outline
                             (mirror-halves cutout)
                             cutout-controller
                             cutout-switch
                             cutout-reset))
                (screw-holes config))))

(defn frame-layer-half [config]
  (let [{{top :max
          bottom :min} :plate-mirror-edge } config
        x-dim 140
        y-dim (- top bottom) 
        frame-layer (frame-layer config)]
    ;; center cube in y, shift to positive x only
    (intersection (translate [(/ x-dim 2) (/ (+ top bottom) 2) 0]
                             (cube x-dim y-dim 10))
                  frame-layer)))
    

(defn bottom-layer [config]
  (difference (base-outline config)
              (screw-holes config)))

(defn create-model [config]
  (let [{plate-thickness :plate-thickness} config
        keycaps (mirror-halves (place-at-key-positions config (keycap config)))
        explode 1
        layers [#_keycaps
                #_(top-layer config)
                (plate-layer-upper config)
                (plate-layer-lower1 config)
                (plate-layer-lower2 config)
                (frame-layer config)
                (frame-layer config)
                #_(bottom-layer config)
                (rubber-feet config)]]
    (union
     (->> layers
          (map-indexed (fn [idx layer] 
                         (translate [0 0 (- (* idx plate-thickness explode))] 
                                    layer)))))))

(defn all-layers [config]
  (union
   ;; Helper layer. Don't cut this directly. The idea is to engrave the exact rubber feet positions onto the bottom layer for easier glueing later on.
   ;; Not intended for cutting holes anywhere, just to provide the tool path for engraving. The layer outline helps align the paths with the bottom layer. 
   (translate [275 0 0] (rubber-feet-outline-layer config))
   (translate [-275 0 0] (top-layer config))
   (translate [-275 -150 0] (plate-layer-upper config))
   (translate [0 -150 0] (plate-layer-lower1 config))
   (translate [275 -150 0] (plate-layer-lower2 config))
   (translate [-275 -300 0] (frame-layer config))
   (translate [0 -300 0] (frame-layer config))
   (translate [275 -300 0] (bottom-layer config))
   #_(translate [-220 -210 0] (rotate (* PI 3/2) [0 0 1] (plate-layer-upper config)))
   #_(color [0.5 0.5 0.5] (translate [-100 150 -20] (cube 495 1000 1.5)))))

(defn optimized-placement [config]
  ;; move to origin
  (let [{[x-offset _] :mirror-offset} config
        width (* 2 (+ 110 x-offset))]
    (union
     ;; wood outline
     (color [0.3 0.3 0.6 1] (translate [500 125 -5] (cube 1000 250 1)))
     (translate [285 145 0]
                (union
                 (translate [width 140 0] (mirror [1 0 0] (rubber-feet-outline-layer config)))

                 (translate [(* -1 width)  0] (frame-layer-half config))
                 (translate [(* 0 width) 0 0] (top-layer config))
                 (translate [(* 1 width) 0 0] (bottom-layer config))
                 (translate [(* 2 width) 0 0] (mirror [1 0 0] (frame-layer config)))

                 (translate [(* 0.5 width) -52 0]
                            (translate [(* -1 width) 0 0] (mirror [0 1 0] (plate-layer-upper config)))
                            (translate [(* 0 width)  0 0] (mirror [0 1 0]  (plate-layer-lower1 config)))
                            (translate [(* 1 width) 0 0] (mirror [0 1 0]  (plate-layer-lower2 config)))
                            (translate [(* 2 width) 0 0] (mirror [0 1 0] (mirror [1 0 0] (frame-layer-half config))))))))))
                

(defn create-multi-model [config]
  (let [variant-config (merge config config-variant)
        variant (create-model variant-config)
        base-model (create-model config)]
    (union base-model
     (translate [0 150 0] variant)
     (translate [0 0 0] (all-layers config)))))
           

(defn run [config]
  (let [{{fa :fa
           fn :fn
           fs :fs} :quality} config]
    (binding [scad-clj.model/*fa* fa
              scad-clj.model/*fn* fn
              scad-clj.model/*fs* fs]
      (spit "things/mk5/azelusmk5.scad"
            (write-scad (create-multi-model config)))
      (spit "things/mk5/all.scad" (write-scad (project (all-layers config))))
      ;; space-efficient placement for laser cutting
      (spit "things/mk5/optimized.scad" (write-scad (optimized-placement config))))))

(run base-config)