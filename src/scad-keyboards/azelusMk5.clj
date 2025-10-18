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
                   ;; normally x and y 13.98
                   :cutout-dimensions {:x 13.88 :y 13.8}
                   :key-distance {:x 19.0 :y 19.0}
                   :keycap-kerf 0.75
                   :plate-thickness 1.5
                   :plate-border 3.5
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
                   :rubber-feet [{:w 10 :d 2 :h 2 :x 112 :y 67}
                                 {:w 10 :d 2 :h 2 :x -112 :y 67}
                                 {:w 2 :d 10 :h 2 :x 67.75 :y -14}
                                 {:w 2 :d 10 :h 2 :x -73.75 :y -13}]
                   :rubber-feet-concept {:positions [[112 70] [-112 70]
                                                     [67.75 -11] [-73.75 -10]]
                                         :dimensions [[10 2] [10 2]
                                                      [2 10] [2 10]]
                                         :offset [0 0]
                                         :angle 0}
                   :plate-mirror-edge {:min -40.2 :max 87 :top-width 26}
                   :controller {:w 18 :d 33.5 :h 1.5}
                   :controller-top-wall 2

                   :screwholes {:additional-positions [[4.8 2.6] [4.5 -0.7] [1.7 -1.4] [1 4.1] [-1.75 -1.3]]
                                :screwhole-radius 0.6
                                :strut-radius 5}
                   :magnets {:additional-positions [[3.95 2.75] [3.99 -0.7]]
                             :magnet-radius 2.95}
                   :battery-cutout {:rows 3
                                    :cols 1
                                    :offset [-19 2]
                                    :dimensions [14 18]}

                   :matrix {:offset [25 -8]
                            :clusters {:finger-cluster {:rows 3
                                                        :cols 6
                                                        :additional-positions [[1 3]]
                                                        :offset [0 5]
                                                        ;; Split: +2 +8 -4 -10 -1
                                                        ;; MK5:   +3 +8 -5 -10 -3
                                                        :staggers [[0 0] [0 2] [0 10] [0 6] [0 -4] [0 -6]]}
                                       :thumb-cluster {:rows 1
                                                       :cols 5
                                                       :offset [-19 -19]
                                                       :staggers [[0 0] [0 0] [0 0] [0 14] [0 10]]}}}})
 (def default-cluster {:rows 0
                       :cols 0
                       :additional-positions []
                       :positions []
                       :dimensions []
                       :offset [0 0]
                       :staggers []
                       :angle 0})

 (def config-variant {:thumb-col-number 4})

 (defn single-key-hole [config x-kerf y-kerf]
   (let [{{cutout-width :x
           cutout-height :y} :cutout-dimensions} config]
     (cube (+ cutout-width x-kerf) (+ cutout-height y-kerf) 25)))

 (defn place-in-cluster-single "Place shape at a single cluster position"
   [config shape stagger pos]
   (let [{{unit-x :x
           unit-y :y} :key-distance
          angle :opening-angle
          {[offset-x offset-y] :offset} :matrix} config
         [col row] pos
         [stagger-x stagger-y] stagger]
     (->> shape
          ;; row, col in units (1u = one key)
          ;; stagger, offset-x/y in mm
          (translate [(+ (* col unit-x)
                         offset-x
                         stagger-x)
                      (+ (* row  unit-y)
                         offset-y
                         stagger-y)
                      0])
          (rotate angle [0 0 1]))))

 (defn place-in-cluster "Place shape iterating over all positions in a single single cluster"
   [config shape cluster-def]
   (let [cluster-def (merge default-cluster cluster-def)
         {rows :rows
          cols :cols
          additional :additional-positions
          staggers :staggers
          offset :offset} cluster-def
         positions (for [c (range cols)
                         r (range rows)]
                     [c r])
         positions (concat positions additional)]
     (->> positions
          (map (fn [[col _ :as pos]]
                 (place-in-cluster-single config
                                          shape
                                          (map +
                                               (get staggers col [0 0])
                                               offset)
                                          pos))))))

 (defn place-at-key-positions "Place shape iterating over all clusters"
   [config shape]
   (let [{{clusters :clusters} :matrix} config]
     (->> clusters
          vals
          (mapcat (partial place-in-cluster config shape)))))

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

 (defn mirror-halves "Unions the shape and mirrors it around the y/z plane. Cut off the shape's -x parts before mirroring"
   [shape]
   (let [neg-x (translate [-500 0 0] (cube 1000 1000 1000))
         clean-shape (difference (union shape) neg-x )]
     (union clean-shape
            (mirror [1 0 0] clean-shape))))

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
   (let [{screw-cluster :screwholes} config
         {hole-radius :screwhole-radius} screw-cluster
         hole (color [0 0 0 1] (cylinder hole-radius 15))]
     (mirror-halves
      (place-in-cluster config hole screw-cluster))))

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
          battery-cutout-cluster :battery-cutout} config
         {[width depth] :dimensions} battery-cutout-cluster
         top (- max-y controller-wall (/ controller-height 2))
         base-outline (base-outline config)
         cutouts (mirror-halves (place-at-key-positions config cutout-switch))
         cutout-battery (hull (mirror-halves
                               (place-in-cluster config
                                                 (cube width depth 10)
                                                 battery-cutout-cluster)))]
     (union 
      (difference base-outline
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
   (let [{magnet-cluster :magnets} config
         {radius :magnet-radius} magnet-cluster]
     (mirror-halves (place-in-cluster config
                                      (cylinder radius 10)
                                      magnet-cluster))))

 (defn plate-layer-upper [config]
   (union #_(translate [0 5 0] (cube 17.5 31 4.3))
    (difference (plate-layer config
                             (single-key-hole config 0 0)
                             (controller-cutout config))
                (magnet-cutouts config))))

 (defn plate-layer-lower [config additional-cutout]
   (let [{{contr-w :w
           contr-d :d} :controller} config
         cutout-controller (hull (cube contr-w contr-d 5)
                                 (cube (+ contr-w 10) (- contr-d 6) 5))
         cutout-usb-c (usb-c-cutout contr-w contr-d)]
     (plate-layer config
                  (single-key-hole config 0 1.2)
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


 (defn frame-layer-internal [config]
   (let [{plate-thickness :plate-thickness
          screwhole-cluster :screwholes} config
         {strut-radius :strut-radius} screwhole-cluster
         base-outline (base-outline config)
         cutout-right (apply hull (place-at-key-positions config (single-key-hole config 0 0)))
         cutout (hull (mirror-halves cutout-right))
         cutout-controller (translate [0 65 0] (cube 28 30 5))
         cutout-switch (translate [18 75 0] (cube 12 8 5))
         cutout-reset (translate [-18 75 0] (cube 10 8 5))
         struts-right (place-in-cluster config
                                        (cylinder strut-radius plate-thickness)
                                        screwhole-cluster)]
     (difference (union
                  (mirror-halves (map (partial intersection base-outline) struts-right))
                  (difference base-outline
                              (mirror-halves cutout)
                              cutout-controller
                              cutout-switch
                              cutout-reset))
                 (screw-holes config))))

 (defn frame-layer [config]
   (difference (frame-layer-internal config)
               (screw-holes config)))

 (defn frame-layer-half [config]
   (let [{{top :max
           bottom :min} :plate-mirror-edge} config
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

 ;; for sound dampening
 (defn foam-layer [config]
   (let [outline (base-outline config)
         frame (scale [1.05 1.05 5]
                      (frame-layer-internal config))]
     (difference outline frame)
     frame))

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
    ;; Helper layer. Not intended for cutting holes anywhere, just to provide the tool path for engraving the rubber feet positions onto the bottom layer. The layer outline is there to help with alignment. 
    #_(translate [275 0 0] (rubber-feet-outline-layer config))
    (translate [-275 0 0] (top-layer config))
    (translate [-275 -150 0] (plate-layer-upper config))
    #_(translate [0 -150 0] (plate-layer-lower1 config))
    #_(translate [275 -150 0] (plate-layer-lower2 config))
    #_(translate [-275 -300 0] (frame-layer config))
    #_(translate [0 -300 0] (frame-layer config))
    #_(translate [275 -300 0] (bottom-layer config))
    #_(translate [-220 -210 0] (rotate (* PI 3/2) [0 0 1] (plate-layer-upper config)))
    #_(color [0.5 0.5 0.5] (translate [-100 150 -20] (cube 495 1000 1.5)))))

 (defn optimized-placement [config]
   ;; move to origin
   (let [{[x-offset _] :mirror-offset
          {top :max
           bottom :min} :plate-mirror-edge} config
         x-dist (* 2 (+ 115 x-offset))
         y-dist (- top bottom 14)]
     (union
      ;; wood outline
      (color [0.3 0.3 0.6 1] (map (fn [n] (translate [(* (/ x-dist 2) (+ n 0.5)) 122.5 -5]
                                                     (cube (dec (/ x-dist 2)) 245 1)))
                                  (range 7)))
      (translate [(* 2 x-dist) 350 0] (mirror [1 0 0] (rubber-feet-outline-layer config)))
      #_(translate [(* 4 x-dist) 350 0] (mirror [1 0 0] (foam-layer config)))
      ;; layers
      (translate [x-dist  (- y-dist 71) 0]
                 (union
                  ;; move 5mm inwards
                  (translate [(+ (* -1 x-dist) 5) 0 0] (frame-layer-half config))
                  (translate [(* 0 x-dist) 0 0] (top-layer config))
                  (translate [(* 1 x-dist) 0 0] (bottom-layer config))
                  (translate [(* 2 x-dist) 0 0] (mirror [1 0 0] (frame-layer config)))

                  (translate [(* 0.5 x-dist) y-dist 0]
                             (translate [(* -1 x-dist) 0 0] (plate-layer-upper config))
                             ;; mirror asymmetric layers. Cut everything "from the bottom", so rubber feet outlines can be engraved onto the bottom layer.
                             (translate [(* 0 x-dist) 0 0] (mirror [1 0 0] (plate-layer-lower1 config)))
                             (translate [(* 1 x-dist) 0 0] (mirror [1 0 0] (plate-layer-lower2 config)))
                             ;; move 5mm inwards
                             (translate [(+ (* 2 x-dist) -5) 0 0]  (mirror [1 0 0] (frame-layer-half config)))))))))


 (defn create-multi-model [config]
   (let [variant-config (merge config config-variant)
         variant (create-model variant-config)
         base-model (create-model config)]
     (union base-model
            #_(translate [0 150 0] variant)
            (translate [0 0 0] (all-layers config)))))


 (defn run [config]
   (let [{{fa :fa
           fn :fn
           fs :fs} :quality} config]
     (binding [scad-clj.model/*fa* fa
               scad-clj.model/*fn* fn
               scad-clj.model/*fs* fs]
       (spit "things/mk5/foam.scad" (write-scad (foam-layer config)))
       (spit "things/mk5/azelusmk5.scad"
             (write-scad (create-multi-model config)))
       (spit "things/mk5/all.scad" (write-scad (project (all-layers config))))
       ;; space-efficient placement for laser cutting
       (spit "things/mk5/optimized.scad" (write-scad (optimized-placement config))))))


 (run base-config)