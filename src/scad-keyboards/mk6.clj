; check if nrepl version in ~/.lein/profilec.clj is up to date
; lein nrepl + connect to nrepl from IDE/Calva (or just jack-in)
; eval file
; open ./things/mk6/all.scad in openScad (will update automatically when re-evaluating)

 (ns scad-keyboards.mk6
   (:refer-clojure :exclude [use import])
   (:require [scad-clj.scad :refer [write-scad]]
             [scad-clj.model :refer [translate rotate scale intersection union hull difference cube color mirror cylinder project minkowski]]))

(def PI Math/PI)

(defn deg2rad [degrees]
  (* (/ degrees 180) PI))

(def base-config {:quality {:fa 2
                            :fn 40  ;; low 10, medium 30, high 50
                            :fs 0.1}
                  :opening-angle (deg2rad 14)
                  ;; usually 13.98 (wood only: 13.95, wood with varnish: 13.98)
                  :cutout-dimensions {:x 13.98 :y 13.98}
                  :key-distance {:x 19.0 :y 19.0}
                  ;; cutout for each keycap is key-distance + keycap-kerf
                  :keycap-kerf 0.75
                  :plate-thickness 1.5
                  ;; initially 3.5, make more room for errors
                  :plate-border {:top 4.5 :bottom 4.5 :inner 6 :outer 4.5}
                  :keycap-dimensions {:x 18 :y 18 :z 10}
                  ;; 86 to avoid holes between top and plate layer
                  :top {:y 86 :width 26}

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

                  ;;:controller {:w 18 :d 33.5 :h 3.0 :usbc-y 1.5 :usbc-z 0 :wall 3}  ;; nice!nano
                  :controller {:w 18 :d 21 :h 1.5
                               :usbc-y 1.5 ;; 1.5mm overhang (top of controller)
                               :usbc-z 1.5 ;; 1.5mm upwards (placed on top of pcb)
                               :wall 3}  ;; xiao-ble () 

                  :battery {:w 13 :d 31 :h 4.3 :wall 3 :pos [-0.9 0.2]}
                  ;;:battery {:w 17.5 :d 31 :h 4.3 :wall 3 :pos [-0.9 0.1]}  ;; 150mAh

                  :magnets {:additional-positions [[-0.88 2.8]
                                                   [-1.55 0.1]
                                                   [4.2 2.8]
                                                   [4.2 -0.8]]
                            :radius 3
                            :height 3}

                  :magnets-case {:additional-positions [[1.9 3.6]
                                                        [1.9 -1.2]]
                                 :radius 3
                                 :height 3}

                  :battery-cutout {:rows 3
                                   :cols 1
                                   :offset [-19.5 1.5]
                                   :dimensions [14 15]}

                  :matrix {:offset [33 -8]
                           :clusters {:finger-cluster {:rows 3
                                                       :cols 6
                                                       :additional-positions [[1 3]]
                                                       :offset [0 5]
                                                       ;; Split: +2 +8 -4 -10 -1
                                                       ;; MK5:   +3 +8 -5 -10 -3
                                                       ;; MK6: try more pinkey stagger (-11 -2)
                                                       :staggers [[0 0] [0 2] [0 10] [0 6] [0 -5] [0 -7]]}
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

;; -------------------------------------------------
;; utility functions
;; -------------------------------------------------

(defn place-in-cluster-single "Place shape at a single cluster position"
  [config shape cluster-def pos]
  (let [{{unit-x :x
          unit-y :y} :key-distance
         angle :opening-angle
         {[offset-x offset-y] :offset} :matrix} config
        [col row] pos
        {staggers :staggers
         offset :offset} cluster-def
        [stagger-x stagger-y] (map +
                                   (get staggers col [0 0])
                                   offset)]
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

(defn place-in-cluster "Place shape iterating over all positions in a single cluster"
  [config shape cluster-def]
  (let [cluster-def (merge default-cluster cluster-def)
        {rows :rows
         cols :cols
         additional :additional-positions} cluster-def
        positions (for [c (range cols)
                        r (range rows)]
                    [c r])
        positions (concat positions additional)]
    (->> positions
         (map (fn [[col _ :as pos]]
                (place-in-cluster-single config
                                         shape
                                         cluster-def
                                         pos))))))

(defn place-at-key-positions "Place shape iterating over all clusters"
  [config shape]
  (let [{{clusters :clusters} :matrix} config]
    (->> clusters
         vals
         (mapcat (partial place-in-cluster config shape)))))


(defn half "Cuts the shape on the x/z plane and removes everything on negative x side."
  [shape]
  (let [neg-x (translate [-500 0 0] (cube 1000 1000 1000))]
    (difference (union shape) neg-x)))

(defn mirror-halves "Unions the shape and mirrors it around the y/z plane. Cut off the shape's -x parts before mirroring"
  [shape]
  (let [clean-shape (half shape)]
    (union clean-shape
           (mirror [1 0 0] clean-shape))))

;; -------------------------------------------------
;; objects
;; -------------------------------------------------

(defn single-key-hole [config x-kerf y-kerf]
  (let [{{cutout-width :x
          cutout-height :y} :cutout-dimensions} config]
    (cube (+ cutout-width x-kerf) (+ cutout-height y-kerf) 10)))


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


(defn usb-c-cutout [config]
  (let [{{contr-d :d
          usbc-y :usbc-y
          usbc-z :usbc-z} :controller} config]
    (->> (cube 9 7 3)
         (translate [0 (/ (- contr-d 7) 2) 1.5]) ;; align with top (and bottom) of pcb
         (translate [0 usbc-y usbc-z]) ;; move according to offsets in controller dimensions
         )))

(defn controller-cutout
  ([config]
   (controller-cutout config 5))
  ([config h]
   (let [{{contr-w :w
           contr-d :d} :controller} config]
     (cube contr-w contr-d h))))

(defn controller-cutout-usbc [config]
  (union (controller-cutout config)
         (usb-c-cutout config)))

(defn magnets [config]
  (let [{magnet-cluster :magnets} config
        {radius :radius
         height :height} magnet-cluster]
    (place-in-cluster config
                      (cylinder radius height)
                      magnet-cluster)))

(defn magnets-case [config]
  (let [{magnet-cluster :magnets-case} config
        {radius :radius
         height :height} magnet-cluster]
    (place-in-cluster config
                         (cylinder radius height)
                         magnet-cluster)))

(defn battery [config]
  (let [{{w :w
          d :d
          h :h} :battery} config]
    (cube w d h)))

(defn cutout-controller-wires [config]
  (let [{{contr-w :w
          contr-d :d
          wall :wall} :controller} config]
    (translate [0 (- wall) 0] (hull (cube contr-w contr-d 5)
                                    ;; move down to center of controller
                                    (translate [0 -3 0]
                                               (cube (+ contr-w 5) (- contr-d 6) 5))))))
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



;; -------------------------------------------------
;; layers
;; -------------------------------------------------

(defn base-outline [config]
  (let [{plate-thickness :plate-thickness
         {top :top bottom :bottom inner :inner outer :outer} :plate-border
         {cap-x :x
          cap-y :y} :keycap-dimensions
         {{finger-cluster :finger-cluster
           thumb-cluster :thumb-cluster} :clusters} :matrix} config
        border-helper (translate [(/ (- outer inner) 2) (/ (- bottom top) 3) 0]
                                 (cube (+ cap-x inner outer)
                                       (+ cap-y top bottom)
                                       plate-thickness))
        bottom-helper (place-in-cluster-single config border-helper thumb-cluster [0 1])
        ;; align x with innermost thumb key (move the helper two units inward, see also comment below)
        top-border-helper (translate [-38 0 0] border-helper)
        ;; align y with topmost key ([1 3]) and its stagger
        top-helper (place-in-cluster-single config top-border-helper finger-cluster [1 3])
        ;;controller-helper (controller-cutout config plate-thickness)
        ;;mirror-top-helper (top-aligned-cube config top-width 0.01 plate-thickness)
        ]
    (->> border-helper
         (place-at-key-positions config)
         #_(cons controller-helper)
         (cons top-helper)
         (cons bottom-helper)
         (apply hull)
         #_(minkowski2 (cylinder plate-border 0.01))
         #_(mirror-halves)
         (half)
         (color [0.98 0.92 0.6 1]))))


(defn top-layer [config]
  (let [{{cap-x :x
          cap-y :y} :keycap-dimensions
         kerf :keycap-kerf} config
        base-outline (base-outline config)
        cap-cutouts (place-at-key-positions config
                                            (cube (+ cap-x (* 2 kerf))
                                                  (+ cap-y (* 2 kerf))
                                                  25))
        ;; TODO remove
        cap-cutouts (union cap-cutouts (magnets config))
        ]
    (difference base-outline
                (mirror-halves cap-cutouts))))

(defn plate-layer [config cutout-switch cutouts-other]
  (let [base-outline (base-outline config)
        cutouts (mirror-halves (place-at-key-positions config cutout-switch))]
    (union
     (difference base-outline
                 cutouts
                 cutouts-other))))

(defn plate-layer-upper [config]
  (let [{{{finger-cluster :finger-cluster} :clusters} :matrix
         {battery-pos :pos} :battery} config
        controller-cutout (place-in-cluster-single config
                                                   (color [0.4 0.4 0.4 1] (controller-cutout-usbc config))
                                                   finger-cluster
                                                   [0 3])
        ;; FIXME remove
        controller-cutout (translate [-1 2 0] controller-cutout)
        battery (place-in-cluster-single config
                                         (color [0.3 0.9 0.3 1] (battery config))
                                         finger-cluster
                                         battery-pos)
        case-magnets (translate [0 0 3] (magnets-case config))]
    (union (difference (plate-layer config
                                    (single-key-hole config 0 0)
                                    controller-cutout)
                       (magnets config))
           battery
           controller-cutout
           case-magnets)))


(defn plate-layer-lower1 [config]
  (let [power-switch-cutout (translate [17 2 0]
                                       (cube 7.2 10 5))
        cutout-controller (cutout-controller-wires config)
        cutout-usb-c (usb-c-cutout config)]
    (difference (plate-layer config
                             (single-key-hole config 0 1.2)
                             (union power-switch-cutout
                                    cutout-controller
                                    cutout-usb-c))
                (magnets config))))

(defn plate-layer-lower2 [config]
  (let [power-switch-cutout (translate [17 -3 0]
                                       (cube 7.2 5 5))
        cutout-controller (cutout-controller-wires config)]
    (plate-layer config
                 (single-key-hole config -0.5 -0.5)
                 (union power-switch-cutout
                        cutout-controller))))


(defn frame-layer [config]
  (let [base-outline (base-outline config)
        cutout (apply hull (place-at-key-positions config (single-key-hole config 0 0))) 
        cutout-controller (translate [0 65 0] (cube 28 30 5))
        cutout-switch (translate [18 75 0] (cube 12 8 5))]
    (difference base-outline
                cutout
                cutout-controller
                cutout-switch)))

(defn bottom-layer [config]
  (base-outline config))


(defn rubber-feet-outline-layer [config]
  (let [{screwhole-cluster :screwholes} config
        layer-outline (base-outline config)
        rubber-feet-indicator (->> (rubber-feet config)
                                   (scale [1 1 10]))
        screw-indicator (mirror-halves (place-in-cluster config
                                                         (cylinder 2 10)
                                                         screwhole-cluster))]
    (difference layer-outline
                rubber-feet-indicator
                screw-indicator)))



;; -------------------------------------------------
;; layer placement
;; -------------------------------------------------

(defn create-model [config]
  (let [{plate-thickness :plate-thickness} config
        keycaps (mirror-halves (place-at-key-positions config (keycap config)))
        explode 1
        layers [#_keycaps
                (top-layer config)
                (plate-layer-upper config)
                (plate-layer-lower1 config)
                (plate-layer-lower2 config)
                (frame-layer config)
                (frame-layer config)
                (bottom-layer config)
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
   (translate [0 -150 0] (plate-layer-lower1 config))
   (translate [275 -150 0] (plate-layer-lower2 config))
   (translate [-275 -300 0] (frame-layer config))
   (translate [0 -300 0] (frame-layer config))
   (translate [275 -300 0] (bottom-layer config))))

;; Place layers on a 100x25 cm^2 plywood sheet for space-efficient laser cutting
(defn optimized-placement [config]
  ;; move to origin
  (let [{{top :y} :top} config
        x-dist (* 2 (+ 115 27.5))
        y-dist (+ top 28)]
    (union
     ;; wood outline
     #_(color [0.3 0.3 0.6 1] (map (fn [n] (translate [(* (/ x-dist 2) (+ n 0.5)) 122.5 -5]
                                                      (cube (dec (/ x-dist 2)) 245 1)))
                                   (range 7)))
     (translate [(* 1 x-dist) 310 0] (mirror [1 0 0] (rubber-feet-outline-layer config)))
     ;; layers
     (translate [x-dist  (- y-dist 70) 0]
                (union
                 ;; move 5mm inwards
                 (translate [(+ (* -1 x-dist) 5) 0 0] (frame-layer config))
                 (translate [(* 0 x-dist) 0 0] (top-layer config))
                 (translate [(* 1 x-dist) 0 0] (bottom-layer config))
                 (translate [(* 2 x-dist) 0 0] (mirror [1 0 0] (frame-layer config)))

                 (translate [(* 0.5 x-dist) y-dist 0]
                            (translate [(* -1 x-dist) 0 0] (plate-layer-upper config))
                            ;; mirror asymmetric layers. Cut everything "from the bottom", so rubber feet outlines can be engraved onto the bottom layer.
                            (translate [(* 0 x-dist) 0 0] (mirror [1 0 0] (plate-layer-lower1 config)))
                            (translate [(* 1 x-dist) 0 0] (mirror [1 0 0] (plate-layer-lower2 config)))
                            ;; move 5mm inwards
                            (translate [(+ (* 2 x-dist) -5) 0 0]  (mirror [1 0 0] (frame-layer config)))))))))


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
      (spit "things/mk6/mk6.scad"
            (write-scad (create-multi-model config)))
      (spit "things/mk6/all.scad" (write-scad (project (all-layers config))))
      (spit "things/mk6/optimized.scad" (write-scad (project (optimized-placement config)))))))


(run base-config)