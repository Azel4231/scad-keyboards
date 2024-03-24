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

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(def redpoll {:keycap-dimensions {:x 18 :y 18 :z 11}
              :cutout-dimensions {:x 13.96 :y 13.96}
              :key-distance {:x 19.0 :y 19.0}
              :keycap-kerf 0.75
              :mirror false

              :opening-angle (deg2rad 14)
              :plate-thickness 1.5
              :layer-thickness 2
              :case-wall-thickness 10
              :plate-border 2
              ;; :plate-mirror-edge {:min 22 :max 101 :x 3}  ;; Obere Kante gerade weiter führen
              :plate-mirror-edge {:min 38 :max 96.1 :x 3}  ;; where the halves touch
              ;; :plate-mirror-edge {:min 22 :max 84 :x 3}  ;; parallel zur Unterkante
              :controller {:x 12 :y 85 :z -1 
                           :w 18 :d 23 :h 1.5} ;; Seeed XIAO BLE
              :battery {:x 15 :y 26 :z -4
                        :w 17 :d 40 :h 5.8}
              ;; :battery {:x 15 :y 25 :z 3 :w 17 :d 40 :h 6.5}   ;; https://www.mylipo.de/Lipo-Akku-240mAh-37V-25C-50C-BLADE-mcp-x-nano-QX-3D
              ;; {:w 20 :d 35 :h 6.5}  https://www.mylipo.de/Lipo-Akku-250mAh-37V-25C-50C-GENIUS-CP-WALKERA
              ;; {:w 20 :d 35 :h 5}  https://www.mylipo.de/Lipo-Akku-180mAh-37V-25C-50C
              ;; {:w 11 :d 42 :h 5.8}  https://www.mylipo.de/Lipo-Akku-150mAh-37V-25C-50CJST-PH125-2-Pin-Stecker
              
              :screwhole-radius 0.6
              :screwhole-positions [[4 2.8] [4 -0.8] [1.2 4.4]]
              :screwhole-matrix []
              :strut-positions [[4 2.9] [4 -0.9]]
              
              
              :matrix {:offset [38 12]
                       :clusters [{:rows 3
                                   :cols 6
                                   :staggers [[0 5] [0 7] [0 15] [0 11] [0 1] [0 0]]
                                   :offset [0 0]
                                   :additional-positions [[1 3]]}  ;; fingers
                                  {:rows 1 
                                   :cols 4
                                   :offset [-19 -19] ;; -1u in both x and y direction
                                   :staggers [[0 0] [0 0] [0 0] [0 4]]}  ;; thumbs
                                  ]
                       }
              }
              )

(def redpoll-nano {
                   :controller {:x 10.2 :y 88 :z -1 :w 18 :d 33.5 :h 1.5}  ;; Nice!Nano
                   })


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
         {[offset-x offset-y] :offset} :matrix} config
        [col row] pos
        [stagger-x stagger-y] stagger]
    (->> shape
         ;; row, col unit: 1u (one key)
         ;; stagger unit: mm
         (translate [(+ (* col dist-x)
                        offset-x
                        stagger-x) 
                     (+ (* row dist-y) 
                        offset-y
                        stagger-y)
                     0])
         (rotate angle [0 0 1])
         )))

(defn place-shape-at [config shape cluster-def]
  (let [{rows :rows
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
                (place-shape config 
                             shape 
                             (map + 
                                  (get staggers col [0 0])
                                  offset)
                             pos))))))

(defn place-at-key-positions [config shape]
  (let [{{clusters :clusters} :matrix} config]
    (->> clusters
         (mapcat (partial place-shape-at config shape))
         )))

(defn single-cutout [config]
  (let [{{cutout-width :x
          cutout-height :y} :cutout-dimensions} config]
    (cube cutout-width cutout-height 25)))

(defn controller [config additional-height]
  (let [{o-angle :opening-angle
         {x :x
          y :y
          z :z
          w :w
          d :d
          h :h} :controller} config]
    (->> (cube w d (+ h additional-height))
         #_(rotate o-angle [0 0 1])
         (translate [x y z])
         (color [0.3 0.3 0.8 1]))))

(defn battery [config additional-height]
  (let [{o-angle :opening-angle
         {x :x
          y :y
          z :z
          w :w
          d :d
          h :h} :battery} config]
    (->> (cube w d (+ h additional-height))
         (rotate o-angle [0 0 1])
         (translate [x y z])
         (color [0.3 0.8 0.3 1]))))


(defn average [& vals]
  (/ (reduce + vals) (count vals)))

(defn base-plate-shape [config]
  (let [{{dim-x :x
          dim-y :y} :cutout-dimensions
         {mirror-y-min :min
          mirror-y-max :max
          mirror-x :x} :plate-mirror-edge} config
        mirror-edge-helper (translate [mirror-x (average mirror-y-max mirror-y-min) 0]
                                      (cube 0.01
                                            (- mirror-y-max mirror-y-min)
                                            0.01))
        dummy (cube dim-x dim-y 0.01)]
    (->> mirror-edge-helper
         (conj (place-at-key-positions config dummy))
         (apply hull)
         (color [0.98 0.92 0.6 1]))))

(defn plate-layer [config]
  (let [{{dim-x :x
          dim-y :y} :cutout-dimensions
         {dist-x :x
          dist-y :y} :key-distance
         plate-border :plate-border
         plate-thickness :plate-thickness} config
        base-plate (base-plate-shape config)
        case-cut-x (-> (- dist-x dim-x)
                       (+ dist-x))
        case-cut-y (-> (- dist-y dim-y)
                       (+ dist-y))
        case-cutout (cube case-cut-x case-cut-y 10)]
    (binding [scad-clj.model/*fa* 2
              scad-clj.model/*fn* 10  ;; low 10, medium 30, high 50
              scad-clj.model/*fs* 0.1]
      (color [0.8 0.8 0.8 1] (-> base-plate
                                   (difference (map (partial place-shape config case-cutout [0 5]) [[-1 0] [-1 1] [-1 2] [-1 3] [-1 4]]))
                                   (difference (map (partial place-shape config case-cutout [0 5]) [[0 3] [0 4]]))
                                   (minkowski (cylinder plate-border plate-thickness))
                                   (difference (place-at-key-positions config (single-cutout config)))
                                   (difference (controller config 10)))))))

(defn case-outline [config]
  (let [{case-wall-thickness :case-wall-thickness
         plate-thickness :plate-thickness
         plate-border :plate-border} config
        plate-shape (base-plate-shape config)
        plate-cutout (-> (base-plate-shape config)
                         (minkowski (cylinder plate-border 10)))]
    (color [0.98 0.92 0.6 1] (-> plate-shape
                                 (minkowski (cylinder case-wall-thickness plate-thickness))
                                 (difference plate-cutout)
                                 ))
    ))

#_(defn base-plate [config]
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
        base-plate (base-plate-shape config)
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
        base-plate (base-plate-shape config)
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
         {bat-x :x
          bat-y :y
          bat-z :z
          bat-w :w
          bat-d :d
          bat-h :h} :battery
         do-mirror :mirror} config
        keycaps (place-at-key-positions config (keycap config))
        mcu (controller config 0)
        battery (battery config 0)
        explode 1]
    (mirror-halves
     do-mirror
     (translate [1 0 0]
                (union
                 (translate [0 0 (* 2 explode)] mcu)
                 (translate [0 0 (* 2 explode)] battery)
                 (translate [0 0 explode] keycaps)
                 (translate [0 0 (* plate-thickness explode)] (plate-layer config)) 
                 (translate [0 0 (* layer-thickness explode)] (case-outline config)) 
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
  (let [redpoll-model (create-model redpoll)
        nano-model (create-model (deep-merge redpoll redpoll-nano))
        ]
    (union redpoll-model
           (translate [0 140 0] nano-model)
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
