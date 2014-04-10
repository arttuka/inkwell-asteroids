(ns inkwell-asteroids.core
  (:require [quil.core :refer :all]
            inkwell.core

            [inkwell-asteroids.util :refer :all]))

(def window-width 1200)
(def window-height 800)
(def window-size [window-width window-height])
(def mirrorings (for [w [window-width (- window-width) 0]
                      h [window-height (- window-height) 0]]
                  [w h]))

(defn new-asteroid [position type]
  {:position position
   :type type
   :rotation 0
   :rotation-speed (case type
                     :big (rand-float 0 0.1)
                     :small (rand-float 0.1 0.2))
   :speed (case type
            :big [(rand-float -1.5 1.5) (rand-float -1.5 1.5)]
            :small [(rand-float -2.5 2.5) (rand-float -2.5 2.5)])
   :size (case type
           :big 50
           :small 18)})

(defn new-bullet [ship-position side rotation]
  {:position (case side
               :left (rotated ship-position [-45 -20] rotation)
               :right (rotated ship-position [45 -20] rotation))
   :rotation rotation
   :rotation-speed 0
   :speed (rotated [0 -30] rotation)
   :size 0
   :ttl 50})

;; This is the state that the sketch starts with. You can return the sketch to
;; this state by pressing F12. See handle-event for how it's done!
(def initial-state {:player {:position [300 400]
                             :speed [0 0]
                             :rotation HALF-PI
                             :rotation-speed 0
                             :accelerating false
                             :turning nil
                             :side :left
                             :size 45}
                    :asteroids [(new-asteroid [300 100] :big)
                              (new-asteroid [500 300] :big)
                              (new-asteroid [1000 700] :big)
                              (new-asteroid [800 200] :big)]
                    :bullets []
                    :tick-count 0
                    :ai false})

;; These functions return new states based on the old state (and some additional
;; arguments). Note that they are pure functions: they do not *mod

(defn move-item [{:keys [position speed rotation rotation-speed] :as item}]
  (let [[new-x new-y] (+v position speed)
        new-rotation (mod (+ rotation rotation-speed) TWO-PI)
        ttl (some-> item :ttl dec)]
    (cond-> (assoc item :position [(mod new-x window-width) (mod new-y window-height)]
                        :rotation new-rotation)
            ttl (assoc :ttl ttl))))

(defn update-tick-count [state]
  (update-in state [:tick-count] inc))

(defn update-asteroids [state]
  (update-in state [:asteroids] #(map move-item %)))

(defn set-player-speed-and-rotation [state new-speed new-rotation-speed]
  (-> state
    (assoc-in [:player :speed] new-speed)
    (assoc-in [:player :rotation-speed] new-rotation-speed)))

(defn set-player-speed [state]
  (let [speed (get-in state [:player :speed])
        rotation (get-in state [:player :rotation])
        rotation-speed (get-in state [:player :rotation-speed])
        turning (get-in state [:player :turning])
        accelerating (get-in state [:player :accelerating])]
    (if (:newtonian state)
      (let [new-speed (if accelerating
                        (+v speed (rotated [0 -0.2] rotation))
                        speed)
            new-rotation-speed (case turning
                                 :left (- rotation-speed 0.001)
                                 :right (+ rotation-speed 0.001)
                                 rotation-speed)]
        (set-player-speed-and-rotation state new-speed new-rotation-speed))
      (let [scalar-speed (vector-length speed)
            new-scalar-speed (if accelerating
                               (min 10 (+ scalar-speed 0.2))
                               (max 0 (- scalar-speed 0.2)))
            new-rotation-speed (case turning
                                 :left (* new-scalar-speed -0.01)
                                 :right (* new-scalar-speed 0.01)
                                 0)
            new-speed (rotated [0 (- new-scalar-speed)] rotation)]
        (set-player-speed-and-rotation state new-speed new-rotation-speed)))))

(defn move-player [state]
  (update-in state [:player] move-item))

(defn move-bullets [state]
  (update-in state [:bullets] #(map move-item %)))

(defn turn-player [state direction]
  (if (:player state)
    (assoc-in state [:player :turning] direction)
    state))

(defn start-stop-player [state accelerating]
  (if (:player state)
    (assoc-in state [:player :accelerating] accelerating)
    state))

(defn shoot [state]
  (let [{:keys [side rotation position]} (:player state)
        bullet (new-bullet position side rotation)
        new-side (case side
                   :left :right
                   :right :left)]
    (-> state
      (update-in [:bullets] conj bullet)
      (assoc-in [:player :side] new-side))))

(defn cull-bullets [state]
  (update-in state [:bullets] #(remove (comp zero? :ttl) %)))

(defn collides? [item1 item2]
  (let [s1 (:size item1)
        s2 (:size item2)]
    (> (+ s1 s2) (distance (:position item1) (:position item2)))))

(defn some-collides [item coll]
  (some (partial collides? item) coll))

(defn new-asteroids-and-bullets [state]
  (let [new-asteroids (apply concat (for [asteroid (:asteroids state)]
                                    (if (some-collides asteroid (:bullets state))
                                      (if (= :big (:type asteroid))
                                        [(new-asteroid (:position asteroid) :small)
                                         (new-asteroid (:position asteroid) :small)
                                         (new-asteroid (:position asteroid) :small)]
                                        [])
                                      [asteroid])))
        new-bullets (remove #(some-collides % (:asteroids state)) (:bullets state))]
    (assoc state :bullets new-bullets
                 :asteroids new-asteroids)))

(defn check-collisions [state]
  (cond-> (new-asteroids-and-bullets state)
    (some-collides (:player state) (:asteroids state)) (dissoc :player)))

(defn move-by [item v]
  (update-in item [:position] +v v))

(defn mirror-asteroid [asteroid]
  (map (partial move-by asteroid) mirrorings))

(defn sector [position begin end length]
  {:position position
   :begin begin
   :end end
   :length length})

(defn sector-l [position rotation]
  (sector position (mod (- rotation HALF-PI) TWO-PI) (mod (- rotation QUARTER-PI) TWO-PI) 200))

(defn sector-fl [position rotation]
  (sector position (mod (- rotation QUARTER-PI) TWO-PI) rotation 400))

(defn sector-cl [position rotation]
  (sector position (mod (- rotation QUARTER-PI) TWO-PI) rotation 200))

(defn sector-fr [position rotation]
  (sector position rotation (mod (+ rotation QUARTER-PI) TWO-PI) 400))

(defn sector-cr [position rotation]
  (sector position rotation (mod (+  rotation QUARTER-PI) TWO-PI) 200))

(defn sector-r [position rotation]
  (sector position (mod (+ rotation QUARTER-PI) TWO-PI) (mod (+ rotation HALF-PI) TWO-PI) 200))

(defn asteroid-in-sector? [{:keys [position begin end length] :as sec} asteroids]
  (let [mirrored-asteroids (map :position (mapcat mirror-asteroid asteroids))]
    (if (< begin end)
      (some #(and (> length (distance position %))
                  (< begin (mod (vector-angle (v position %)) TWO-PI) end))
            mirrored-asteroids)
      (or (asteroid-in-sector? (assoc sec :end TWO-PI) asteroids)
          (asteroid-in-sector? (assoc sec :begin 0) asteroids)))))

(defn ai [state]
  (let [{:keys [position rotation]} (:player state)
        asteroids (:asteroids state)
        [l fl cl cr fr r] [(asteroid-in-sector? (sector-l position rotation) asteroids)
                           (asteroid-in-sector? (sector-fl position rotation) asteroids)
                           (asteroid-in-sector? (sector-cl position rotation) asteroids)
                           (asteroid-in-sector? (sector-cr position rotation) asteroids)
                           (asteroid-in-sector? (sector-fr position rotation) asteroids)
                           (asteroid-in-sector? (sector-r position rotation) asteroids)]]
    (if (or l fl fr r)
      (cond
        (or cl cr) (if (not cl) :left :right)
        (< (count-true [l fl]) (count-true [r fr])) :left
        :else :right)
      :straight)))

(defn turn-with-ai [state]
  (if (:ai state)
    (assoc-in state [:player :turning] (ai state))
    state))

;; Like the functions above, handle-event is a pure function that returns a new
;; state based on the old state and an event. In fact, it mostly just applies
;; the above functions to the old state.
(defn handle-event [state event]
  (case (:type event)
    :key-pressed (condp = (:key-name event)
                   ;; When F12 is pressed, we return initial-state. This gives
                   ;; us a simple way to reset the sketch to a known state.
                   :f12 initial-state
                   :up (start-stop-player state true)
                   :left (turn-player state :left)
                   :right (turn-player state :right)
                   :a (-> state
                        (update-in [:ai] not)
                        (assoc-in [:player :accelerating] (not (:ai state))))
                   :g (update-in state [:newtonian] not)
                   (keyword " ") (shoot state)
                   ;; When any other key is pressed, we don't want to change the
                   ;; state, so we just return the old state.
                   state)
    :key-released (case (:key-name event)
                    :up (start-stop-player state false)
                    :left (turn-player state nil)
                    :right (turn-player state nil)
                    state)
    :tick (cond-> state
            :always update-tick-count
            :always update-asteroids
            (:player state) set-player-speed
            (:player state) move-player
            :always move-bullets
            :always cull-bullets
            (:player state) check-collisions
            (:player state) turn-with-ai)
    state))

;; See directories:
;; - resources/platformer
;; - resources/space-shooter
;; for more images by Kenney Vleugels (www.kenney.nl)
(def bg-image (delay (load-image "space-shooter/background.jpg")))
(def player-image {nil (delay (load-image "space-shooter/player.png"))
                   :straight (delay (load-image "space-shooter/player.png"))
                   :left (delay (load-image "space-shooter/playerLeft.png"))
                   :right (delay (load-image "space-shooter/playerRight.png"))})
(def asteroid-image-big (delay (load-image "space-shooter/meteorBig.png")))
(def asteroid-image-small (delay (load-image "space-shooter/meteorSmall.png")))
(def jet-image (delay (load-image "space-shooter/jetFlame1.png")))
(def bullet-image (delay (load-image "space-shooter/laserRed.png")))

(defn draw-component [c img & [cs]]
  (push-matrix)
  (apply translate (:position c))
  (rotate (:rotation c))
  (image @img 0 0)
  (doseq [[comp img] cs]
    (draw-component comp img))
  (pop-matrix))

(def jets
  [[{:position [-18 45]
     :rotation 0}
    jet-image]
   [{:position [-28 42]
    :rotation 0}
    jet-image]
   [{:position [18 45]
    :rotation 0}
    jet-image]
   [{:position [28 42]
    :rotation 0}
    jet-image]])

(defn draw-sector [{:keys [position begin end length]}]
  (let [start (- begin HALF-PI)
        stop (- end HALF-PI)
        length (* 2 length)]
    (if (< start stop)
      (arc (first position) (second position) length length start stop)
      (arc (first position) (second position) length length start (+ TWO-PI stop)))))

;; draw is an impure function that takes a state, and calls Quil's drawing
;; functions to update the screen to match the state.
(defn draw [state]
  (background 0 0 0)
  (image-mode :center)
  (when (:player state)
    (draw-component (:player state) (player-image (get-in state [:player :turning]))
                    (when (get-in state [:player :accelerating])
                      jets)))
  (doseq [asteroid (:asteroids state)
          :let [image (case (:type asteroid)
                        :big asteroid-image-big
                        :small asteroid-image-small)]]
    (draw-component asteroid image))
  (when (:ai state)
    (let [{:keys [position rotation]} (:player state)
          sectors [(sector-l position rotation)
                   (sector-fl position rotation)
                   (sector-cl position rotation)
                   (sector-cr position rotation)
                   (sector-fr position rotation)
                   (sector-r position rotation)]]
      (doseq [s sectors
              m mirrorings]
        (if (asteroid-in-sector? s (:asteroids state))
          (fill 255 0 0 50)
          (fill 255 255 255 30))
        (draw-sector (move-by s m)))))
  (doseq [bullet (:bullets state)]
    (draw-component bullet bullet-image)))

(defn make-sketch! []
  (inkwell.core/make-sketch! {:title "Inkwell Playground"
                              :renderer :p2d
                              :draw #'draw
                              :size window-size
                              :handle-event #'handle-event
                              :initial-state initial-state}))
