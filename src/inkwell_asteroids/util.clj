(ns inkwell-asteroids.util
  (:require [quil.core :refer :all]))

(defn rand-float
  [min max]
  (+ min (* (- max min) (rand))))

(defn vector-length [[x y]]
  (sqrt (+ (* x x) (* y y))))

(defn +v [& vectors]
  (apply mapv + vectors))

(defn v [[x1 y1] [x2 y2]]
  [(- x2 x1)
   (- y2 y1)])

(defn vector-angle [[x y]]
  (atan2 x (- y)))

(defn count-true [coll]
  (count (filter identity coll)))

(defn rotated
  ([[x y] rotation]
    (rotated [0 0] [x y] rotation))
  ([[x0 y0] [x1 y1] rotation]
    (let [s (sin rotation)
          c (cos rotation)
          dx (- (* x1 c) (* y1 s))
          dy (+ (* x1 s) (* y1 c))]
      [(+ x0 dx) (+ y0 dy)])))

(defn squared-distance [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)]
    (+ (* dx dx) (* dy dy))))

(defn distance [[x0 y0] [x1 y1]]
  (sqrt (squared-distance x0 y0 x1 y1)))
