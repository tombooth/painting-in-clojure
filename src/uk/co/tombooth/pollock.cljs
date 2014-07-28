(ns uk.co.tombooth.pollock
  (:require [quil.core :as q :include-macros true]))

;; ---
;; title: Painting by Clojure
;; ...

;; Learning Clojure by build a digital Jackson Pollock.


;; <canvas id="pollock"></canvas>
;; <button id="add">Add stroke</button>
;; <button id="fill">Fill canvas</button>


;; Dimensions of space

(def space [8   ;; width
            5   ;; height
            6]) ;; depth

(def pixels-per-metre 100)

;; How would you convert from metres to pixels?

(defn metres-to-pixels [metres]
  (* metres pixels-per-metre))

;; Mainly want to be working in pixels so store the space in pixels

(def space-in-pixels (map metres-to-pixels space))

;; We need to pick a place to start the stroke. It will be somewhere
;; inside of the space

(defn starting-point []
  (let [[width height depth] space]
    [(rand width)
     (rand height)
     (rand depth)]))


;; To start off with lets assume that the paint is falling and has 0
;; velocity of its own when it starts from the brush

;; We need to know gravity, measured in metres per second
(def gravity [0 -9.8 0])


;; we need to know the amount of time it is going to take the paint to
;; fall, which we can work out based on the y dimension alone

;; we know the initial position and velocity, final position and
;; acceleration and we need to find the time. We cant use the equation
;; r = r0 + v0 * t + at^2/2
;; we need to use the quadratic equation to solve for t
;;
;; rearranged we get
;;   at^2 + 2v0t + 2r0 - 2r = 0
;; in quad equation:
;;   a = a
;;   b = 2v0
;;   c = 2r0 - 2r
;;     r (final position) is always going to be 0 so,
;;   c = 2r0

(defn time-to-canvas [position velocity acceleration]
  (let [a acceleration
        b (* 2 velocity)
        c (* 2 position)
        discriminant (- (* b b) (* 4 a c))
        minus-b (- 0 b)
        add-sqrt (/ (+ minus-b (Math/sqrt discriminant)) (* 2 a))
        minus-sqrt (/ (- minus-b (Math/sqrt discriminant)) (* 2 a))]
    (max add-sqrt minus-sqrt)))

;; now we can get the time we need to be able to derive the final
;; velocity and position of any dimension

;; v = at + v0
(defn velocity-at [time initial-velocity acceleration]
  (+ (* acceleration time) initial-velocity))

;; for position we can use the same equation we used to derive the
;; time

(defn position-at [time initial-position initial-velocity acceleration]
  (+ initial-position
     (* initial-velocity time)
     (/ (* acceleration time time) 2)))

;; we can now link these up into a function to get the final position
;; and velocity for a point

(defn project-point [position velocity]
  (let [[i j k]            position
        [vi vj vk]         velocity
        [ai aj ak]         gravity
        
        time               (time-to-canvas j vj aj)
        
        projected-position [(position-at time i vi ai)
                            0  ;; we don't need to calculate as it
                               ;; should be 0, on the canvas
                            (position-at time k vk ak)]
        
        projected-velocity [(velocity-at time vi ai)
                            (velocity-at time vj aj)
                            (velocity-at time vk ak)]]
    [projected-position
     projected-velocity]))


;; splatter
;; now that we know the impact velocity of a point, we know need to
;; work out whether that impact should splatter

;; we need to work out the impact force as a scalar so that we can
;; define a cut off

;; this will require working out the absolute value of a vector. We do
;; this by summing up the squares of the dimensions and then taking th root

(defn vector-absolute [vector]
  (Math/sqrt (reduce + (map #(* % %) vector))))

;; need something about why it is ok this is velocity and not
;; acceleration, maybe because it is the relative acceleration of the
;; imapct of the two objects (paint has velocity, cavnas doesn't so
;; accel = velocity?)

(defn impact-force [mass velocity]
  (* (vector-absolute velocity) mass))

;; we need to work out if an impact should splatter off some of its
;; paint. This should include some randomess as it is likely a
;; consecutive points will all splatter and including some randomness
;; will make it look at little less 'generated'

(def min-impact-force-for-splatter 50)

(defn does-impact-splatter? [mass velocity]
  (and (> (impact-force mass velocity) min-impact-force-for-splatter)
       (> (rand) 0.8)))

;; if an impact splatters then we will need to bounce its velocity
;; vector as this is the direction it will exit its current position

;; the equation to bounce a vector, V, off a plane with normal, N, is:
;; needs rewriting http://mathworld.wolfram.com/Reflection.html
;; 
;;  N is the normal vector of the plane
;;  R is the reflected vector
;; 
;;  R = V - (2 * (V.N) * N)

(def canvas-normal [0 1 0])

;; dot product is he sum of the multiples of the dimensions

(defn dot-product [vector1 vector2]
  (reduce + (map * vector1 vector2)))

(defn vector-subtraction [vector1 vector2]
  (map - vector1 vector2))

(defn vector-multiply-by-constant [vector constant]
  (map #(* % constant) vector))


;; blah? need a better name for this variable

(defn bounce-vector [vector normal]
  (let [vector-dot-normal (dot-product vector normal)
        blah (vector-multiply-by-constant normal (* 2 vector-dot-normal))]
    (vector-subtraction vector blah)))


;; splatter will never take all of the paint and velocity with it so
;; we need a dampening constant

(def splatter-dampening-constant 0.7)

(defn splatter-vector [velocity]
  (let [bounced-vector (bounce-vector velocity canvas-normal)]
    (vector-multiply-by-constant bounced-vector
                                 splatter-dampening-constant)))


;; doing paths
;; having all the code for dealing with a point is all well and good,
;; but we need to be dealing with paths

;; I've had a play with path generation before and besier curves gave
;; a nice output

;; De Casteljau's algorithm is the way we shall be coming up with
;; paths

;; first off we need to generate the anchor points for the curve

;; to do this we will need to be able to:
;;   - get a random number between two points (for distance, steps)
;;   - get a random direction
;;   - add vectors

(defn random-between [lower-bound upper-bound]
  (+ lower-bound (rand (- upper-bound lower-bound))))

;; need some blurb about this algo, hopefully find something from the asimuth

(defn random-unit-vector []
  (let [asimuth (* (rand) 2 Math/PI)
        k (- (rand 2) 1)
        a (Math/sqrt (- 1 (* k k)))
        i (* (Math/cos asimuth) a)
        j (* (Math/sin asimuth) a)]
    [i j k]))

(defn vector-add [vector1 vector2]
  (map + vector1 vector2))

;; need to walk between a start and end position in a random fashion

(defn random-vector-between [lower upper]
  [(random-between lower upper)
   (random-between lower upper)
   (random-between lower upper)])

(defn random-path [position step-vector bounds]
  (cons position
        (lazy-seq (random-path (vector-add (vector-add position step-vector)
                                           (random-vector-between (- 0 bounds) bounds))
                               step-vector bounds))))

;; what is an anchor point for a bezier curve?
;; added min/max steps to try and guide whether this is a flick, as
;; well as the variation in random positions

(defn anchor-points [position min-distance max-distance min-steps max-steps variation]
  (let [direction       (random-unit-vector)
        distance        (random-between min-distance max-distance)
        steps           (random-between min-steps max-steps)
        step-vector     (vector-multiply-by-constant direction (/ distance steps))
        random-positions (take steps (random-path position step-vector variation))
        end-position    (vector-add position
                                    (vector-multiply-by-constant step-vector steps))]
    (conj (vec random-positions) end-position)))


;; de casteljau code ripped from pollock, needs a REWRITE and a whole
;; bunch of explanation

(defn recur-relation [t a b]
  (+ (* t b) (* a (- 1 t))))

(defn for-component [t component-vals]
  (if (= (count component-vals) 1)
    (first component-vals)
    (for-component t
      (map #(recur-relation t %1 %2) component-vals (rest component-vals)))))

(defn for-t [t components]
  (map #(for-component t %) components))

(defn de-casteljau [control-points step-amount]
  (let [x-vals (map first control-points)
        y-vals (map second control-points)
        z-vals (map #(nth % 2) control-points)
        points (map #(for-t % [x-vals y-vals z-vals]) (range 0 1 step-amount))]
    points))

;; this can generate paths that go below the canvas, we should set
;; these to 0 as it is the equivalent of painting on the canvas

(defn ensure-above-canvas [path]
  (map (fn [[i j k]] [i (if (< j 0) 0 j) k]) path))


;; all the points along the generated path should have an associated
;; velocity. To start with we can generate a linear velocity along the
;; path, given a randomised total time to traverse the path and the
;; total length of the path.

;; In order to calculate the length of the paths, we will want to do
;; something similar to a map but with pairs of values. Using this we
;; can take two points, calculate the distance between them and then
;; sum all the distances

(defn map-2 [f coll]
  (when-let [s (seq coll)]
    (let [s1 (first s)
          s2 (second s)]
      (if (not (nil? s2))
        (cons (f (first s) (second s)) (map-2 f (rest s)))))))

;; in order to find the distance between two points we need subtract
;; the two vectors, square and sum the resultant dimensions and then
;; take the root. (https://en.wikipedia.org/wiki/Euclidean_distance)

(defn vector-multiply [vector1 vector2]
  (map * vector1 vector2))

(defn distance-between-points [point1 point2]
  (let [difference-vector (vector-subtraction point1 point2)
        summed-vector (reduce + (vector-multiply difference-vector difference-vector))]
    (Math/sqrt summed-vector)))

(defn path-length [path]
  (reduce + (map-2 distance-between-points path)))


(defn vector-divide-by-const [vector const]
  (map #(/ % const) vector))

(defn velocity-between [point1 point2 total-time total-distance]
  (let [difference-vector (vector-subtraction point1 point2)
        time-between (* total-time (/ (distance-between-points point1 point2)
                                      total-distance))]
    (vector-divide-by-const difference-vector time-between)))

;; this calculation will leave off the last points velocity, so for
;; now we can just set it to 0

(defn path-velocities [path total-time]
  (let [total-distance   (path-length path)
        number-of-points (count path)]
    (conj (vec (map-2 #(velocity-between %1 %2
                                         total-time
                                         total-distance)
                      path))
          [0 0 0])))

;; as well as the velocity at each point along the path, we also need
;; how much paint there is falling. Again to keep life simple we are
;; going to model this as a linear flow along the path with there
;; always being no paint left.

(defn path-masses [path initial-mass]
  (let [number-of-points (count path)
        step (- 0 (/ initial-mass number-of-points))]
    (take number-of-points (range initial-mass 0 step))))


;; now we need to assemble all of the above functions into something
;; that will work out everything

(defn do-the-things []
  (let [position       (starting-point)
        total-time     (random-between 1 5)
        path           (ensure-above-canvas (de-casteljau (anchor-points position 0.1 2 3 15 0.4) 0.005))
        velocities     (path-velocities path total-time)
        masses         (path-masses path (random-between 5 30))
        projected-path (map #(project-point %1 %2) path velocities)
        splatter       (map (fn [[position velocity] mass]
                              (if (does-impact-splatter? mass velocity)
                                [position (splatter-vector velocity) (* mass splatter-dampening-constant)]
                                nil))
                            projected-path masses)
        projected-splatter (map (fn [[position velocity mass :as point]]
                                  (if (nil? point)
                                    nil
                                    (conj (vec (project-point position velocity)) mass)))
                                splatter)]
    {:air path
     :path (map #(conj %1 %2) projected-path masses)
     :splatter (filter #(not-any? nil? %) (partition-by nil? projected-splatter))}))


;; lets draw these things

(def sketch-size
  (let [[width _ height] space-in-pixels]
    [width height]))

(defn position-to-pixel [[i j k]]
  [(Math/floor (metres-to-pixels i))
   (Math/floor (metres-to-pixels k))])


(defn draw-path [path]
  (doall
   (map-2 (fn [[position1 _ mass] [position2 _ _]]
            (q/stroke-weight mass)
            (apply q/line (concat (position-to-pixel position1) (position-to-pixel position2))))
          path)))

(defn draw-splats [path]
  (doall (map (fn [[position _ mass]]
                (q/stroke-weight mass)
                (apply q/point (position-to-pixel position)))
              path)))

(defn make-shape [& any]
  (q/with-sketch (q/get-sketch-by-id "pollock")
    (let [{:keys [path splatter] :as path-hash} (do-the-things)]
      (.log js/console (clj->js path-hash))
      (q/stroke (q/color (rand-int 256) (rand-int 256) (rand-int 256)))
      (draw-path path)
      (doall (map draw-splats splatter)))))

(defn draw []
  (q/background 255)
  (q/fill 0))

(q/defsketch pollock
  :setup draw
  :host "pollock"
  :size sketch-size)

(.addEventListener (.querySelector js/document "#add")
                   "click"
                   make-shape)

(def interval-ref (atom nil))
(def fill-count (atom 0))
(.addEventListener (.querySelector js/document "#fill")
                   "click"
                   (fn [e]
                     (reset! interval-ref
                             (js/setInterval (fn []
                                               (if (> @fill-count 500)
                                                 (js/clearInterval @interval-ref)
                                                 (do (make-shape) (swap! fill-count inc)))) 100))))

