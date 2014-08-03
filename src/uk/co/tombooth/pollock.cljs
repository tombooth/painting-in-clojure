(ns uk.co.tombooth.pollock
  (:require [quil.core :as q :include-macros true]))

;; ---
;; title: Painting in Clojure
;; ...

;; Learning Clojure by building a digital Jackson Pollock.


;; <canvas id="pollock" style="width: 100%; border: 5px solid #eee"></canvas>
;; <button id="add">Add stroke</button>
;; <button id="fill">Fill canvas</button>



;; ## Setting the scene

;; We want to define some facts about the space that our digital
;; Pollock will work in. Clojure will let us define facts using one of
;; the following value types:
;;
;;    - Number
;;       + 102
;;       + 1.5
;;       + 1/2
;;    - String ""
;;    - List (...). You may notice all of the code written takes for
;;      form of lists. By default if you have written (...) Clojure
;;      will assume the first item is a function and the rest are
;;      arguments to be passed in. In order for the list not to be
;;      executed you should prefix it with a '.
;;    - Vector [...]. A lot like a list except they are optimised for
;;      appending to the end of the list rather than the front
;;    - Hash Map
;;    - Hash Set

;; The most important fact about the space is its size. We will use
;; metres to measure the size only converting to pixels when we need
;; to draw to the screen. We are going to define size as a vector
;; containing its width, height and depth.

(def space [8   ;; width
            5   ;; height
            6]) ;; depth

;; We need to know the gravity of the space so it can influence the
;; flow of paint as it leaves the brush. This will be defined as a
;; vector that represents acceleration influenced by gravity.

(def gravity [0 -9.8 0])

;; Lastly, we need to know the normal of the surface of the canvas that
;; the paint will impact with. This will be used to dictate how paint
;; acts when it spatters from the impact with the canvas. (Maybe it
;; should be used to define what the coords are that the paint has to
;; impact with, it doesn't at the moment)

(def canvas-normal [0 1 0])







;; ## Starting points and projection

;; Our digital Pollock is going to start a stroke of the brush by
;; picking a random point in space. This point will then be projected
;; to find where it impacts with the canvas.

;; In order to generate a random point inside of the space we need to
;; define a function that each time it is called will emit a vector
;; containing the position of the point. Function values can be
;; created by calling `(fn [...] ...)` with the first vector being the
;; arguments the function should receive and any follow items in the
;; list are the body of the function and executed with the arguments
;; bound. Rather than calling `(def name (fn ...))` Clojure has provided a
;; shortcut function `(defn name [...] ...)`. An example of a defined
;; function is `(defn say-hello [name] (str "Hello " name))`, this
;; creates a function called say-hello that when called `(say-hello
;; "James")` it will return the string "Hello James".

;; We are going to cover a common functional idiom when dealing with
;; lists to change the dimensions of the space above into a random
;; point inside that space. To do this we want to iterate over each
;; dimension of the size of space, generate a random number between 0
;; and the magnitude of each dimension and then return the resultant
;; list of random numbers as a list. To generate a random number in
;; Clojure we can use the `(rand)` function, which will return a
;; random number between 0 (inclusive) and 1 (exclusive). The rand
;; function can take an optional parameter `(rand 100), this will
;; define the bounds of the number generated to 0 and 100.

;; The function map `(map [fn] [sequence])` will iterate through the
;; sequence executing the function with the current value of the
;; sequence as its first parameter, the values returned from the
;; function will be stored in a list the same length as the sequence
;; and returned by the function.

;; We can now define a random point inside of space as follows

(defn starting-point [] (map rand space))


;; Now that we can generate a random point in space we want to project
;; this to the canvas. We are going to use [Newtonian equations of
;; motion](http://wiki), we know the position, velocity and acceleration of the
;; point and we want to know what the position and velocity are when y
;; is 0. In order to work out final positions we need to know the
;; total time the point spent falling, we can do this using the y
;; position as we know that the final position should be 0.

;; To work out the time it takes for the point to reach the canvas we
;; will solve the following equation for t:
;;
;;    - $r$  = final displacement,
;;    - $r0$ = initial displacement,
;;    - $v0$ = initial velocity,
;;    - $a$  = acceleration,
;;    - $t$  = time.
;;
;; $r = r0 + v0 * t + \frac{at^2}{2}$
;;
;; This rearranges to:
;;
;; $at^2 + 2v0t + 2r0 - 2r = 0$
;;
;; We can solve this using the Quadratic Equation, but this will yield
;; us two results. In general we can say that we are interested in the
;; result with the maxium value.
;;
;; In this function you can see an example of call out to Java.
;; Clojure doesn't have an inbuilt sqrt function so we are calling out
;; to the java version. A function named in the form `foo/bar` means
;; it will call the function `bar` in the namespace `foo`. **WHAT IS A NAMESPACE?**

(defn time-to-canvas [position velocity acceleration]
  (let [a acceleration
        b (* 2 velocity)
        c (* 2 position)
        discriminant (- (* b b) (* 4 a c))
        minus-b (- 0 b)
        add-sqrt (/ (+ minus-b (Math/sqrt discriminant)) (* 2 a))
        minus-sqrt (/ (- minus-b (Math/sqrt discriminant)) (* 2 a))]
    (max add-sqrt minus-sqrt)))

;; We can calculate the time but we want the final position and
;; velocity. For position we can use the same function that we
;; rearranged above to derive the time.

(defn position-at [time initial-position initial-velocity acceleration]
  (+ initial-position
     (* initial-velocity time)
     (/ (* acceleration time time) 2)))

;; For velocity we can use another equation of motion:
;;
;; $v = at + v0$

(defn velocity-at [time initial-velocity acceleration]
  (+ (* acceleration time) initial-velocity))


;; These functions we just implemented can be joined up to get given
;; an initial position and velocity and return the final position and
;; velocity. This function doesn't explicitly ask for the acceleration
;; on the paint, it assumes only gravity is acting on it and uses the
;; constant defined earlier on.

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







;; ## Paint splatter

;; An important aspect of Pollocks painting is the splatter of the
;; paint hitting the canvas and what this adds to the images. We are
;; going to add a simple splatter model based of the velocity at
;; impact we calculated in the last part.

;; Not all paint that hits the canvas will splatter, so we need to
;; work out the impact force of the paint and use this as a cutoff for
;; whether the paint should splatter.

;; In order to work out this value as a scalar we will need to be able
;; to calculate the absolute value of a vector. This can be done by
;; summing the squares of the dimensions **is it dimensions and not
;; some other lingo** and then take the square root of that summation.

;; This function will introduce a shorthand for defining functions
;; that is very useful in combination with functions like `map` and
;; `reduce`. Rather than writing `(fn [args...] body)` you can use
;; `#(body)` and if you want access to the arguments use `%n` where
;; `n` is the position of the argument. If you are only expecting one
;; argument then you can use just `%` on its own.

(defn vector-absolute [vector]
  (Math/sqrt (reduce + (map #(* % %) vector))))

;; **need something about why it is ok this is velocity and not
;; acceleration, maybe because it is the relative acceleration of the
;; imapct of the two objects (paint has velocity, cavnas doesn't so
;; accel = velocity?)**

(defn impact-force [mass velocity]
  (* (vector-absolute velocity) mass))

;; Based of this function to calculate the impact force we can define
;; a predicate that will tell us whether paint should splatter based
;; off its mass and velocity. It is idiomatic in Clojure to end
;; predicates with a `?`. We are going to add some randomness to this
;; function so that we don't necessarily just get a uniform line of
;; points. Also defined is a minimum force for us to consider whether
;; some paint could splatter.

(def min-impact-force-for-splatter 50)

(defn does-impact-splatter? [mass velocity]
  (and (> (impact-force mass velocity) min-impact-force-for-splatter)
       (> (rand) 0.8)))

;; If an impact splatters then we will need to bounce its velocity
;; vector as this is the direction it will leave its current position.

;; The equation to bounce a vector, $V$, off a plane with normal, $N$, is:
;; **needs rewriting http://mathworld.wolfram.com/Reflection.html**
;; 
;;    - $N$ is the normal vector of the plane
;;    - $V$ = the incoming vector
;;    - $B$ is the outgoing, bounced, vector
;; 
;;  $B = V - (2 * (V.N) * N)$

;; We are missing a few of the required vector operations used in this
;; equation so we should define some more functions before trying to
;; implement it. The first is the vector dot product, this is defined
;; as the sum of the multiples of each dimension. Otherwise we need
;; substraction of two vectors and a function to multiply a vector by
;; a constant.

(defn dot-product [vector1 vector2]
  (reduce + (map * vector1 vector2)))

(defn vector-subtraction [vector1 vector2]
  (map - vector1 vector2))

(defn vector-multiply-by-constant [vector constant]
  (map #(* % constant) vector))


;; Using the above functions we can now implement the vector bouncing
;; equation. I have pulled $(2 * (V.N) * N)$ out into a variable
;; called extreme for clarity. **I don't think extreme is a much
;; better name than blah**

(defn bounce-vector [vector normal]
  (let [vector-dot-normal (dot-product vector normal)
        extreme (vector-multiply-by-constant normal (* 2 vector-dot-normal))]
    (vector-subtraction vector extreme)))


;; When an impact splatters it will only take a fraction of the
;; velocity, otherwise know as being elastic rather than inelastic
;; **check these definitions**. We can define a constant that will be
;; used to reduce the total velocity of the bounced vector to reflect
;; this elasticity.

;; **should this splatter vector be randomised a little bit?**

(def splatter-dampening-constant 0.7)

(defn splatter-vector [velocity]
  (let [bounced-vector (bounce-vector velocity canvas-normal)]
    (vector-multiply-by-constant bounced-vector
                                 splatter-dampening-constant)))






;; ## Paths vs Points

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







;; ## Motion, going through the paces

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






;; ## Putting it all together

;; I've pulled a bunch of colours that Pollock used in his seminal
;; work "Number 8" so that each flick of paint can be rendered in a
;; random colour out of this pallete

(def canvas-colour [142 141 93])

(def paint-colours
  [[232 51 1]
   [248 179 10]
   [247 239 189]
   [29 16 8]])

(defn pick-a-colour []
  (nth paint-colours (rand-int (count paint-colours))))

;; now we need to assemble all of the above functions into something
;; that will work out everything

(defn fling-paint []
  (let [position       (starting-point)
        total-time     (random-between 1 5)
        path           (ensure-above-canvas (de-casteljau (anchor-points position 0.1 2 3 15 0.4) 0.01))
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
    {:colour (pick-a-colour)
     :air-path path
     :canvas-path (map #(conj %1 %2) projected-path masses)
     :splatter (filter #(not-any? nil? %) (partition-by nil? projected-splatter))}))






;; ## Rendering the canvas

;; We need to know the available size for the outputted image to fit
;; in. To work this out we are going to have to interface with
;; JavaScript directly. Luckily ClojureScript makes this very easy
;; using the js namespace.

(def image-width (.-clientWidth (.querySelector js/document "#pollock")))

;; Now we have the width of the image we can use the dimensions of the
;; space to work out the pixel size of the image and how to convert
;; between metres and pixels.

(def pixels-in-a-metre
  (let [[width _ _] space]
    (/ image-width width)))

(defn metres-to-pixels [metres]
  (Math/floor (* metres pixels-in-a-metre)))

;; We can now use this function to work out the size the sketch should
;; be and how to convert a position in metres over to a position to be
;; drawn in the image.

(def sketch-size
  (let [[width _ height] space]
    [(metres-to-pixels width)
     (metres-to-pixels height)]))

(defn position-to-pixel [[i j k]]
  [(metres-to-pixels i)
   (metres-to-pixels k)])


;; Now the dimensions of the image our calculated we can use Quil to
;; define the sketch that we will draw into. We also need to define a
;; function that will initialise the image into the state we want it.
;; This function will be run when the sketch is defined.

(defn setup-image []
  (apply q/background canvas-colour)
  (q/fill 0))

(q/defsketch pollock
  :setup setup-image
  :host "pollock"     ;; the id of the <canvas> element
  :size sketch-size)

;; To draw the trails of paint across the canvas we need draw a path
;; following the defined positions, which takes into account the
;; amount of paint at each position and uses this to set with width of
;; the path. In order to do this cleanly in Quil we need to consider
;; the path as pairs of positions that we shall draw paths between
;; using the initial paint amount as the stroke-weight. This allows
;; for a smooth decrease in the width of the path.

(defn draw-path [path]
  (doall
   (map-2 (fn [[position1 _ mass] [position2 _ _]]
            (q/stroke-weight mass)
            (apply q/line (concat (position-to-pixel position1) (position-to-pixel position2))))
          path)))

;; For splatter we are just going to draw a point that has a stroke-weight
;; proportional to the amount of paint.

(defn draw-splats [path]
  (doall (map (fn [[position _ mass]]
                (q/stroke-weight mass)
                (apply q/point (position-to-pixel position)))
              path)))

;; Now that we can render the result of flinging some paint around we
;; need a function that will fling the paint and render the result.

(defn fling-and-render [& any]
  (q/with-sketch (q/get-sketch-by-id "pollock")
    (let [{:keys [colour canvas-path splatter]} (fling-paint)]
      (q/stroke (apply q/color colour))
      (draw-path canvas-path)
      (doall (map draw-splats splatter)))))


;; Lastly, we shall attach to the buttons and cause our image to come
;; to life.

(.addEventListener (.querySelector js/document "#add")
                   "click"
                   fling-and-render)

(def interval-ref (atom nil))
(def fill-count (atom 0))
(.addEventListener (.querySelector js/document "#fill")
                   "click"
                   (fn [e]
                     (reset! interval-ref
                             (js/setInterval (fn []
                                               (if (> @fill-count 500)
                                                 (js/clearInterval @interval-ref)
                                                 (do (fling-and-render) (swap! fill-count inc)))) 100))))

