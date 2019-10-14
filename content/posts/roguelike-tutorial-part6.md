+++
title = "Roguelike tutorial for Common Lisp - Part 6 - Combat"
author = ["Nick Forrer"]
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials", "roguelike-tutorial"]
draft = true
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 6](http://rogueliketutorials.com/tutorials/tcod/part-6/) of that tutorial.

In this part, we will be adding a combat system, allowing players to kill monsters,
as well as be killed. To accomplish this, we will also be adding a component
system, and implementing an A\* pathfinding algorithm. This will be a
relatively long post, so let's get right into it.


## Components {#components}

For any enemies that can fight and take damage, we'll be creating a fighter
"component," which will contain the entities hp, attack, and defense. This is
known as "composition," rather than inheritance. In an inheritance model, we
would likely have created a new Fighter class that inherits from the Entity
class. These types of hierarchies can start out fine, but will quickly become
difficult to work with.

Create a new file, components.lisp, with the new component and fighter classes:

```lisp
(in-package :cl-rltut)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))
```

We'll also be creating a component to represent an enemies AI behavior, along
with a "take-turn" method to execute the behavior:

```lisp
(defclass basic-monster (component) ())

(defgeneric take-turn (component))

(defmethod take-turn ((component basic-monster))
  (format t "The ~A wonders when it will get to move." (component/owner component)))
```

New we need to add slots to the "entity" class to hold the two new components.
They will be optional, since not all entities will have the components. We'll
also need to set the "owner" slot on the components to the entity that they are
being attached to, so that the components can get information for their entity,
such as the entity position.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=8-9  11-17" >}}
(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks)
   (fighter :initarg :fighter :accessor entity/fighter :initform nil)
   (ai :initarg :ai :accessor entity/ai :initform nil)))

(defmethod initialize-instance :after ((entity entity) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fighter ai) entity
    (when fighter
      (setf (component/owner fighter) entity))
    (when ai
      (setf (component/owner ai) entity))))
{{< /highlight >}}

Now, when creating the player entity, we'll want to create a fighter component
to add (in the main function in cl-rltut.lisp):

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=2-5 13" >}}
...
(let* ((fighter-component (make-instance 'fighter
                                         :hp 30
                                         :defense 2
                                         :power 5))
       (player (make-instance 'entity
                              :name "Player"
                              :x (/ *screen-width* 2)
                              :y (/ *screen-height* 2)
                              :char #\@
                              :color (blt:white)
                              :blocks t
                              :fighter fighter-component))
       (entities (list player))
       (map (make-instance 'game-map :w *map-width* :h *map-height*)))
  ...)
{{< /highlight >}}

And we'll do the same for the monsters (in the place-entities function in
game-map.lisp):

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=8-9 11 14-15 17" >}}
(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room)
  (let ((num-monsters (random max-enemies-per-room)))
    (dotimes (monster-index num-monsters)
      (let ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
            (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room)))))
        (unless (entity-at entities x y)
          (cond ((< (random 100) 80)
                 (let* ((fighter-component (make-instance 'fighter :hp 10 :defense 0 :power 3))
                        (ai-component (make-instance 'basic-monster))
                        (orc (make-instance 'entity :name "Orc" :x x :y y :color (blt:green) :char #\o :blocks t
                                                    :fighter fighter-component :ai ai-component)))
                   (nconc entities (list orc))))
                (t
                 (let* ((fighter-component (make-instance 'fighter :hp 16 :defense 1 :power 4))
                        (ai-component (make-instance 'basic-monster))
                        (troll (make-instance 'entity :name "Troll" :x x :y y :color (blt:yellow) :char #\T :blocks t
                                                      :fighter fighter-component :ai ai-component)))
                   (nconc entities (list troll))))))))))
{{< /highlight >}}

Now we can update the game loop to call the take-turn function on all the
entities with the AI component:

{{< highlight lisp "linenos=table, linenostart=1" >}}
(when (eql (game-state/state game-state) :enemy-turn)
  (dolist (entity (remove-if-not #'entity/ai entities))
    (take-turn (entity/ai entity)))
  (setf (game-state/state game-state) :player-turn))
{{< /highlight >}}

You can now run the game again. The only difference you'll see is the message
being printed out by the monsters is different, but we've set up a working
component system. Next we'll want to update the AI component to move towards the
player and attack.


## Basic monster AI {#basic-monster-ai}

Add a "move-towards" method to the entity class, which will move an the entity
towards a target location. It will only move in a straight line towards the
target, and stop if it hits a wall. We'll add pathfinding a little later.

```lisp
(defgeneric move-towards (e target-x target-y map entities))

(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let* ((dx (- target-x x))
           (dy (- target-y y))
           (distance (sqrt (+ (expt dx 2) (expt dy 2)))))
      (setf dx (round (/ dx distance))
            dy (round (/ dy distance)))
      (unless (or (blocked-p map (+ x dx) (+ y dy))
                  (blocking-entity-at entities (+ x dx) (+ y dy)))
        (move e dx dy)))))
```

Also add a "distance-to" method, which we'll use to decide whether a monster is
close enough to attack the player:

```lisp
(defmethod distance-to ((e entity) (other entity))
  (let ((dx (- (entity/x other) (entity/x e)))
        (dy (- (entity/y other) (entity/y e))))
    (sqrt (+ (expt dx 2) (expt dy 2)))))
```

Now update the "take-turn" method to move towards the player. We'll move the
monster as long as it's within the FOV of the player. If the monster is one tile
away from the player, then print out an attack message.

```lisp
(defgeneric take-turn (component target map entities))

(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter target)) 0)
             (format t "The ~A insults you! Your ego is damaged!" (entity/name monster)))))))
```

We'll also need to update the call to "take-turn" to pass in the target, map,
and entities parameters.

```lisp
(when (eql (game-state/state game-state) :enemy-turn)
  (dolist (entity (remove-if-not #'entity/ai entities))
    (take-turn (entity/ai entity) player map entities))
  (setf (game-state/state game-state) :player-turn))
```

Now you can run the game, and the enemies will chase you. When they get close
enough they'll print out a message insulting you.

Currently, the monsters are able to attack the player from a diagonal position,
but the player can only attack up/down/left/right, and both the player and
monsters can only move in four directions. We can either stop the monsters from
attacking diagonally, or allow all entities to attack and move in eight
directions. We'll implement the latter. First, let's allow the player to move in
eight directions. We'll use the "vim keys" for movement. Update the
"handle-keys" function as follows:

{{< highlight lisp "linenos=table, linenostart=1" >}}
(defun handle-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  ((or :up :k) (list :move (cons 0 -1)))
                  ((or :down :j) (list :move (cons 0 1)))
                  ((or :left :h) (list :move (cons -1 0)))
                  ((or :right :l) (list :move (cons 1 0)))
                  (:y (list :move (cons -1 -1)))
                  (:u (list :move (cons 1 -1)))
                  (:b (list :move (cons -1 1)))
                  (:n (list :move (cons 1 1)))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))
{{< /highlight >}}


## A-star Pathfinding {#a-star-pathfinding}

Now, we'll want to allow the enemies to move in eight directions. To do this,
we'll need to implement a pathfinding algorithm called "A\* (A-star)." Red Blob Games
as a great [introduction to A\*](https://www.redblobgames.com/pathfinding/a-star/introduction.html), as well as an [implementation guide](https://www.redblobgames.com/pathfinding/a-star/implementation.html) for
Python, C++, and C#. For my implementation, I used [this article](https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2) as a guide. I
won't be going very in-depth of how the algorithm works at a high level, so
please refer to the those articles for more information.

First, we're going to use a new library to make use of a priority queue. Update the
dependencies in your ASDF file to match the following:

```lisp
:depends-on (#:cl-blt #:queues.priority-queue)
```

If you have you're REPl open, go ahead and load the system with Quicklisp:

```lisp
(ql:quickload :queues.priority-queue)
```

Create a new file "pathfinding.lisp". We'll start by creating a "node" class to
hold the position of the tile, the "parent" (node that we came from to get to
this node), and the g/h/f variables used in the A\* algorithm.

-   "h" is the estimated distance from the current node to the target location. We
    just calculate the distance between the positions using the Pythagorean
    Theorem, but other estimations could be used instead.
-   "g" is the distance from the starting node to the current node. To calculate
    this, we've decided moving left/right/up/down counts as a distance of 10, and
    moving diagonally counts as 14. The "g" for a given node is the sum of all the
    "g" values of the previous nodes it took to get to the current node.
-   "f" is the total cost of the node. It's the sum of the "g" and "h" values.

We'll also include some helper functions for the class as well.
`*all-directions*` stores the directions the path can follow from a given tile,
which is in all eight directions.

```lisp
(in-package #:cl-rltut)

(defparameter *all-directions*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)
        (cons -1 -1)
        (cons -1 1)
        (cons 1 -1)
        (cons 1 1)))

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
   (distance-from-parent :initarg :distance-from-parent :accessor node/distance-from-parent)
   (parent :initarg :parent :initform nil :accessor node/parent)
   (position :initarg :position :initform nil :accessor node/position)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (position parent) obj
      (format stream "~A, parent ~A" position parent))))

(defun node-equal (n1 n2)
  "The two nodes are equal if their position slots are equal."
  (equal (node/position n1) (node/position n2)))

(defun node-compare (n1 n2)
  "Compares the F slots on the node, and returns true if n1's F slot is less than n2's."
  (< (node/f n1) (node/f n2)))

(defun find-in-queue (queue n)
  "Finds the node N in the QUEUE by it's position. If there are multiple nodes
with the same position, it will return the last one it finds."
  (let ((node nil))
    (queues:map-queue #'(lambda (item)
                          (when (node-equal n item)
                            (setf node item)))
                      queue)
    node))
```

Next, we'll write some helper functions to support the A\* algorithm. The
documentation on the functions should describe their functionality.

```lisp
(defun create-path (current-node)
  "Given a node, return a list of all parent nodes leading to it."
  (do ((path nil)
       (current current-node (node/parent current)))
      ((null current) (reverse path))
    (setf path (append path (list (node/position current))))))

(defun make-node (parent-node node-x node-y direction-from-parent)
  "Creates a NODE instance with the given PARENT, NODE-X and NODE-Y, and calculates the
DISTANCE-FROM-PARENT."
  (let ((distance 10))
    (if (and (not (zerop (car direction-from-parent)))
             (not (zerop (cdr direction-from-parent))))
        (setf distance 14))
    (make-instance 'node :parent parent-node
                         :position (cons node-x node-y)
                         :distance-from-parent distance)))

(defun generate-node-cost (child current-node end-node)
  "Calculates and sets the G, H, and F slots on child."
  (with-slots (g h f position distance-from-parent) child
    (setf g (+ distance-from-parent (node/g current-node))
          h (+ (expt (- (car position) (car (node/position end-node))) 2)
               (expt (- (cdr position) (cdr (node/position end-node))) 2))
          f (+ g h))))

(defun update-open-queue (open-list child-node)
  "Updates an existing entry in OPEN-LIST if one exists that both matches CHILD-NODE, and
has a larger G value. If there is no existing entry matching CHILD-NODE, then if pushes
CHILD-NODE onto OPEN-LIST."
  (let ((existing-child (find-in-queue open-list child-node)))
    (cond ((and existing-child (< (node/g child-node) (node/g existing-child)))
           (queues:queue-change open-list
                                (queues:queue-find open-list existing-child)
                                child-node))
          (t
           (queues:qpush open-list child-node)))))

(defun generate-node-children (current-node map open-list closed-list end-node)
  "Generates a list of all valid nodes that can be moved to from CURRENT-NODE,
and adds them to OPEN-QUEUE. A valid node is one that is within the MAP dimensions,
the tile is not blocking, and the node is not on CLOSED-LIST."
  (dolist (new-position *all-directions*)
    (let ((node-x (+ (car (node/position current-node))
                     (car new-position)))
          (node-y (+ (cdr (node/position current-node))
                     (cdr new-position))))
      (unless (or (> node-x (1- (game-map/w map)))
                  (< node-x 0)
                  (> node-y (1- (game-map/h map)))
                  (< node-y 0))
        (unless (tile/blocked (aref (game-map/tiles map) node-x node-y))
          (let ((child (make-node current-node node-x node-y new-position)))
            ;; child is on the closed list
            (unless (find child closed-list :test 'node-equal)
              (generate-node-cost child current-node end-node)
              (update-open-queue open-list child))))))))
```

Now we can write the main function of the algorithm, `astar`. It will start by
creating node instances for the start and end nodes, as well as the open-list
and closed-list. The open-list is initially populated with the start-node as the
first node to check. It will then begin a `do` loop, which pops the next node
off of the open-list queue to process, and continues looping until the path to
the end-node is found, or the open-list queue is empty. The function will return
a list of positions representing the path from start-node to end-node.

```lisp
(defun astar (map start end)
  "Returns a list of cons cells as a path from the given start to the given end in the given map."
  (let ((start-node (make-instance 'node :position start))
        (end-node (make-instance 'node :position end))
        (open-list (queues:make-queue :priority-queue :compare #'node-compare))
        (closed-list nil))
    (queues:qpush open-list start-node)
    (do ((current-node (queues:qpop open-list) (queues:qpop open-list)))
        ((null current-node))
      (setf closed-list (append closed-list (list current-node)))

      ;; found the goal
      (when (node-equal current-node end-node)
        (return-from astar (create-path current-node)))

      (generate-node-children current-node map open-list closed-list end-node))))
```

To make use of this function, modify the `move-towards` method in entity.lisp
to call `astar` and move to the second position in the returned path (remember
the first position in the path is where the entity is currently at):

```lisp
(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let ((path (astar map (cons x y) (cons target-x target-y))))
      (when path
        (let ((next-location (nth 1 path)))
          (unless (blocking-entity-at entities (car next-location) (cdr next-location))
            (move e (- (car next-location) x) (- (cdr next-location) y))))))))
```

Now if you run the game, you'll see the enemy monsters following the player
around, and that they can now move diagonally.


## Combat {#combat}

Next we'll start to implement the cambat system. First, add a `take-damage`
method to the `fighter` class:

```lisp
(defgeneric take-damage (component amount))

(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount))
```

Also add an `attack` method to the `fighter` class. It calculate the damage of
the attack by subtracting the defenders defense value from the attackers power.
If the damage is above zero, then it'll call the `take-damage` function on the defender.

```lisp
(defgeneric attack (component target))

(defmethod attack ((component fighter) (target entity))
  (let ((damage (- (fighter/power component) (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (take-damage (entity/fighter target) damage))
       (format t "~A attacks ~A for ~A hit points.~%"
               (entity/name (component/owner component))
               (entity/name target)
               damage)
      (t
       (format t "~A attacks ~A but does no damage.~%"
               (entity/name (component/owner component))
               (entity/name target))))))
```

We can now replace the placeholder messages that we were printing before. In the
cl-rltut.lisp files game loop:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=5" >}}
...
(unless (blocked-p map destination-x destination-y)
  (let ((target (blocking-entity-at entities destination-x destination-y)))
    (cond (target
           (attack (entity/fighter player) target))
          (t
           (move player (car move) (cdr move))
           (fov map (entity/x player) (entity/y player)))))
  (setf (game-state/state game-state) :enemy-turn))
{{< /highlight >}}

And the placeholder in components.lisp `take-turn`:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=8" >}}
(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter monster)) 0)
             (attack (entity/fighter monster) target))))))
{{< /highlight >}}


## Conclusion {#conclusion}

That's all there is for now. In the next post we'll be focusing on creating the
user interface so that we can display messages within the game, rather than
printing them to the REPL.

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-6). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-5...part-6>.

If you run into any issues, or have questions/feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).
