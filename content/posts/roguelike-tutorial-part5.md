+++
title = "Roguelike tutorial for Common Lisp - Part 5 - Placing enemies"
author = ["Nick Forrer"]
date = 2019-07-15T23:19:00-04:00
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials", "roguelike-tutorial"]
draft = false
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 5](http://rogueliketutorials.com/tutorials/tcod/part-5/) of that tutorial.

In this post, we'll start adding enemies to the dungeon. We won't be getting
into the combat system yet, but we'll get collision detection and turn-based
movement working.


## Placing enemies {#placing-enemies}

We'll start by randomly placing enemies during the dungeon generation. Add a
\`place-entities\` method in the game-map.lisp file:

```common-lisp
(defun entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y))
        (return entity))))

(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room)
  (let ((num-monsters (random max-enemies-per-room)))
    (dotimes (monster-index num-monsters)
      (let ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
            (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room)))))
        (unless (entity-at entities x y)
          (if (< (random 100) 80)
              (nconc entities (list (make-instance 'entity :x x :y y :color (blt:green) :char #\o)))
              (nconc entities (list (make-instance 'entity :x x :y y :color (blt:yellow) :char #\T)))))))))
```

We choose a random number of entities to generate, from 0 up to the
max-enemies-per-room passed in. We then chose a random x and y position, within
the passed in room boundaries. Then, as long as there isn't already an enemy at
the chosen position, we'll place a new enemy. We'll randomly choose between two
different enemies, an orc or a troll. It will be an 80% chance to spawn an Orc,
which will be the weaker enemy.

Call this new method after creating each room in the \`make-map\` method. The
method will also need to take in two new parameters: the entities list, and the max-enemies-per-room.

```common-lisp
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room)
  (do* ((rooms nil)
        (num-rooms 0)
        (room-index 0 (1+ room-index))
        (w (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (h (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (x (random (- map-width w))
           (random (- map-width w)))
        (y (random (- map-height h))
           (random (- map-height h)))
        (new-room (make-instance 'rect :x x :y y :w w :h h)
                  (make-instance 'rect :x x :y y :w w :h h))
        (can-place-p t t))
       ((>= room-index max-rooms))
    (dolist (other-room rooms)
      (if (intersect new-room other-room)
          (setf can-place-p nil)))
    (when can-place-p
      (create-room map new-room)
      (multiple-value-bind (new-x new-y) (center new-room)
        (if (zerop num-rooms)
            (setf (entity/x player) new-x
                  (entity/y player) new-y)
            (multiple-value-bind (prev-x prev-y) (center (car (last rooms)))
              (cond ((= (random 2) 1)
                     (create-h-tunnel map prev-x new-x prev-y)
                     (create-v-tunnel map prev-y new-y new-x))
                    (t
                     (create-v-tunnel map prev-y new-y prev-x)
                     (create-h-tunnel map prev-x new-x new-y)))))
        (place-entities map new-room entities max-enemies-per-room)
        (if (null rooms)
            (setf rooms (list new-room))
            (push new-room (cdr (last rooms))))
        (incf num-rooms)))))
```

Since the method signature has been updated, we'll need to update the call from
our main function:

```common-lisp
(make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
```

The temporary npc we were using before can be deleted now as well.

If you run the game, you should now see enemies spawning in rooms:
![](/cl-rltut/placing-enemies.png)


## Colliding with enemies {#colliding-with-enemies}

If you move around in the game now, you'll notice that you can walk right
through the enemies. We'll want to make sure that when you try to walk into
them, you'll collide. Eventually, colliding with enemies should make you attack,
but that will come later. For now, we'll stop the movement and print out a
message.

First, we'll add a \`blocks\` slot to the entity class. We'll also add a \`name\`
slot while we're here, which we can use when printing entity information:

```common-lisp
(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks)))
```

Now, update everywhere an entity is created, to supply this additional
information (creating the player, and the enemies)
The player will look like this:

```common-lisp
(make-instance 'entity
               :name "Player"
               :x (/ *screen-width* 2)
               :y (/ *screen-height* 2)
               :char #\@
               :color (blt:white)
               :blocks t)
```

The enemies will look like this:

```common-lisp
(make-instance 'entity :name "Orc" :x x :y y :color (blt:green) :char #\o :blocks t)
(make-instance 'entity :name "Troll" :x x :y y :color (blt:yellow) :char #\T :blocks t)
```

We'll need a way to check whether a blocking entity exists in a specific
position on the map. Add a \`blocking-entity-at\` function, which will return the
entity that is found, or nil otherwise:

```common-lisp
(defun blocking-entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y)
             (entity/blocks entity))
        (return entity))))
```

Now, we can update the player movement code to check for enemy collisions when
moving. To do this, we'll check whether the tile that the player will be moving
to contains a blocking entity. If it does, we won't move the player, and we'll
print out a message instead. Update the movement check in the \`game-tick\` function:

```common-lisp
(when move
  (let ((destination-x (+ (entity/x player) (car move)))
        (destination-y (+ (entity/y player) (cdr move))))
    (unless (blocked-p map destination-x destination-y)
      (let ((target (blocking-entity-at entities destination-x destination-y)))
        (cond (target
               (format t "You kick the ~A.~%" (entity/name target)))
              (t
               (move player (car move) (cdr move))
               (fov map (entity/x player) (entity/y player))))))))
```

Now if you run the game, you'll collide with enemies, and see messages printed
out with the name of the entity that you collided with.


## Taking turns {#taking-turns}

The final change we'll make in this post is to introduce "turns". Right now, the
player can move whenever they want. However, the game is turn based, so will
need to let the enemies take their turns after the player. To do this, we'll
keep track of whose turn it is (the player or the enemies), and only let them
perform any action if it's their turn.

Add a type definition to hold the various states for the game:

```common-lisp
(deftype game-states () '(member :player-turn :enemy-turn :exit))
```

Update the \`game-tick\` function to track and update the game-state. When the
state is \`:player-turn\`, we'll let the player move and then set the state to
\`:enemy-turn\`. When the state is \`:enemy-turn\`, we'll loop through all of the
enemies, and let them take a turn. For now, an enemy will just print something
out when taking a turn, but later on we'll introduce enemy AI. We're also going
to be using the game-state to track when the game will exit, rather than
returning a boolean from the game-tick function.

```common-lisp
(defun game-tick (player entities map game-state)
  (declare (type game-states game-state))
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))
    (when (and move (eql game-state :player-turn))
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at entities destination-x destination-y)))
            (cond (target
                   (format t "You kick the ~A.~%" (entity/name target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
          (setf game-state :enemy-turn))))
    (when exit
      (setf game-state :exit)))

  (when (eql game-state :enemy-turn)
    (dolist (entity entities)
      (if (not (eql player entity))
          (format t "The ~A sits idly.~%" (entity/name entity))))
    (setf game-state :player-turn))

  game-state)
```

If you run the game now, when you move, you'll see all enemies that have been
spawned in the dungeon print something out. Each time you move, you'll see the
print statements.


## Conclusion {#conclusion}

That's all there is for now. In the next post we'll be focusing on the combat
system, now that we have enemies to attack.

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-5). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-4...part-5>.

If you run into any issues, or have some feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).
