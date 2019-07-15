+++
title = "Roguelike tutorial for Common Lisp - Part 3 - Generating a dungeon"
author = ["Nick Forrer"]
date = 2019-07-05T17:05:00-04:00
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials"]
draft = false
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 3](http://rogueliketutorials.com/tutorials/tcod/part-3/) of that tutorial.

In this post, we'll be procedurally generating the dungeon! We will generate
randomly sized rooms, and connect them with tunnels for the player and npcs to
walk around.


## Looping over tiles {#looping-over-tiles}

First things first, we're going to set all tiles to blocked by default.
Previously, we had all tiles set as floors, and placed a couple of walls to
test. Most dungeon generation algorithms work in the opposite way. They first
block all tiles, and then procedurally carve out rooms and corridors.

Update the initialize-tiles method to match the following:

```common-lisp
(defmethod initialize-tiles ((map game-map))
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
      (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t)))))
```

Looping over tiles like this is going to be something that happens a few times.
To make this easier, we can define a macro that will loop over all tiles (or a
subsection of tiles), and assign the current tile to a variable for us to use.
The macro looks like this:

```common-lisp
(defmacro map-tiles-loop ((map tile-val &key (row-val (gensym)) (col-val (gensym)) (x-start 0) (y-start 0) (x-end nil) (y-end nil)) &body body)
  `(loop :for ,col-val :from ,x-start :below (if (null ,x-end) (game-map/w ,map) ,x-end)
         :do
            (loop :for ,row-val :from ,y-start :below (if (null ,y-end) (game-map/h ,map) ,y-end)
                  :do
                     (let ((,tile-val (aref (game-map/tiles ,map) ,col-val ,row-val)))
                       (declare (ignorable ,tile-val))
                       ,@body))))
```

The macro takes in a map with all the tiles initialized, a tile-val which holds
the name you want to use for the variable that holds the current tile. It also
takes in some optional parameters via keys: row-val and col-val represent the
names of the x and y variables if you want to access them within the body of the
macro. If not supplied, they are generated. x-start, y-start, x-end, and y-end
allow you to specify the start and end of the tile array to loop over. By
default, it loops over all the tiles.

To see this in use, we can update the initialize-tiles method:

```common-lisp
(defmethod initialize-tiles ((map game-map))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))
```

If we were to expand the macro, it looks like this:

```common-lisp
(loop :for x :from 0 :below (if (null nil)
                                  (game-map/w map)
                                  nil)
        :do (loop :for y :from 0 :below (if (null nil)
                                            (game-map/h map)
                                            nil)
                  :do (let ((tile (aref (game-map/tiles map) x y)))
                        (declare (ignorable tile))
                        (setf (aref (game-map/tiles map) x y)
                                (make-instance 'tile :blocked
                                               initial-blocked-value)))))
```


## Defining rooms and tunnels {#defining-rooms-and-tunnels}

Before we get into the map generation, let's create a helper class that we'll
use to represent rooms:

```common-lisp
(defclass rect ()
  ((x1 :initarg :x1 :accessor rect/x1)
   (x2 :initarg :x2 :accessor rect/x2)
   (y1 :initarg :y1 :accessor rect/y1)
   (y2 :initarg :y2 :accessor rect/y2)))

(defmethod initialize-instance :after ((rect rect) &key x y w h)
  (with-slots (x1 x2 y1 y2) rect
    (setf x1 x
          y1 y
          x2 (+ x w)
          y2 (+ y h))))
```

rect holds the values to represent the top left and bottom right corners of the
rectangle. We also defined the initialize-instance method for the class, and
allowed x y w h to be passed in, making it easier to create an instance of the class.

Now we can carve rooms into the map:

```common-lisp
(defmethod set-tile-slots ((tile tile) &key (blocked nil blocked-supplied-p) (block-sight nil block-sight-supplied-p))
  (if blocked-supplied-p
      (setf (slot-value tile 'blocked) blocked))
  (if block-sight-supplied-p
      (setf (slot-value tile 'block-sight) block-sight)))

(defmethod create-room ((map game-map) (room rect))
  (map-tiles-loop (map tile
                   :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                   :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil)))
```

We make sure to leave the border of the room untouched, which is why the loop
starts at x1/y1 + 1, and the ending x2/y2 is excluded (remember, the
map-tiles-loop macro uses :below in the loop). This ensures that if we make two
rooms next to each other, one starting at (1,1) going to (6,6), and the other
starting at (7,1) going to (9,6) there will still be a wall in between them.
Otherwise, it would look like one room instead of two rooms.

Now, let's make a couple rooms to test. Create the following method:

```common-lisp
(defmethod make-map ((map game-map))
  (let ((room-1 (make-instance 'rect :x 20 :y 15 :w 10 :h 15))
        (room-2 (make-instance 'rect :x 35 :y 15 :w 10 :h 15)))
    (create-room map room-1)
    (create-room map room-2)))
```

In order to call this method in our main function, we're going to update our
game loop structure a bit. We'll add a new game-tick function, and update the main function to look like this:

```common-lisp
(defun game-tick (player entities map)
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))
    (when move
      (unless (blocked-p map
                         (+ (entity/x player) (car move))
                         (+ (entity/y player) (cdr move)))
        (move player (car move) (cdr move))))

    exit))

(defun main ()
  (blt:with-terminal
      (config)
    (let ((player (make-instance 'entity
                                  :x (/ *screen-width* 2)
                                  :y (/ *screen-height* 2)
                                  :char #\@
                                  :color (blt:white)))
          (npc (make-instance 'entity
                               :x (- (/ *screen-width* 2) 5)
                               :y (/ *screen-height* 2)
                               :char #\@
                               :color (blt:yellow)))
          (entities (list player npc))
          (map (make-instance 'game-map :w *map-width* :h *map-height*)))
      (make-map (map))

      (do ((exit nil (game-tick player entities map)))
          (exit)))))
```

You can also remove the **map** global variable we had before, as we now create
the map locally. While we're at it, we don't really need the initialize-tiles method in the
game-map.lisp file. Since we always want the tiles initialized, we'll move that
code into the initialize-instance method:

```common-lisp
(defmethod initialize-instance :after ((map game-map) &key (initial-blocked-value t))
  (setf (game-map/tiles map) (make-array (list (game-map/w map) (game-map/h map))))
  (map-tiles-loop (map tile :col-val x :row-val y)
                  (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked initial-blocked-value))))
```

Now if you run the game, it should look like this:
![](/cl-rltut/create-test-rooms.png)

With the rooms being created, we should also create tunnels to connect them.
For our generation code, the tunnels will just be a combination of a horizontal
and vertical section, so there won't be any winding tunnels. Add the following
two methods:

```common-lisp
(defmethod create-h-tunnel ((map game-map) x1 x2 y)
  (let ((start-x (min x1 x2))
        (end-x (max x1 x2)))
    (map-tiles-loop (map tile
                     :x-start start-x :x-end (1+ end-x)
                     :y-start y :y-end (1+ y))
      (set-tile-slots tile :blocked nil :block-sight nil))))

(defmethod create-v-tunnel ((map game-map) y1 y2 x)
  (let ((start-y (min y1 y2))
        (end-y (max y1 y2)))
    (map-tiles-loop (map tile
                     :x-start x :x-end (1+ x)
                     :y-start start-y :y-end (1+ end-y))
      (set-tile-slots tile :blocked nil :block-sight nil))))
```

Let's test it out by updating our make-map method:

```common-lisp
(defmethod make-map ((map game-map))
  (let ((room-1 (make-instance 'rect :x 20 :y 15 :w 10 :h 15))
        (room-2 (make-instance 'rect :x 35 :y 15 :w 10 :h 15)))
    (create-room map room-1)
    (create-room map room-2))

  (create-h-tunnel map 25 40 23))
```

Running the game now should look like:
![](/cl-rltut/test-tunnel.png)


## Generating the dungeon {#generating-the-dungeon}

Now that we can create rooms and tunnels, it's time to move on to the actual
dungeon generation. It will be relatively basic: we'll create a bunch of rooms,
make sure they don't overlap, and connect them together.

First, add a couple methods to the rect class to assist with detecting when two
rooms overlap:

```common-lisp
(defmethod center ((rect rect))
  (with-slots (x1 x2 y1 y2) rect
    (let ((center-x (round (/ (+ x1 x2) 2)))
          (center-y (round (/ (+ y1 y2) 2))))
      (values center-x center-y))))

(defmethod intersect ((rect rect) (other rect))
  "Returns T if this RECT intersects with OTHER"
  (and (<= (rect/x1 rect) (rect/x2 other))
       (>= (rect/x2 rect) (rect/x1 other))
       (<= (rect/y1 rect) (rect/y2 other))
       (>= (rect/y2 rect) (rect/y1 other))))
```

Add a couple of variables to the cl-rltut.lisp file, to use with our generation:

```common-lisp
(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)
```

Now, update the make-map method signature to take in those variables, and start
calculating the position and size for the rooms:

```common-lisp
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player)
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
       ((>= room-index max-rooms))))
```

We're calculating each room's width and height as a random size between the
room-min-size and room-max-size. The x and y position is a random point within
the map. We then create a new-room variable with these calculated values. The
rooms variable is going to hold all the rooms that we create, so we can check
for overlaps, and the room-index and num-rooms will help with looking up the
previous room. When we generate the tunnels, we'll just be connecting the
current room to the previously created room.

Update the do\* body to check for intersections:

```common-lisp
(dolist (other-room rooms)
  (if (intersect new-room other-room)
      (setf can-place-p nil)))
```

That just loops over the rooms list (which we'll populate later), and checks if
the current room we're placing intersects with any of the other rooms. If there
is an intersection, we're setting the can-place-p variable to show that we
shouldn't actually create this room.

After that dolist loop, we can create the new room, and connect it to the
previous room:

```common-lisp
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
    (if (null rooms)
        (setf rooms (list new-room))
        (push new-room (cdr (last rooms))))
    (incf num-rooms)))
```

The tunnel creation randomly decides whether to use a horizontal or vertical
tunnel first, so that it doesn't look so uniform. The tunnel is created from the
center of the previous room, to the center of the new room. After the room is
created, it's added to the rooms list so that we can check for overlaps in the
next rooms.

Running the code now, you should see a complete dungeon:
![](/cl-rltut/dungeon-generation.png)

Note that since it's randomly generated, your output won't look exactly the same.


## Conclusion {#conclusion}

That's all there is to it. It's a pretty simple algorithm, but gives decent
results. There are plenty of other algorithms to generate different looking
dungeons. For example, you can use a type of [maze algorithm](http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/) to make the tunnels
between rooms more interesting.

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-3). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-2...part-3>.

Continue to the [part 4](/posts/roguelike-tutorial-part4).
