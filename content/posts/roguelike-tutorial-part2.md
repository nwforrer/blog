+++
title = "Roguelike tutorial for Common Lisp - Part 2 - Generic entity and map"
author = ["Nick Forrer"]
date = 2019-06-27T21:53:00-04:00
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials", "roguelike-tutorial"]
draft = false
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 2](http://rogueliketutorials.com/tutorials/tcod/part-2/) of that tutorial.

It's time to start thinking about how we want to structure our entities, and
what the map will look like. We will be creating a generic entity class that all
entities in the game will use, and introduce the concept of how the map will be
structured. We'll be using classes and generic methods provided with CLOS here.
If you're unfamiliar with CLOS, there is a good overview in [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/clos.html).


## Generic entity {#generic-entity}

The first thing we'll be doing is creating a class to represent entities in the
game, storing it's position, color, and character to render.

```common-lisp
(defclass entity ()
  ((x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)))
```

Next we can create some methods on that class to handle drawing and moving the
entity.

```common-lisp
(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))

(defmethod draw ((e entity))
  (with-slots (x y char color) e
    (setf (blt:color) color
          (blt:cell-char x y) char)))
```

We'll rename our old draw function to render-all, and from there call the entities draw
method for each entity.

```common-lisp
(defun render-all (entities)
  (blt:clear)
  (mapc #'draw entities)
  (blt:refresh))
```

We're just mapping the draw method call over the list of all entities passed
into the function.

Now we actually need to create some entities. Update the main function as
follows:

```common-lisp
(defun main()
  (blt:with-terminal
    (config)
    (loop :with player = (make-instance 'entity
                                        :x (/ *screen-width* 2)
                                        :y (/ *screen-height* 2)
                                        :char #\@
                                        :color (blt:white))
          :and npc = (make-instance 'entity
                                    :x (- (/ *screen-width* 2) 5)
                                    :y (/ *screen-height* 2)
                                    :char #\@
                                    :color (blt:yellow))
          :with entities = (list player npc)
          :do
             (render-all entities)
             (let* ((action (handle-keys))
                    (move (getf action :move))
                    (exit (getf action :quit)))
               (when exit
                 (return))
               (when move
                 (move player (car move) (cdr move)))))))
```

Here we removed the player-x and player-y variables, and instead create a player
entity. We have also added an npc entity to show how we can use the entity class
for more than just the player. We add those two entities to an entities list,
which gets passed to the render-all function. We also call the new move method
on the player entity when a move action is requested.

If you run the game now, it should look like the following:
![](/cl-rltut/generic-entity-class.png)

The NPC should look like a yellow @ symbol, and player movement should work like it
did before.


## Creating the map {#creating-the-map}

Now that we can create and render entities, we should create a map for them to
move around in. We're not going to procedurally generate the map yet (that will
be in the next tutorial), but we'll create the structure needed to render the
map.

Before we do, let's create a new file to store the map related code, as it can
become pretty large once we're generating it. Create a file named
"game-map.lisp" in the root of your project, and declare that it's in the same
package by placing this at the top of the file:

```common-lisp
(in-package #:cl-rltut)
```

You also need to update the ASDF file to include the new file in your system:

```common-lisp
(asdf:defsystem #:cl-rltut
  :description "Describe cl-rltut here"
  :author "Nick Forrer"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "cl-rltut")
               (:file "game-map")))
```

In the "game-map.lisp" file, create a new tile class.

```common-lisp
(defclass tile ()
  ((blocked :initarg :blocked
            :accessor tile/blocked
            :initform nil)
   (block-sight :initarg :block-sight
                :accessor tile/block-sight
                :initform nil)))

(defmethod initialize-instance :after ((tile tile) &rest initargs)
  (declare (ignore initargs))
  (with-slots (blocked block-sight) tile
    (if (null block-sight)
        (setf block-sight blocked))))
```

The blocked slot will indicate whether this tile will block movement, such as if
it represents a wall. The block-sight slot will indicate if this tile will block
an entities vision. These are separate so that we can support things like lava
tiles, where we don't want an entity to walk through it, but they can see past
it. We will implement field-of-vision later, but that will determine which tiles
the player can see at a given time.
We've also defined an after method for initialize-instance for the tile class.
This will be called after a make-instance is called for the class, and the slots
are initialized. Here, we want to make sure the block-sight slot is set to true
if blocked is also true.

Now lets create a game-map class, which will hold a 2D array of tiles to make up
our map.

```common-lisp
(defclass game-map ()
  ((width :initarg :w :accessor game-map/w)
   (height :initarg :h :accessor game-map/h)
   (tiles :accessor game-map/tiles)))

(defmethod initialize-instance :after ((map game-map) &rest initargs)
  (declare (ignore initargs))
  (setf (game-map/tiles map) (make-array (list (game-map/w map) (game-map/h map)))))
```

We also define an initialize-instance method for game-map. This initializes the
tiles slot to an array with WxH dimensions.

Next create a function to initialize the tiles in the array.

```common-lisp
(defun initialize-tiles ((map game-map))
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
      (setf (aref (game-map/tiles map) x y) (make-instance 'tile))))

  (setf (tile/blocked (aref (game-map/tiles map) 30 22)) t)
  (setf (tile/block-sight (aref (game-map/tiles map) 30 22)) t)
  (setf (tile/blocked (aref (game-map/tiles map) 31 22)) t)
  (setf (tile/block-sight (aref (game-map/tiles map) 31 22)) t)
  (setf (tile/blocked (aref (game-map/tiles map) 32 22)) t)
  (setf (tile/block-sight (aref (game-map/tiles map) 32 22)) t))
```

This loops through all the tiles in the map, and initializes them to a new
instance of the tile class. This will create all the tiles with blocked and
block-sight set to nil. We then set 3 of the tiles to have blocked and
block-sight set to true so we can test it.

We now have a map created, but we can't see it yet. Back in the "cl-rltut.lisp"
file, update the render-all function to take the map as a parameter and render
all it's tiles.

```common-lisp
(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)))

(defun render-all (entities map)
  (blt:clear)
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
      (let* ((tile (aref (game-map/tiles map) x y))
             (wall (tile/blocked tile)))
        (if wall
            (setf (blt:background-color) (getf *color-map* :dark-wall))
            (setf (blt:background-color) (getf *color-map* :dark-ground))))
      (setf (blt:cell-char x y) #\Space)))

  (mapc #'draw entities)

  (setf (blt:background-color) (blt:black))
  (blt:refresh))
```

We first declare a color-map global variable as a property list to hold a
mapping of keys (like "dark-wall") to their BearLibTerminal color value. This
just makes it easier to reference colors, and easy to update them later if we
wanted to. Then, in the render-all method, we loop over all of the tiles in the
map. We check if the blocked slot is true, and if so, set blt:background-color
to dark-wall. Whenever you set the background-color, all subsequent draw calls
will use it as the background in the cell you draw to. If the tile is not
blocked, we set background-color to dark-ground. We then draw an empty space at
the tiles cell, which will just populate the background of the cell.
After the entire map and all entities are drawn, we reset the background-color
to black.

Now in the main function, lets create a map instance and pass it to the
render-all function to actually see it displayed.

```common-lisp
(defparameter *map-width* 80)
(defparameter *map-height* 45)

(defparameter *map* nil)

(defun main ()
  (blt:with-terminal
    (config)
    (setf *map* (make-instance 'game-map :w *map-width* :h *map-height*))
    (initialize-tiles *map*)
    (loop :with player = (make-instance 'entity
                                        :x (/ *screen-width* 2)
                                        :y (/ *screen-height* 2)
                                        :char #\@
                                        :color (blt:white))
          :and npc = (make-instance 'entity
                                    :x (- (/ *screen-width* 2) 5)
                                    :y (/ *screen-height* 2)
                                    :char #\@
                                    :color (blt:yellow))
          :with entities = (list player npc)
          :do
             (render-all entities *map*)
             (let* ((action (handle-keys))
                    (move (getf action :move))
                    (exit (getf action :quit)))
               (when exit
                 (return))
               (when move
                 (move player (car move) (cdr move)))))))
```

We create two global variables to hold the map width and height (in cells). The
height is set to 5 cells smaller than the height of the window. This is so that
there is some blank space at the bottom of the window to be used for messages,
which we'll get to in a later tutorial. We also declare the map variable as a
global. This isn't really necessary, and could easily be created as a local
variable in the main function. The reason I've declared it global is so that it
can be looked at and manipulated in the REPL. It's up to you whether you find
that useful enough to make the variable global.
The changes to the main method are straight forward. We initialize an instance
of the game-map class, and initialize it's tiles. Then we just pass it along to
the render-all method to have it displayed.

If you run the game now, it should look like this:
![](/cl-rltut/initial-map-render.png)

The three dark tiles are the walls. You'll notice that you can still walk
through them, which isn't correct. We can easily fix that by first adding a
helper method in the "game-map-lisp" file:

```common-lisp
(defmethod blocked-p ((map game-map) x y)
  (tile/blocked (aref (game-map/tiles map) x y)))
```

This takes in the map and an x,y coordinate, and returns whether that tile is
blocked.

We then just need to call that with the coordinates we're moving to, and if it's
blocked, don't move the player. Update the \`(when move)\` block in the main
function like:

```common-lisp
(when move
  (unless (blocked-p *map*
                     (+ (entity/x player) (car move))
                     (+ (entity/y player) (cdr move)))
    (move player (car move) (cdr move))))
```

Now if you run the game, it should block you from walking through the walls!


## Conclusion {#conclusion}

That's all for this tutorial. Next we'll be working on procedurally generating
the dungeon map!

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-2). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-1...part-2>.

If you run into any issues, or have some feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).

Continue to the [part 3](/posts/roguelike-tutorial-part3).
