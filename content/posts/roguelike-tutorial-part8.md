+++
title = "Roguelike tutorial for Common Lisp - Part 8 - Items and inventory"
author = ["Nick Forrer"]
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials", "roguelike-tutorial"]
draft = true
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 8](http://rogueliketutorials.com/tutorials/tcod/part-8/) of that tutorial.

In this part, we will add items to the game, as well as an inventory system to
interact with the items. For now, we will only be adding one item, a healing
potion, but we will be adding more items in the future.


## Placing items {#placing-items}

To start, we will update the map generation code to start placing healing
potions. Create a new function "place-items" that will place items in random
positions within a room. Then, update the "place-entities" method to take in a new parameter for the
max items allowed per room, and to call the new "place-items" function with a
random number of items to place. We'll also separate out a new function for
"place-monsters" so that the "place-entities" function doesn't get too large.

```lisp
(defun place-monsters (room entities num-monsters)
  (dotimes (monster-index num-monsters)
    (let ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
          (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room)))))
      (unless (entity-at entities x y)
        (cond ((< (random 100) 80)
               (let* ((fighter-component (make-instance 'fighter :hp 10 :defense 0 :power 3))
                      (ai-component (make-instance 'basic-monster))
                      (orc (make-instance 'entity :name "Orc" :x x :y y :color (blt:green) :char #\o :blocks t
                                                  :render-order :actor
                                                  :fighter fighter-component :ai ai-component)))
                 (nconc entities (list orc))))
              (t
               (let* ((fighter-component (make-instance 'fighter :hp 16 :defense 1 :power 4))
                      (ai-component (make-instance 'basic-monster))
                      (troll (make-instance 'entity :name "Troll" :x x :y y :color (blt:yellow) :char #\T :blocks t
                                                    :render-order :actor
                                                    :fighter fighter-component :ai ai-component)))
                 (nconc entities (list troll)))))))))

(defun place-items (room entities num-items)
  (dotimes (item-index num-items)
    (let ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
          (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room)))))
      (unless (entity-at entities x y)
        (let ((potion (make-instance 'entity :name "Healing Potion" :x x :y y :color (blt:purple)
                                           :char #\! :blocks nil :render-order :item)))
          (nconc entities (list potion)))))))

(defgeneric place-entities (map room entities max-enemies-per-room max-items-per-room))

(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room max-items-per-room)
  (let ((num-monsters (random max-enemies-per-room))
        (num-items (random (1+ max-items-per-room))))
    (place-monsters room entities num-monsters)
    (place-items room entities num-items)))
```

Now, in the "make-map" method, add a new "max-items-per-room" parameter, and
pass it through to the "place-entities" method.

```lisp
(defgeneric make-map (map max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room max-items-per-room))
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room max-items-per-room)
    ...
    (place-entities map new-room entities max-enemies-per-room max-items-per-room)
    ...)
```

In the main "cl-rltut.lisp" file, add the max items variable, and update the
call to "make-map" to pass the variable through.

```lisp
(defparameter *max-items-per-room* 2)

(make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room* *max-items-per-room*)
```

Now, if you run the game, you should see some health potions placed throughout
the map. However, you won't be able to interact with them, which is what we'll
be working on now.


## Creating the inventory {#creating-the-inventory}

Now, we will create a way to pickup the items we've placed on the map. The first
step will be to create an inventory to hold all of the items we pick up. In the
"components.lisp" file, create a new "inventory" class:

```lisp
(defclass inventory (component)
  ((capacity :initarg :capacity :accessor inventory/capacity)
   (items :initarg :items :accessor inventory/items :initform nil)))
```

Next, we need a way to tell which entities can be picked up and placed in the
inventory. For that, create an "item" component, which will be added to any
entities that can be pickup up. For now, it won't hold any additional slots, but
some will be added later.

```lisp
(defclass item (component))
```

Now, we need to add slots for these two new components to the entity class:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=11 12" >}}
(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks)
   (render-order :initarg :render-order :accessor entity/render-order :initform :corpse)
   (fighter :initarg :fighter :accessor entity/fighter :initform nil)
   (ai :initarg :ai :accessor entity/ai :initform nil)
   (item :initarg :item :accessor entity/item :initform nil)
   (inventory :initarg :inventory :accessor entity/inventory :initform nil)))
{{< /highlight >}}

Now, make sure to add an inventory component to the player, and an item
component to the healing potions.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=3-4 13 23 25" >}}
(defun main (...)
...
(inventory-component (make-instance 'inventory
                                               :capacity 26))
(player (make-instance 'entity
                                  :name "Player"
                                  :x (/ *screen-width* 2)
                                  :y (/ *screen-height* 2)
                                  :char #\@
                                  :color (blt:white)
                                  :blocks t
                                  :render-order :actor
                                  :fighter fighter-component
                                  :inventory inventory-component))
...)

(defun place-items (room entities num-items)
  (dotimes (item-index num-items)
    (let* ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
           (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room))))
           (existing-entity (entity-at entities x y)))
      (unless existing-entity
        (let* ((item-component (make-instance 'item))
               (potion (make-instance 'entity :name "Healing Potion" :x x :y y :color (blt:purple)
                                              :item item-component
                                              :char #\! :blocks nil :render-order :item)))
          (nconc entities (list potion)))))))
{{< /highlight >}}


## Using items {#using-items}


## Dropping items {#dropping-items}


## Conclusion {#conclusion}

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-6). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-7-1...part-8>.

If you run into any issues, or have questions/feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).
