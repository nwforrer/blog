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

{{< figure src="/cl-rltut/items.png" >}}


## Picking up items {#picking-up-items}

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

The way we're going to have the player pickup an item is to stand on top of it
and press the 'g' key. We'll start by accepting the players input. Update the
handle-keys function to check for the 'g' key being pressed, and to return a
result that the player wants to pickup an item:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=12" >}}
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
                  (:g (list :pickup t))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))
{{< /highlight >}}

The inventory class will need a method to add new items to it's item list:

```lisp
(defgeneric add-item (inventory item))
(defmethod add-item ((inventory inventory) (item entity))
  (let ((results nil))
    (with-slots (items capacity) inventory
      (cond
        ((>= (length items) capacity)
         (setf results (list :item-added nil
                             :message "You cannot carry any more, your inventory is full")))
        (t
         (setf results (list :item-added item
                             :message (format nil "You pick up the ~A" (entity/name item))
                             :message-color (blt:yellow)))
         (setf items (append items (list item))))))
    results))
```

Next, we'll need a way to process the results of adding an item to the
inventory. But before we do, the game-tick function is getting a little large,
so lets pull out the turn handling code into separate functions for the
player-turn, handle-player-results, and enemy-turn:

```lisp
(defun player-turn (game-state map player action)
  (let ((player-turn-results nil)
        (move (getf action :move)))
    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at (game-state/entities game-state) destination-x destination-y)))
            (cond (target
                   (setf player-turn-results (attack (entity/fighter player) target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
          (setf (game-state/state game-state) :enemy-turn))))

    (values player-turn-results game-state)))

(defun handle-player-results (game-state player player-turn-results log)
  (let ((message (getf player-turn-results :message))
        (message-color (if (getf player-turn-results :message-color)
                           (getf player-turn-results :message-color)
                           (blt:white)))
        (dead-entity (getf player-turn-results :dead))
        (item-added (getf player-turn-results :item-added)))
    (when message
      (add-message log message :color message-color))
    (when dead-entity
      (cond ((equal dead-entity player)
             (setf (values message (game-state/state game-state))
                   (kill-player dead-entity)))
            (t
             (setf message (kill-monster dead-entity))))
      (add-message log message :color (blt:orange))))
  game-state)

(defun enemy-turn (game-state player map log)
  (dolist (entity (remove-if-not #'entity/ai (game-state/entities game-state)))
    (let* ((enemy-turn-results (take-turn (entity/ai entity) player map (game-state/entities game-state)))
           (message (getf enemy-turn-results :message))
           (dead-entity (getf enemy-turn-results :dead)))
      (when message
        (add-message log message))
      (when dead-entity
        (cond ((equal dead-entity player)
               (setf (values message (game-state/state game-state))
                     (kill-player dead-entity)))
              (t
               (setf message (kill-monster dead-entity))))
        (add-message log message :color (blt:red)))))
  game-state)

(defun game-tick (player map game-state stats-panel log)
  (declare (type game-state game-state))
  (declare (type message-log log))
  (render-all game-state player map stats-panel *screen-width* *screen-height*)
  (let* ((player-turn-results nil)
         (action (handle-keys game-state))
         (exit (getf action :quit)))

    (when (eql (game-state/state game-state) :player-turn)
      (setf (values player-turn-results game-state) (player-turn game-state map player action)))

    (when exit
      (if (or (eql (game-state/state game-state) :show-inventory)
              (eql (game-state/state game-state) :drop-inventory))
          (setf (game-state/state game-state) (game-state/previous-state game-state))
          (setf (game-state/running game-state) nil)))

    (setf game-state (handle-player-results game-state player player-turn-results log))

    (when (eql (game-state/state game-state) :enemy-turn)
      (setf game-state (enemy-turn game-state player map log))
      (when (eql (game-state/state game-state) :player-dead)
        (return-from game-tick game-state))
      (setf (game-state/state game-state) :player-turn)))

  game-state)
```

Now, we can handle the item pickup action. When the player wants to pickup an
item, we will loop through all of the entities to see which ones are on the same
tile as the player. If one of the entities is on the same tile, and it is an
item, then the player will pick it up.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=4 17-22" >}}
(defun player-turn (game-state map player action)
  (let ((player-turn-results nil)
        (move (getf action :move))
        (pickup (getf action :pickup)))
    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at (game-state/entities game-state) destination-x destination-y)))
            (cond (target
                   (setf player-turn-results (attack (entity/fighter player) target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
          (setf (game-state/state game-state) :enemy-turn))))

    (when pickup
      (dolist (entity (game-state/entities game-state))
        (when (and (entity/item entity)
                   (= (entity/x entity) (entity/x player))
                   (= (entity/y entity) (entity/y player)))
          (setf player-turn-results (add-item (entity/inventory player) entity)))))

    (values player-turn-results game-state)))
{{< /highlight >}}

Then, process the results of pickup up the item. When the item is picked up, it
needs to be removed from the entities list so that it is no longer rendered on
the map. Picking up an item should also use up the player's turn.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=7 17-22" >}}
(defun handle-player-results (game-state player player-turn-results log)
  (let ((message (getf player-turn-results :message))
        (message-color (if (getf player-turn-results :message-color)
                           (getf player-turn-results :message-color)
                           (blt:white)))
        (dead-entity (getf player-turn-results :dead))
        (item-added (getf player-turn-results :item-added)))
    (when message
      (add-message log message :color message-color))
    (when dead-entity
      (cond ((equal dead-entity player)
             (setf (values message (game-state/state game-state))
                   (kill-player dead-entity)))
            (t
             (setf message (kill-monster dead-entity))))
      (add-message log message :color (blt:orange)))
    (when item-added
      (setf (game-state/entities game-state) (remove-if
                                              #'(lambda (entity)
                                                  (eql entity item-added))
                                              (game-state/entities game-state))
            (game-state/state game-state) :enemy-turn)))
  game-state)
{{< /highlight >}}

Now, if you run the game, you should be able to pick up the healing potions by
pressing 'g' while on the same tile. However, you aren't able to do anything
with the yet. Before we can implement using items, we need to be able to display
what the player has in their inventory.


## Displaying the inventory menu {#displaying-the-inventory-menu}

We're going to add in an inventory menu that the player will be able to
display by pressing 'i'. From the inventory menu, the player will be presented
with all of the items they are holding, and shown a keyboard button they can
press which will correspond with each item.

To start, create a new "menu.lisp" file, with a generic "menu" function that
will handle rendering the menu with a header and a list of options to display.
For each item in the list of options, it will display an alphabetic character
which the player can press to use the items, starting at 'a' and increasing by
one for each subsequent option. Since it will use one alphabetic character as
the keyboard shortcut for every item, it will be limited to displaying a maximum
of 26 options.

```lisp
(defun menu (header options width screen-width screen-height)
  (assert (<= (length options) 26))

  (let* ((header-height 3)
         (height (+ (length options) header-height 1))
         (box-x (round (- (/ screen-width 2) (/ width 2))))
         (box-y (round (- (/ screen-height 2) (/ height 2)))))

    (blt:draw-box box-x box-y width height :contents header)

    (let ((y (+ header-height box-y))
          (letter-index (char-code #\a)))
      (dolist (option-text options)
        (let* ((text (concatenate 'string "(" (string (code-char letter-index)) ") " option-text)))
          (blt:print (1+ box-x) y text)
          (incf y)
          (incf letter-index))))))
```

Next, create an inventory-menu function that will pass the inventory items
through to the more generic "menu" function:

```lisp
(defun inventory-menu (header inventory inventory-width screen-width screen-height)
  (let ((options (if (zerop (length (inventory/items inventory)))
                     '("Inventory is empty.")
                     (mapcar #'(lambda (i)
                                 (entity/name i))
                             (inventory/items inventory)))))
    (menu header options inventory-width screen-width screen-height)))
```

We now need to be able to display the menu. When the player opens the inventory,
we will have the game switch to a new "show-inventory" state. We'll need to also
track the previous state that the game was in so that when the inventory menu is
closed, the game returns to the correct state so that the player doesn't lose a
turn if they just open and close the inventory without using anything. We'll use
the 'i' key to open the inventory.

Update the game-state class to hold the previous state:

```lisp
(defclass game-state ()
  ((state :initarg :state :accessor game-state/state)
   (previous-state :initarg :previous-state :accessor game-state/previous-state :initform nil)
   (entities :initarg :entities :accessor game-state/entities)
   (running :initarg :running :accessor game-state/running)))

(defmethod initialize-instance :after ((game-state game-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (state previous-state) game-state
    (unless previous-state
      (setf previous-state state))))
```

Update the handle-keys function to check for the 'i' key:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=13" >}}
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
                  (:g (list :pickup t))
                  (:i (list :show-inventory t))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))
{{< /highlight >}}

Update the player-turn function to check for the show-inventory action, change
the state to show-inventory and store the current state:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=5  25-28" >}}
(defun player-turn (game-state map player action)
  (let ((player-turn-results nil)
        (move (getf action :move))
        (pickup (getf action :pickup))
        (show-inventory (getf action :show-inventory)))
    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at (game-state/entities game-state) destination-x destination-y)))
            (cond (target
                   (setf player-turn-results (attack (entity/fighter player) target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
          (setf (game-state/state game-state) :enemy-turn))))

    (when pickup
      (dolist (entity (game-state/entities game-state))
        (when (and (entity/item entity)
                   (= (entity/x entity) (entity/x player))
                   (= (entity/y entity) (entity/y player)))
          (setf player-turn-results (add-item (entity/inventory player) entity)))))

    (when show-inventory
      (with-slots (previous-state state) game-state
        (setf previous-state state
              state :show-inventory)))

    (values player-turn-results game-state)))
{{< /highlight >}}

We'll also want to update the game-tick function so that when "escape" is
pressed and the inventory menu is open, the menu closes instead of exiting the game.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=13-16" >}}
(defun game-tick (player map game-state stats-panel log)
  (declare (type game-state game-state))
  (declare (type message-log log))
  (render-all game-state player map stats-panel *screen-width* *screen-height*)
  (let* ((player-turn-results nil)
         (action (handle-keys game-state))
         (inventory-index (getf action :inventory-index))
         (exit (getf action :quit)))

    (when (eql (game-state/state game-state) :player-turn)
      (setf (values player-turn-results game-state) (player-turn game-state map player action)))

    (when exit
      (if (eql (game-state/state game-state) :show-inventory)
          (setf (game-state/state game-state) (game-state/previous-state game-state))
          (setf (game-state/running game-state) nil)))

    (setf game-state (handle-player-results game-state player player-turn-results log))

    (when (eql (game-state/state game-state) :enemy-turn)
      (setf game-state (enemy-turn game-state player map log))
      (when (eql (game-state/state game-state) :player-dead)
        (return-from game-tick game-state))
      (setf (game-state/state game-state) :player-turn)))

  game-state)
{{< /highlight >}}

Now to finally render the inventory menu, update the render-all function to take
in the game-state as a parameter, and to render the menu when the state is
currently set to show-inventory. Make sure to update the call to render-all to
pass in the new game-state parameter:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=1  31-33" >}}
(defun render-all (game-state player map stats-panel screen-width screen-height)
  (blt:clear)
  (dotimes (y *map-height*)
    (dotimes (x *map-width*)
      (let* ((tile (aref (game-map/tiles map) x y))
             (wall (tile/block-sight tile))
             (visible (tile/visible tile))
             (explored (tile/explored tile)))
        (cond (visible
               (if wall
                   (setf (blt:background-color) (getf *color-map* :light-wall))
                   (setf (blt:background-color) (getf *color-map* :light-ground)))
               (setf (blt:cell-char x y) #\Space))
              (explored
               (if wall
                   (setf (blt:background-color) (getf *color-map* :dark-wall))
                   (setf (blt:background-color) (getf *color-map* :dark-ground)))
               (setf (blt:cell-char x y) #\Space))))))
  (let ((entities (game-state/entities game-state)))
    (mapc #'(lambda (entity) (draw entity (game-map/tiles map)))
          (sort (copy-seq entities) #'render-order-compare))
    (setf (blt:background-color) (blt:black)
          (blt:color) (blt:white))
    (render-panel stats-panel)

    (let ((entity-names (get-names-under-mouse (blt:mouse-x) (blt:mouse-y) entities map)))
      (when entity-names
        (setf (blt:color) (blt:yellow))
        (blt:print (1+ (panel/x stats-panel)) (1+ (panel/y stats-panel)) entity-names))))

  (when (or (eql (game-state/state game-state) :show-inventory))
    (let ((inventory-title "Press key next to item to use it, or Esc to cancel."))
      (inventory-menu inventory-title (entity/inventory player) 50 screen-width screen-height)))

  (blt:refresh))
{{< /highlight >}}

Now, if you run the game, you should be able to open the inventory menu by
pressing 'i'. If you are holding any items, they should be displayed, and you
should be able to press the escape key to close the menu.

{{< figure src="/cl-rltut/inventory-menu.png" >}}


## Using items {#using-items}


## Dropping items {#dropping-items}


## Conclusion {#conclusion}

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-6). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-7-1...part-8>.

If you run into any issues, or have questions/feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).
