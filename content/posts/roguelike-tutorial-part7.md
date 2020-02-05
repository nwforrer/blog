+++
title = "Roguelike tutorial for Common Lisp - Part 7 - Creating the interface"
author = ["Nick Forrer"]
date = 2020-02-03T22:00:00-05:00
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials", "roguelike-tutorial"]
draft = false
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 7](http://rogueliketutorials.com/tutorials/tcod/part-7/) of that tutorial.

In this part, we will be working on the UI for the game. This will include
a nicer looking health bar, displaying the game messages on screen rather than
in the REPL, and showing entity information when hovering over a tile with your
mouse cursor.


## The health bar {#the-health-bar}

First, lets work on showing a prettier health bar. Right now we are just
displaying text for the current health of the player. Instead, we will render a
"progress bar" that will fill and deplete based on the player's health. Before
we get into creating this bar, I'd first like to create the concept of a
"panel". This will be a subsection of the screen where various UI elements can
be grouped together, and rendered with a border to show the separation from the
game map itself.

First, update the map height to give some more space for the UI to be displayed:

```lisp
(defparameter *map-height* 43)
```

Next, create a new file "ui.lisp" to hold the panel implementation, along with
the additional UI elements we'll be creating. Define the "panel" class, which
will hold it's position on screen, it's width and height, along with a list of
the components that should be rendered within. This will include the health bar
once we create it, as well as the message log.

```lisp
(in-package :cl-rltut.ui)

(defclass panel ()
  ((x :initarg :x :accessor panel/x)
   (y :initarg :y :accessor panel/y)
   (width :initarg :width :accessor panel/width)
   (height :initarg :height :accessor panel/height)
   (components :initarg :components :accessor panel/components :initform nil)))

(defmethod print-object ((object panel) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y width height) object
      (format stream "(~A,~A) ~Ax~A" x y width height))))

(defun make-panel (x y width height)
  (make-instance 'panel
                 :x x :y y :width width :height height))
```

We'll create a base class for UI components that will be rendered within a
panel. It will hold a reference to it's parent panel, and it's position within
the panel:

```lisp
(defclass panel-component ()
  ((panel :initarg :panel :accessor panel-component/panel)
   (x :initarg :x :accessor panel-component/x)
   (y :initarg :y :accessor panel-component/y)))
```

Now we can create the "bar" class, which we will keep generic enough to be used
for more than just health, such as mana or stamina. The bar will have a "name",
which will be the text rendered within the bar, the width of the bar, the
current value and maximum value, and the colors to use while rendering. It will
also include "value-bind" and "max-bind" slots. These will be functions that can
be called to update the value and maximum values each frame. When a "bar" is
created using the "make-bar" function, it will also add it to it's parent
panel's list of components.

```lisp
(defclass bar (panel-component)
  ((name :initarg :name :accessor bar/name)
   (total-width :initarg :total-width :accessor bar/total-width)
   (value :initarg :value :accessor bar/value)
   (value-bind :initarg :value-bind)
   (maximum :initarg :maximum :accessor bar/maximum)
   (max-bind :initarg :max-bind)
   (color :initarg :color :accessor bar/color)
   (bg-color :initarg :bg-color :accessor bar/bg-color)))

(defmethod print-object ((object bar) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name total-width value maximum) object
      (format stream "~A ~Aw ~A/~A" name total-width value maximum))))

(defun make-bar (name panel x y total-width value color bg-color &key (value-bind nil) (max-bind nil))
  "Create an instance of the `bar` class, and add it to it's parent panels list of components."
  (let ((bar (make-instance 'bar :name name :panel panel
                                 :x x :y y :total-width total-width
                                 :value value :maximum value
                                 :value-bind value-bind :max-bind max-bind
                                 :color color :bg-color bg-color)))
    (setf (panel/components panel) (append (panel/components panel) (list bar)))))
```

Now that we have the classes created, we can work on rendering the UI to the
screen. All of the UI components will implement a "render" method that will
handle rendering it within the panel. For the health bar, we'll render a box for
the background that is the full width of the bar, followed by another bar
over the top of the background whose width is calculated by the percentage of
health the player has. We'll also render the text for the player's health within
the bar.

```lisp
(defgeneric render (component))

(defmethod render ((bar bar))
  (with-slots (name panel x y total-width value value-bind maximum max-bind color bg-color) bar
    (when value-bind
      (setf value (funcall value-bind)))
    (when max-bind
      (setf maximum (funcall max-bind)))
    (let ((x-pos (+ (panel/x panel) x))
          (y-pos (+ (panel/y panel) y))
          (fill-width (round (* (/ value maximum) total-width)))
          (content (format nil "~A: ~A/~A" name value maximum)))
      (blt:draw-box x-pos y-pos total-width 1 :background-color bg-color :border nil)
      (unless (zerop value)
        (setf fill-width (max 1 fill-width))
        (blt:draw-box x-pos y-pos fill-width 1 :background-color color :border nil))
      (setf (blt:color) (blt:rgba 255 255 255))
      (blt:draw-box x-pos (1- y-pos) total-width 2 :background-color nil :border nil
                                                   :contents content))))
```

The render method for each component will be called by it's parent panel's
"render-panel" method:

```lisp
(defmethod render-panel ((panel panel))
  (with-slots (x y width height components) panel
      (blt:draw-box x y width height)
      (dolist (component components)
        (render component))))
```

Back in the main function, we will actually create the panel that will hold
these UI components, as well as the health bar. The "value-bind" and "max-bind"
slots on the health bar will be assigned to the methods to retrieve the player's
hp and max-hp slots. The stats instance will then be passed into the "game-tick" method.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=18-24 28" >}}
(defun main ()
  (blt:with-terminal
    (config)
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
                                  :render-order :actor
                                  :fighter fighter-component))
           (entities (list player))
           (map (make-instance 'game-map :w *map-width* :h *map-height*))
           (stats-panel (make-panel 0 *map-height* *screen-width* (- *screen-height* *map-height*))))
      (make-bar "HP" stats-panel 1 1 15
                (fighter/hp fighter-component)
                (blt:rgba 0 128 0) (blt:rgba 100 100 100)
                :value-bind #'(lambda () (fighter/hp fighter-component))
                :max-bind #'(lambda () (fighter/max-hp fighter-component)))
      (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
      (fov map (entity/x player) (entity/y player))

      (do ((*state* (make-instance 'game-state :running t :state :player-turn) (game-tick player entities map *state* stats-panel)))
          ((null (game-state/running *state*)))))))
{{< /highlight >}}

The "game-tick" function needs to be updated to include the stats panel as a
parameter, and pass it into the "render-all" function:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=1 3" >}}
(defun game-tick (player entities map game-state stats-panel)
  (declare (type game-state game-state))
  (render-all entities player map stats-panel *screen-width* *screen-height*)
{{< /highlight >}}

Finally, the "render-all" function will take the panel in as a parameter, and
then call the "render-panel" method to render the panel and all it's
components. The previous code to render the health can be removed:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=1 2 24" >}}
(defun render-all (entities player map stats-panel screen-width screen-height)
  (declare (ignore screen-width screen-height player))
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
  (mapc #'(lambda (entity) (draw entity (game-map/tiles map)))
        (sort entities #'render-order-compare))
  (setf (blt:background-color) (blt:black)
        (blt:color) (blt:white))
  (render-panel stats-panel)

  (blt:refresh))
{{< /highlight >}}

Now, if you run the game, you should see a health bar rendered towards the
bottom of the screen, and it should update when the player gets damaged:
![](/cl-rltut/health-bar.png)


## The message log {#the-message-log}

Now we'll work on displaying the message log on the screen, rather than just in
the REPL. We'll begin by defining the "message-log" class which will hold a list
of messages along with it's width and height. The "message" class will hold the
message text, as well as the message color, so that different messages can be
displayed with different colors.

```lisp
(defclass message-log (panel-component)
  ((messages :initarg :messages :accessor message-log/messages :initform nil)
   (width :initarg :width :accessor message-log/width)
   (height :initarg :height :accessor message-log/height)))

(defclass message ()
  ((text :initarg :text :accessor message/text)
   (color :initarg :color :accessor message/color)))

(defun make-message-log (panel x y width height)
  (let ((log (make-instance 'message-log :panel panel :x x :y y :width width :height height)))
    (setf (panel/components panel) (append (panel/components panel) (list log)))
    log))
```

We will also want a method to add a message to the message log. This will add a
message to the message-log "messages" list, and will also take care of removing
old messages when the messages will overflow the height of the message log.
We'll also handle "word-wrapping" when messages are adding that exceed the width
of the message log.

```lisp
(defun word-wrap (full-line width)
  (do ((lines nil)
       (line full-line))
      ((zerop (length line)) lines)
    (cond ((< (length line) width)
           (setf lines (append lines (list line))
                 line nil))
           (t
            (setf lines (append lines (list (subseq line 0 width)))
                  line (subseq line width))))))

(defgeneric add-message (log message &key color))
(defmethod add-message ((log message-log) message &key (color (blt:rgba 255 255 255)))
  (with-slots (messages width height) log
    (let ((wrapped-text (word-wrap message width)))
      (dolist (text wrapped-text)
        (setf messages (append messages (list (make-instance 'message :text text :color color))))
        (when (>= (length messages) (1- height))
          (setf messages (rest messages)))))))
```

Now, to implement the "render" method for the message log, we'll just loop over
all of the messages, and render them line-by-line:

```lisp
(defmethod render ((log message-log))
  (let ((x (+ (panel-component/x log) (panel/x (panel-component/panel log))))
        (y (+ (panel-component/y log) (panel/y (panel-component/panel log)))))
    (dolist (message (message-log/messages log))
      (setf (blt:color) (message/color message))
      (blt:print x y (message/text message))
      (incf y))))
```

Now, back in the main function, we can create the message log, and add an
initial message.

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=20 29" >}}
(defun main ()
  (blt:with-terminal
    (config)
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
                                  :render-order :actor
                                  :fighter fighter-component))
           (entities (list player))
           (map (make-instance 'game-map :w *map-width* :h *map-height*))
           (stats-panel (make-panel 0 *map-height* *screen-width* (- *screen-height* *map-height*)))
           (message-log (make-message-log stats-panel 20 2 (- *screen-width* 20) (- *screen-height* *map-height* 1))))
      (make-bar "HP" stats-panel 1 2 15
                (fighter/hp fighter-component)
                (blt:rgba 0 128 0) (blt:rgba 100 100 100)
                :value-bind #'(lambda () (fighter/hp fighter-component))
                :max-bind #'(lambda () (fighter/max-hp fighter-component)))
      (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
      (fov map (entity/x player) (entity/y player))

      (add-message message-log "Welcome to the dungeon!")

      (do ((*state* (make-instance 'game-state :running t :state :player-turn) (game-tick player entities map *state* stats-panel)))
          ((null (game-state/running *state*)))))))
{{< /highlight >}}

If you run the game now, you should see that initial message displayed on
screen:
![](/cl-rltut/initial-message-log.png)

To have the remaining messages displayed in the log, replace the "format"
function calls with "add-message" calls in the game-tick function. Once done,
you will see the message log fill up with messages:
![](/cl-rltut/message-log.png)


## Entities under cursor {#entities-under-cursor}

The final change we'll make is to incorporate mouse movement into the UI. When
the mouse cursor is over a tile, we'll display the list of all entity names to
the player. This will make it easier for player's to learn which entities are
which once there are more than two in the game.

The first change is to configure BearLibTerminal to track mouse movement events:

{{< highlight lisp "linenos=table, linenostart=1, hl_lines=5" >}}
(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard, mouse")
  (blt:set "window.title = Roguelike"))
{{< /highlight >}}

Now, in "rendering.lisp" we'll need a function to retrieve a list of all entity
names under the mouse cursor. This will first check to make sure the tile is
visible by the player. Then, it'll loop over the list of entities, and for any
entities at the mouse cursor position, it will append the entity name to the
"names" list. Then, to join all of the names to a string separated by a comma,
we'll use the "format" method, with the "{}" descriptor to iterate over the
names, and the "^" descriptor to only include the comma when it's not the last
name in the list.

```lisp
(defun get-names-under-mouse (x y entities map)
  (when (and (< y (game-map/h map))
             (< x (game-map/w map)))
    (let ((names nil)
          (in-fov (tile/visible (aref (game-map/tiles map) x y))))
      (when in-fov
        (dolist (entity entities)
          (when (and (= (entity/x entity) x)
                     (= (entity/y entity) y))
            (setf names (append names (list (entity/name entity)))))))
      (format nil "~{~A~^, ~}" names))))
```

Finally, update the "render-all" function to call this method, and render the
names to the screen.

```lisp
(let ((entity-names (get-names-under-mouse (blt:mouse-x) (blt:mouse-y) entities map)))
(when entity-names
    (setf (blt:color) (blt:yellow))
    (blt:print (1+ (panel/x stats-panel)) (1+ (panel/y stats-panel)) entity-names)))
```

Now when you run the game and hover your mouse over an entity, it will display
their names to the screen:
![](/cl-rltut/entities-under-cursor.png)


## Conclusion {#conclusion}

These were some changes to improve the user experience by making the UI nicer to
look at, and more informative.

You can find the current state of the code on [Github](https://github.com/nwforrer/cl-rltut/tree/part-6). The list of changes since
the previous tutorial can be found at
<https://github.com/nwforrer/cl-rltut/compare/part-6...part-7-1>.

If you run into any issues, or have questions/feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).
