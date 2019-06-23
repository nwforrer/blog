+++
title = "Roguelike tutorial for Common Lisp - Part 0"
author = ["Nick Forrer"]
date = 2019-06-23
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials"]
draft = true
+++

This set of tutorials is going to follow the [Python TCOD tutorial](http://rogueliketutorials.com/tutorials/tcod/), but will be
written in Common Lisp and use the BearLibTerminal library.


## Prior Knowledge {#prior-knowledge}

This tutorial assumes you are familiar with programming in Common Lisp. If
you're not familiar, I would highly recommend checking out [A Road to Common Lisp](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/)
on Steve Losh's blog. It provides many valuable resources to get up to speed
with Common Lisp, and is how I got started.


## Installation {#installation}

You will need the following installed:

-   A Common Lisp implementation. I will be using and testing my code against
    [SBCL](http://www.sbcl.org/).
-   [Quicklisp](https://www.quicklisp.org/beta/)
-   An editor. I use Emacs, with the [Doom Emacs](https://github.com/hlissner/doom-emacs) configuration.

I run Linux (Fedora) on my machine, and SBCL is available in the default
repositories. If it is not available in your distro (or if you're not running
Linux), check out the [getting started](http://www.sbcl.org/getting.html) page.

Quicklisp should be configured based on the installation guide on their [home page](https://www.quicklisp.org/beta/).

For the Emacs configuration, the most important peice is to have a REPL
available. I use the [sly](https://github.com/joaotavora/sly) package, although [SLIME](https://common-lisp.net/project/slime/) is popular as well. Having a
REPL running in your editor makes the development process much more efficient
and enjoyable.


## Project setup {#project-setup}

```common-lisp
(asdf:defsystem #:cl-rltut
  :description "Describe cl-rltut here"
  :author "Nick Forrer"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "cl-rltut")))
```
