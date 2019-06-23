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

I will be using the SBCL implementation, and Emacs as my editor. Specifically, I
use the [Doom Emacs](https://github.com/hlissner/doom-emacs) configuration.
