+++
title = "Roguelike tutorial for Common Lisp - Part 3 - Generating a dungeon"
author = ["Nick Forrer"]
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials"]
draft = true
+++

This tutorial series is based on the [Python Roguelike Tutorial](http://rogueliketutorials.com). This will be
covering [Part 3](http://rogueliketutorials.com/tutorials/tcod/part-3/) of that tutorial.

In this post, we'll be procedurally generating the dungeon! We will generate
randomly sized rooms, and connect them with tunnels for the player and npcs to
walk around.
