+++
title = "Roguelike tutorial for Common Lisp - Part 0 - Setup"
author = ["Nick Forrer"]
date = 2019-06-23T10:24:00-04:00
tags = ["roguelike", "gamedev", "lisp", "tutorial"]
categories = ["tutorials", "roguelike-tutorial"]
draft = false
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

Once you have everything installed that you need, you can get the project
skeleton created, with the needed dependencies. Since you have Quicklisp
installed, it will be easy to generate a project skeleton using [Quickproject](https://www.xach.com/lisp/quickproject/).

First, open emacs and start your REPL (if you're using sly, the Emacs command is
just M-x "sly"). In the REPL:

```common-lisp
(ql:quickload :quickproject)
(quickproject:make-project #p"cl-rltut" :depends-on '(:cl-blt))
```

That should create a "cl-rltut" directory within the current working
directory of your REPL (this can be shown for sly with the "sly-pwd" command,
and changed with the "sly-cd" command). Within the project directory, you should
see an ASDF file named "cl-rltut.asd", which looks like the following:

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

You will also need to download the cl-blt package, which contains the
Common Lisp bindings for BearLibTerminal. The reason for this is because
cl-blt is not currently available in an online repository that
Quicklisp could download it automatically. Download the package using:

```sh
git clone https://github.com/sjl/cl-blt.git ~/quicklisp/local-projects/cl-blt
```

That will clone the cl-blt Git repo into a directory that Quicklisp will look at
when trying to load an ASDF system. For reference, the latest commit on the
cl-blt repository is \`ee69ac7bfb473e9cdd8c2d50d45ef288ef315ff3\` at the time of
writing. If you want to guarantee you're using the same version as I am, just
make sure you checkout that commit.

In addition to the Common Lisp bindings, you will also need the BearLibTerminal
library itself. Download the appropriate binary for your system from
<http://foo.wyrd.name/en:bearlibterminal#download>. Once downloaded, extract the
archive and find the shared library dependency. For the Linux archive, this will
be Linux64/libBearLibTerminal.so (assuming you are using a 64-bit OS). This file
should be placed in a "lib" directory in the root of your project.

NOTE: Windows users may need to add the following to their package.lisp file, to
ensure the bearlibterminal library is found:

```lisp
(cffi:define-foreign-library blt:bearlibterminal
  (t "lib/BearLibTerminal.dll"))

(cffi:use-foreign-library blt:bearlibterminal)
```

To make it easier to load your project from the REPL, you can create a symlink
in the local-projects directly that points to your project, so that Quicklisp
can always find it (use the correct path to your project instead of ~/src/lisp/cl-rltut):

```sh
ln -s ~/src/lisp/cl-rltut ~/quicklisp/local-projects
```

With all of that in place, you should now be able to load your projects system
in the REPL to ensure in can find all the dependencies with:

```common-lisp
(ql:quickload :cl-rltut)
```

Assuming you don't see any errors, then your project is all set to continue with
the tutorial! To see what the project should look like at this point, you can
find the current state for Part 0 here: <https://github.com/nwforrer/cl-rltut/tree/part-0>.


## Questions and Feedback {#questions-and-feedback}

If you run into any issues, or have questions/feedback, please open an issue on this
blogs [GitHub repository](https://github.com/nwforrer/blog/issues).

Continue to the [next tutorial](/posts/roguelike-tutorial-part1).
