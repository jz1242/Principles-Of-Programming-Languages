FbDK
====

This is a distribution of the Fb ("F-flat") development kit (FbDK).  The FbDK is
a tool for learning programming language theory through the development of
interpreters and type systems.  Your course instructor will indicate how the kit
is to be used for class.

Please see the `COPYING` file in this directory for licensing information.


Prerequisites
-------------

The FbDK has the following requirements:

* A working OCaml compiler
* An installation of `ocamlbuild`
* An installation of the `menhir` libraries available through `ocamlfind`

Installing the compiler, `ocamlbuild`, and `menhir` through OPAM should be
sufficient.

All of the commands below are to be run with `.../fbdk/` as the current
working directory.


Building Interpreters
---------------------

To compile the interpreters, it is sufficient to run

    # make

An individual interpreter may be compiled by naming it directly, e.g.

    # make fb.byte

The above command compiles the basic Fb interpreter from the `src/Fb/fb.ml`
file.


Running your FbDK Interpreter from the OCaml Toploop
----------------------------------------------------

As you develop your interpreters, you may wish to interact with your `eval`
function directly from the OCaml toploop.  Some OCaml toploop scripts have been
provided to assist you with this.  The procedure is to

  1. Compile your sources using `make`.  If you do not compile your interpreter
     (or if it fails to compile), the OCaml toploop will not behave correctly.
  2. Open the OCaml toploop.
  3. Load the appropriate file from the `debugscript` directory, such as
     `#use "debugscript/fb.ml";;`.  Select the script that matches the
     interpreter you are using.

If all goes well, the compiled OCaml objects will be loaded into your toploop
session and you will be able to use the types and functions defined in your
source code.  Good luck!


Precompiled Interpreters
------------------------

To provide a basis of comparison, compiled versions of the completed
interpreters are provided in the `binaries/` directory.  You may run those
binaries like so:

    # ocamlrun binaries/fb.byte

This will open the provided Fb interpreter.  The user may now enter code into the toploop, which will evaluate it:

```
$ ocamlrun binaries/fb.byte
	Fb version 1.3.0		(typechecker disabled)

# 3 + 5;;
==> 8
# (Function x -> x + 1) 5;;
==> 6
# True Or False;;
==> True
#
```

Pressing Ctrl+C will exit the toploop.

The interpreters may also be used to run files directly rather than through a
toploop.  The interpreter may take a filename as an argument:

```
$ ocamlrun binaries/fb.byte one_plus_three.fb
4
$
```

Any of the interpreters can provide more command-line usage information when passed the argument `--help`.

### Running the precompiled interpreters in the OCaml top loop

The precompiled binary interpreters may also be used in the OCaml top loop in the same spirit as "Running your FbDK Interpreter from the OCaml Toploop" above.  To do so, edit the `#directory` directive in file `debugscript/fb.ml` for example to refer to `binaries/libraries` instead of `_build/src/Fb`.
