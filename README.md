# Kiln - Practical Scripting in Common Lisp

The Kiln project enables using Common Lisp for scripting. Kiln is *not* a curated set of Lisp libraries: Kiln is an *infrastructure* (managing a hidden multicall binary) to make Lisp scripting efficient and ergonomic.

“Scripting” means minimal overhead: writing small programs as single-file scripts, without an (explicit) compile step, as in Bash, sed, awk, TCL, Perl, Ruby, and Python. Kiln bridges the scripting edit-run-edit cycle and the interactive, incremental Lisp development style.

Common Lisp is designed to solve big problems. Kiln makes it easy to reach for Common Lisp to solve *small* problems. Kiln makes it practical to write *very* small scripts. Kiln scripts are cheap to the point where it makes sense to expose even small pieces of Lisp functionality to the shell.

For example, would you ever think of [`format` as an alternative to `awk`](scripts/format.lisp)?

``` sh
$ echo -e "a b c\nd e f" | kiln format "~:@(~a~),~a:~a~%"
A,b:c
D,e:f
```

Or having the Lisp numeric tower in a [command-line calculator](scripts/math.lisp)?

``` sh
$ kiln math "factorial(30)"
265252859812191058636308480000000
```

Or writing [shell loops with the loop macro](scripts/loop.lisp)?

``` sh
$ kiln loop for '@i' from 0 to 5 do '!echo $i'
0
1
2
3
4
5
```

You can use `kiln help` to summarize the built-in subcommands. On a clean Kiln install, the output looks like this:

``` sh
$ kiln help
argv:         Print parsed arguments as a list
command:      Launch a subprocess with cmd
count:        Print number of arguments and input lines
echo:         Echo arguments to standard output
eval:         Eval arguments as Lisp forms
fire:         Alias for rebuild
format:       Use format like awk
grep:         Grep files with CL-PPCRE
help:         Print brief information about what scripts do
loop:         Write shell loops with the loop macro
math:         Do math with C-style syntax but a Lisp numeric tower
random:       Generate random numbers in a given range
range:        Print a range of numbers
rebuild:      Rebuild kiln executable
repl:         Launch a simple REPL
script-cache: Inspect script cache
self-test:    Perform self-test of Kiln itself
sprof:        Invoke Kiln command with profiling
version:      Print the Kiln version
```

See [INSTALL.md](./INSTALL.md) for installation instructions.

## Caveats

Kiln, while usable, is currently in alpha, because its implementation (particularly the hot-reloading part) involves hacking ASDF internals. Kiln will only have a non-alpha release once it uses only ASDF’s APIs.

Kiln is currently only tested with SBCL on Linux. You will also need
an ASDF recent enough to support package-local nicknames (so at least
3.3.3.2).

## What it does

Kiln provides two kinds of scripts (shebang scripts and package scripts) and two ways of loading scripts (hot reloading and rebuilding).

### Scripting, two ways

Kiln supports two kinds of scripts: shebang scripts and package scripts.

1. *Shebang scripts* are classic Unix scripts that start with `#!/usr/bin/env kiln`.
2. *Package scripts* are a halfway point between Lisp and Unix. You can write Kiln scripts at the REPL as package-inferred systems. For example, after defining `local-scripts/myscript` as a local package, you can invoke it as `kiln myscript`.

Both shebang scripts and package scripts *must* define a `main` function. For shebang scripts, this should be `kiln-user:main`. For package scripts, this should just be a `main` function in the script’s package.

### Reloading, two ways

Kiln makes scripting practical in two ways: (aggressively optimized) hot-reloading and easy image dumping.

1. It uses a whole bag of tricks to make hot-reloading of edited scripts (pacakage scripts or shebang scripts) as fast as possible.
2. Kiln makes it trivial (`kiln rebuild`) to dump a new image that bakes in *all* of your scripts and their dependencies, which can then be invoked either as subcommands (`kiln myscript`) or in multicall mode (`myscript` or `kiln-myscript` as a symlink to `kiln`).

Hot reloading is much faster than you would expect, as Kiln uses ASDF and
OS tricks to reuse preloaded dependencies and control compilation to
prioritize compilation speed.

## Writing shebang scripts

Writing shebang scripts is just like writing any script: start a file with `#!/usr/bin/env kiln`, make it executable, and put it in your path. (Or leave out the shebang and invoke it as `kiln ./myscript.lisp`.)

One difference from some scripting languages is that you do need to define a `main` function. Do this by defining `kiln-user:main`:

``` lisp
#!/usr/bin/env kiln
(defun kiln-user:main (args)
  (print "Hello, world!"))
```

The main function takes a single argument, the list of command-line
arguments.

## Writing package scripts

“Package scripts” are subsytems of package-inferred systems; their packages must define a `main` function.

Kiln has a way (`KILN_PATH_SYSTEMS`, discussed below) to tell which systems
should have their subsystems exposed as scripts. Kiln will dispatch to those subsystem’s packages’ scripts, through subcommands or multicalls, by calling their interned `main` function with the list of command-line arguments.

### `KILN_PATH_SYSTEMS`

There is an internal “path” defining which systems to check for
scripts. The highest-priority system on the path is always `local-scripts`, so
defining a file at `~/common-lisp/local-scripts/myscript.lisp` or
`~/quicklisp/local-projects/local-scripts/myscript.lisp` implicitly
makes `myscript` accessible with Kiln, either as subcommands of the
Kiln executable (so `$ kiln myscript`) or through multicall. The lowest-priority system is Kiln itself. In between are the systems in the (colon-separated) `KILN_PATH_SYSTEMS` environment variable.

For example, if you set `KILN_PATH_SYSTEMS` to `my-fancy-scripts:scripts-i-found-somewhere`, and you attempt to invoke a script named `my-script`, Kiln will look in the following subsystems:

1. `local-scripts/my-script`
1. `my-fancy-scripts/my-script`
1. `scripts-i-found-somewhere/my-script`
1. `kiln/scripts/my-script` (built-in scripts)

## Kiln environment

When a Kiln script runs (through Kiln), the following are always true:

- All ASDF systems present when Kiln was last rebuilt are marked as immutable and will not be reloaded.
- All classes are finalized.
- `*random-state*` has been re-initialized.
- `*standard-output*` points to stdout, `*error-output*` and `*trace-output*` point to stderr, and `*terminal-io*` and `*debug-io*` are two-way streams on `*standard-input*` and `*error-output*`.
- Unless `--debug` is passed to Kiln, `*compile-verbose*` `*compile-print*`, `*load-verbose*` and `*load-print*` are bound to `nil`.
- There is a handler in place to exit on EOF on any of stdin, stderr, or stdout.
- `*default-pathname-defaults*` is bound to the OS-level working directory.
- A handler is in place for `SIGINT` (`C-c`). If `SIGINT` is not handled, then the process will exit in such a way callers can see it exited with sigint; see https://www.cons.org/cracauer/sigint.html.
- There is a top-level error handler in place. Normally it prints the error (and a backtrace if Kiln gets `--debug` or `--backtrace`), and returns an error code based on the generic function `error-exit-code`. (If `--repl-on-error` is specified to Kiln, then it drops into a REPL instead.)
- You can always use `kiln:exit` to exit with a particular exit code. (Unlike `uiop:quit` this is guaranteed to unwind.)

[Buildapp]: https://www.xach.com/lisp/buildapp/
[Roswell]: https://roswell.github.io/
[adopt]: https://github.com/sjl/adopt
[cl-launch]: https://www.cliki.net/cl-launch
[clingon]: https://github.com/dnaeon/clingon
[clon]: https://github.com/didierverna/clon
[command-line-arguments]: https://github.com/fare/command-line-arguments
[unix-opts]: https://github.com/libre-man/unix-opts
