# goldplate 🏅

    $ goldplate -j2 tests/
    Found 32 specs
    Running 49 executions in 2 jobs
    Finished in 0.84s
    Ran 32 specs, 49 executions, 146 asserts, all A-OK!

`goldplate` is a cute and simple opaque [golden test] runner for CLI
applications.  You place your test cases in a directory, annotate them with
`.goldplate` files, and that's it.  It is completely language agnostic.  And
perhaps its best feature is that it can automatically✨🪄 fix your tests outputs!

Give `goldplate` a try if:

 -  You are testing a CLI application
 -  The application produces deterministic output (e.g. compilers, prettifiers,
    convertors)
 -  The application is quick to start (a process is created for every test)
 -  Your output is text-based and not huge in size

At [Fugue](https://fugue.co), we've been using internal versions of this tool
since 2016, so it should be pretty stable.


## Table of Contents

-   [Tutorial](#tutorial)
    -   [Simple asserts](#simple-asserts)
    -   [Feeding input on stdin](#feeding-input-on-stdin)
    -   [Setting environment
        variables](#setting-environment-variables)
    -   [Globbing input files](#globbing-input-files)
    -   [Post processing: find and
        replace](#post-processing-find-and-replace)
    -   [Post processing: prettify
        JSON](#post-processing-prettify-json)
    -   [Created files and
        directories](#created-files-and-directories)
-   [Installation](#installation)
    -   [Using stack](#using-stack)
    -   [Using Cabal](#using-cabal)
-   [Reference](#reference)
    -   [Syntax](#syntax)
    -   [Environment variables](#environment-variables)
-   [Similar projects](#similar-projects)

## Tutorial

You can follow along with the tutorial by cloning the repository and running
this command:

    $ goldplate tests

As you can see, `goldplate` itself is tested using `goldplate`.  In this
tutorial, we'll walk through some examples.  By the end, you should have a good
idea of how to test your CLI application using `goldplate`.

### Simple asserts

View example: [`tests/echo.goldplate`](tests/echo.goldplate)

In this very simple example, we just run `echo "Hello, world!"`.  This is
specified in the `command` and `arguments` fields.

The actual tests that we're executing live in the `asserts` field.  This simple
test has two asserts:

1.  We verify that the exit code is 0 (success).
2.  We check the `stdout` (output) of the command against the file
    [`tests/echo.stdout`](tests/echo.stdout).

We can check that our asserts are correct:

    $ goldplate tests/echo.goldplate

If we want to regenerate the expected output, we can simply do:

    $ rm tests/echo.stdout
    $ goldplate --fix --pretty-diff tests/echo.goldplate

And `goldplate` will show you that it fixed one file.

### Feeding input on stdin

View example: [`tests/cat.goldplate`](tests/cat.goldplate)

You can pass one or multiple lines of input to the command by using the `stdin`
field.

### Setting environment variables

View example: [`tests/env.goldplate`](tests/env.goldplate)

The `environment` field can be used to set environment variables for the
program.

`goldplate` also sets [a number of environment
variables](#environment-variables).  You can use these directly within the
configuration JSON.  In this example, we use:

    {"stdout": "${GOLDPLATE_NAME}.stdout"}

Rather than:

    {"stdout": "env.stdout"}

We found this to be good practice, it makes mass-renaming of tests much easier.

### Globbing input files

View example: [`tests/glob.goldplate`](tests/glob.goldplate)

`.goldplate` files are fairly small but if you have a whole directory of files
that you just want to run the same command on, they can get very repetitive.
This is why `goldplate` provides a simple way to pull in multiple input files.

If the `input_files` field is set to a glob, all asserts will be ran for _every_
matching input file.  `goldplate` will set the following variables:

 -  `${GOLDPLATE_INPUT_FILE}`: the path to the input file
 -  `${GOLDPLATE_INPUT_NAME}`: the input file without extension

### Post processing: find and replace

View example: [`tests/replace.goldplate`](tests/replace.goldplate)

Sometimes you may want to do a find-and-replace on the actual output, for
example to filter out timestamps or other information that you do not expect to
match up against the expected output.

### Post processing: prettify JSON

View example: [`tests/prettify-json.goldplate`](tests/prettify-json.goldplate)

Many modern CLI tools output JSON.  You can use the `prettify_json` post
processor to make sure the JSON is converted to a normalized form with sorted
keys and consistent indentation.

### Created files and directories

View example: [`tests/create.goldplate`](tests/create.goldplate)

`goldplate` is geared towards checking the `stdout` and `stderr` outputs of a
program, but you can also check that files were created with specific contents.
If you do this, `goldplate` will remove these files and directories afterwards
to leave a clean repository behind.

## Installation

Installation through source is done using standard Haskell tooling -- [Cabal]
and [stack] both work well.

### Using stack

1.  Install [stack] for your platform.
2.  Clone this repository and `cd` into it.
3.  Run `stack install`.
4.  Make sure `$HOME/.local/bin` is in your `$PATH`.

### Using Cabal

1.  Install [Cabal] for your platform.
2.  Clone this repository and `cd` into it.
3.  Run `cabal install`.
4.  Make sure `$HOME/.cabal/bin` is in your `$PATH`.

## Reference

### Syntax

Environment variables can be spliced into the configuration using `${VAR}`
syntax within strings.  To escape this syntax, use `$${VAR}` to get a literal
`${VAR}`, `$$${VAR}` to get a literal `$${VAR}`, and so on.

### Environment variables

The test is always executed in the directory that holds the `.goldplate` file.
`goldplate` will always set the following environment variables:

 -  `GOLDPLATE_FILE`: The filename of the `.goldplate` file.
 -  `GOLDPLATE_NAME`: The filename of the `.goldplate` file _without_ the
    extension.

When dealing with [multiple input files](#globbing-input-files), the following
additional variables are set:

 -  `GOLDPLATE_INPUT_FILE`: The input file name (relative to the current
    directory).
 -  `GOLDPLATE_INPUT_NAME`: The same as `GOLDPLATE_INPUT_FILE` but without
    the extension.

To recap, if we have a specification `prettify-js.goldplate` that uses:

    "input_files": "module-*.js"

And `module-1.js` matches the glob, we'll get an execution with the following
environment variables set:

| Variable               | Value                   |
| ---------------------- | ----------------------- |
| `GOLDPLATE_FILE`       | `prettify-js.goldplate` |
| `GOLDPLATE_NAME`       | `prettify-js`           |
| `GOLDPLATE_INPUT_FILE` | `module-1.js`           |
| `GOLDPLATE_INPUT_NAME` | `module-1`              |

## Similar projects

A similar project is [smoke].  I think `goldplate` has two major advantages
over smoke:

 -  It can fix the output files automatically using `--fix`!  This is very
    useful if you make a change to your tool that affects _a lot_ of test
    files.  You still need to manually review the diff, but at least you don't
    need to manually update the specs.
 -  You can avoid most repetitive configs by using
    [simple globbing](#globbing-input-files).

[Cabal]: https://www.haskell.org/cabal/
[golden test]: https://ro-che.info/articles/2017-12-04-golden-tests
[stack]: https://docs.haskellstack.org/en/stable/README/
[smoke]: https://github.com/SamirTalwar/smoke
