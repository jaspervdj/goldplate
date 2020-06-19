# goldplate 🏅

    $ goldplate -j2 tests/
    Found 32 specs
    Running 49 executions in 2 jobs
    Finished in 0.84s
    Ran 32 specs, 49 executions, 146 asserts, all A-OK!

`goldplate` is a simple opaque [golden test] runner for CLI applications.  You
place your test cases in a directory, annotate them with `.goldplate` files, and
that's it.  It is completely language agnostic.

At [Fugue](https://fugue.co), we've been using internal versions of this tool
since 2016, so it should be pretty stable.

## Installation

### Using stack

1.  Install [stack] for your platform.
2.  Clone this repository and `cd` into it.
3.  Run `stack install`.
4.  Make sure `$HOME/.local/bin` is in your `$PATH`.

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
    `${GOLDPLATE_NAME}.stdout`.  `GOLDPLATE_NAME` is the name of the
    specification without the extension; so our expected output lives in
    [`tests/echo.stdout`](tests/echo.stdout) in this case.

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

## Reference

### Syntax

Environment variables can be spliced into the configuration using `${VAR}`
syntax within strings.

### Environment variables

The test is always executed in the directory that holds the `.goldplate` file.
`goldplate` will always set the following environment variables:

 -  `GOLDPLATE_FILE`: The filename of the `.goldplate` file, e.g.
    `echo.goldplate`.
 -  `GOLDPLATE_NAME`: The filename of the `.goldplate` file without the
    extension, e.g. `echo`.

When dealing with [multiple input files](#globbing-input-files), the following
additional variables are set:

 -  `GOLDPLATE_INPUT_FILE`: The input file name, relative to the current
    directory.
 -  `GOLDPLATE_INPUT_NAME`: The same as `GOLDPLATE_INPUT_FILE` but without
    any extensions.

[golden test]: https://ro-che.info/articles/2017-12-04-golden-tests
[stack]: https://docs.haskellstack.org/en/stable/README/
