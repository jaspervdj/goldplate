# goldplate 🏅

    $ goldplate -j2 tests/
    1..26
    ok tests/prettify-json.goldplate: exit_code
    ok tests/prettify-json.goldplate: stdout
    ok tests/env.goldplate: exit_code
    ok tests/env.goldplate: stdout
    ...

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

`goldplate` produces output compatible with the [Test Anything Protocol].

## Table of Contents

-   [Tutorial](#tutorial)
    -   [Creating a first test](#creating-a-first-test)
    -   [Feeding input on stdin](#feeding-input-on-stdin)
    -   [Setting environment
        variables](#setting-environment-variables)
    -   [Setting work
        directory](#setting-work-directory)
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

### Creating a first test

Imagine we are up to testing the behaviour of `echo` command.  In this very
simple example, we run `echo "Hello, world!"` and expect it to print `Hello,
world!` to the stdout stream as a result.

Create a new file `echo.goldplate` and add the following content:

```json
{
    "command": "echo",
    "arguments": ["Hello, world!"],
    "asserts": [
        {"exit_code": 0},
        {"stdout": "hello-world.txt"}
    ]
}
```

Let's go through this bit by bit.

The test invocation is specified by the `command` and `arguments` fields.  We
are invoking the `echo` process with a single argument, `"Hello, world!"`.

The expected results of our test live in the `asserts` field.  This simple test
has two asserts:

1.  We verify that the exit code is 0 (success).
2.  We check the `stdout` (output) of the command against the contents of
    the file `hello-world.txt`.

We haven't created `hello-world.txt` yet, but that's not a problem.  We can
invoke `goldplate --fix` to create it:

    $ goldplate echo.goldplate --pretty-diff --fix
    1..2
    ok echo.goldplate: exit_code
    not ok echo.goldplate: stdout
         diff:
         0a1
         > Hello, world!
         fixed ./hello-world.txt

After `hello-world.txt` has been created with proper contents, subsequent
`goldplate` invocations will pass:

    $ goldplate echo.goldplate
    1..2
    ok echo.goldplate: exit_code
    ok echo.goldplate: stdout

You can view the full example here:

 -  [`tests/echo.goldplate`](tests/echo.goldplate)
 -  [`tests/hello-world.txt`](tests/hello-world.txt)

### Feeding input on stdin

View example:

 -  [`tests/cat.goldplate`](tests/cat.goldplate)
 -  [`tests/cat.stdout`](tests/cat.stdout)

You can pass one or multiple lines of input to the command by using the `stdin`
field.

### Setting environment variables

View example:

 -  [`tests/env.goldplate`](tests/env.goldplate)
 -  [`tests/env.stdout`](tests/env.stdout)

The `environment` field can be used to set environment variables for the
program.

`goldplate` also sets [a number of environment
variables](#environment-variables).  You can use these directly within the
configuration JSON.  In this example, we use:

    {"stdout": "${GOLDPLATE_NAME}.stdout"}

Rather than:

    {"stdout": "env.stdout"}

We found this to be good practice, it makes mass-renaming of tests much easier.

### Setting work directory

View example:

 -  [`tests/work-dir.goldplate`](tests/work-dir.goldplate)
 -  [`tests/work-dir.stdout`](tests/work-dir.stdout)

The `working_directory` field can be used to set the work directory in which the
command will be executed. It can either be an absolute path or a path relative
to the `goldplate` file. If a `working_directory` is specified then the other
fields like `input_files` and `stdout` need to be relative to the
`working_directory` as well.

If a work directory is not specified the `command` will be executed in the
same directory as the `goldplate` file.

### Globbing input files

View example:

 -  [`tests/glob.goldplate`](tests/glob.goldplate)
 -  [`tests/glob-1.txt`](tests/glob-1.txt)
 -  [`tests/glob-1.stdout`](tests/glob-1.stdout)
 -  [`tests/glob-2.txt`](tests/glob-2.txt)
 -  [`tests/glob-2.stdout`](tests/glob-2.stdout)

`.goldplate` files are fairly small but if you have a whole directory of files
that you just want to run the same command on, they can get very repetitive.
This is why `goldplate` provides a simple way to pull in multiple input files.

If the `input_files` field is set to a glob, all asserts will be ran for _every_
matching input file.  `goldplate` will set the following variables:

 -  `${GOLDPLATE_INPUT_FILE}`: the path to the input file
 -  `${GOLDPLATE_INPUT_NAME}`: the input file without extension

### Post processing: find and replace

View example:

 -  [`tests/replace.goldplate`](tests/replace.goldplate)
 -  [`tests/replace.stdout`](tests/replace.stdout)

Sometimes you may want to do a find-and-replace on the actual output, for
example to filter out timestamps or other information that you do not expect to
match up against the expected output.

### Post processing: prettify JSON

View example:

 -  [`tests/prettify-json.goldplate`](tests/prettify-json.goldplate)
 -  [`tests/prettify-json.json`](tests/prettify-json.json)

Many modern CLI tools output JSON.  You can use the `prettify_json` post
processor to make sure the JSON is converted to a normalized form with sorted
keys and consistent indentation.

### Created files and directories

View example:

 -  [`tests/create.goldplate`](tests/create.goldplate)
 -  [`tests/create.file`](tests/create.file)

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
 -  `GOLDPLATE_BASENAME`: The basename (filename without directory) of
    the `.goldplate` file.

When dealing with [multiple input files](#globbing-input-files), the following
additional variables are set:

 -  `GOLDPLATE_INPUT_FILE`: The input file name (relative to the current
    directory).
 -  `GOLDPLATE_INPUT_NAME`: The same as `GOLDPLATE_INPUT_FILE` but without
    the extension.
 -  `GOLDPLATE_INPUT_BASENAME`: The basename (filename without directory) of
    the input file.

Here is an example that outputs all of these environment variables:

 -  [`tests/builtin.goldplate`](tests/builtin.goldplate)
 -  [`tests/builtin.stdout`](tests/builtin.stdout)

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
[Test Anything Protocol]: http://testanything.org/
