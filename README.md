# goldplate

    $ goldplate -j2 tests/
    Found 32 specs
    Running 49 executions in 2 jobs
    Finished in 0.84s
    Ran 32 specs, 49 executions, 146 asserts, all A-OK!

`goldplate` is a simple opaque [golden test] runner for CLI applications.
You place your test cases in a directory, annotate them with `.goldplate` files,
and that's it.

## Tutorial

You can run `goldplate` by pointing it to either specific `.goldplate` files,
or directories that contain `.goldplate` files.  To invoke `goldplate` in this
repository, we just pass in the [`tests/`](tests/) directory:

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
    `${SPEC_NAME}.stdout`.  `SPEC_NAME` is the name of the specification without
    the extension; so our expected output lives in
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

### Post processing: find and replace

View example: [`tests/replace.goldplate`](tests/replace.goldplate)

Sometimes you may want to do a find-and-replace on the actual output, for
example to filter out timestamps or other information that you do not expect to
match up against the expected output.

[golden test]: https://ro-che.info/articles/2017-12-04-golden-tests
