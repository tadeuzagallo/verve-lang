# Verve

Verve is an experimental, minimalistic, static, functional language with zero dependencies.

## Installing

To install Verve, simply clone the repo and run
```
make install
```

After that you should be able to run the command line `verve`, you can try it by running any of the tests, e.g.:
```
verve tests/math_parser.vrv
```

## Running the tests

The tests are broken into 3 categories:
* `output_tests` - run a program and compare it's output against the expected output
* `error_tests` - run a failing program and compare it's message against the expected
* `cpp_tests` - C++ unit tests

Each category of tests can be ran individually, with make, as in:
```
make output_tests
```

Or all together with:
```
make test
```

## Syntax highlight
Vim syntax highlight is available within the repo, you can install it by running:
```
make install_vim_highlight
```
