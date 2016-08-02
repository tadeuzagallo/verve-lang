# ![logo](https://cloud.githubusercontent.com/assets/764414/16891384/c68f22dc-4af0-11e6-9be2-242ce7d73ff8.png) Verve [![Build Status](https://travis-ci.org/tadeuzagallo/verve-lang.svg?branch=master)](https://travis-ci.org/tadeuzagallo/verve-lang) [![CircleCI](https://circleci.com/gh/tadeuzagallo/verve-lang.svg?style=svg)](https://circleci.com/gh/tadeuzagallo/verve-lang)

`Verve` is an experimental, minimalistic, static, functional language with zero dependencies.

## Getting Started

Right now, Verve runs macOS and Ubuntu

### macOS

For macOS, all you need is Xcode (available from the App Store)

### Ubuntu

For Ubuntu, you need `make` and `clang`, which you can install using

```
$ sudo apt-get install make clang
```

### Installing Verve

To install Verve, simply clone the repo and build it, as follows
```
$ git clone https://github.com/tadeuzagallo/verve-lang
$ cd verve-lang
$ make install
```

After that you should be able to use the command line `verve`. You can try it by running any of the tests, e.g.:
```
$ verve tests/math_parser.vrv
```

## Running the tests

The tests are split in 3 categories:
* `output_tests` - run a program and compare it's output against the expected output
* `error_tests` - run a failing program and compare it's message against the expected
* `cpp_tests` - C++ unit tests

Each category of tests can be ran individually, with `make`, as in:
```
$ make output_tests
```

Or all together with:
```
$ make test
```

## Syntax highlight
Vim syntax highlight is available within the repo, you can install it by running:
```
$ make install_vim_highlight
```

---

_I am providing code in this repository to you under an open source license. Because this is my personal repository, the license you receive to my code is from me and not from my employer (Facebook)._
