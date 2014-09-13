# SWDOGEN

[![Build Status](https://api.travis-ci.org/cyyuen/swdogen.png)](https://travis-ci.org/cyyuen/swdogen)


This is a documentation generator for [swagger-ui](https://github.com/wordnik/swagger-ui). It will scan your project source code, extract the comment with special beginning, parse the swagger code within the comment and generate the swagger documentation.

## [See the Wiki!](https://github.com/dotcy/swdogen/wiki)

The [github wiki](https://github.com/dotcy/swdogen/wiki) contains documentation, samples, etc.  Start there

## Supported Language

Any languages that supports c-style bloack comment, ie. 

```C
/* 
	comments
	...
*/
```

## How to get it

### Install from OPAM

Swdogen is implemented in OCaml and managed by [OPAM](http://opam.ocaml.org/pkg/swdogen/0.1.0/).

1. Install OPAM. See [instructions](http://opam.ocaml.org/doc/Quick_Install.html).
2. Install Swdogen: `opam install swdogen`

### Build from source

Download source code of Swdogen from [releases](https://github.com/dotcy/swdogen/releases) or clone with git.

To build using the following commands

```text
$ ocaml setup.ml -configure
$ ocaml setup.ml -build
```

To install using the following commands

```text
$ ocaml setup.ml -install
```

For optional test, reconfigure with test enable

```text
$ ocaml setup.ml -configure --enable-tests
$ ocaml setup.ml -build
$ ocaml setup.ml -test
```

To uninstall using the following commands

```shell
$ ocaml setup.ml -uninstall
```

#### Dependencies

If there is any missing dependency, you may install them by opam or by hand 

- [ounit](https://github.com/mlin/ounit): a unit test framework for OCaml
- [atdgen](https://github.com/mjambon/atdgen): a tool that derives OCaml boilerplate code from type definitions

## Usage (Just 3 steps to go)

1. create the centre config file [_swdogen](https://github.com/dotcy/swdogen/wiki/Configuration) in the top-level of your project. For details, [see the wiki](https://github.com/dotcy/swdogen/wiki).
2. Document your project with [swdogen notation](https://github.com/dotcy/swdogen/wiki/Notation), a javadoc like notation.
3. run ```swdogen``` in the top-level of your project

## Live Demo 

There are some sample programs in the example folder. Pick the one you like and move to its top-level directory which contains a _swdogen. (eg. example/petstore)

```
$ cd example/petstore
$ ls
_swdogen  models  resources
``` 

and run

```
$ swdogen
$ ls
_swdogen  apis  models  resources
```

the generated swagger-ui doc would be lie in the apis folder.

## Templates

There are some [Templates](https://github.com/dotcy/swdogen/wiki/Template) to make your API swagger even more easier.

## Contact

Problems or questions? fire an issue at [github](https://github.com/dotcy/swdogen/issues)

## License

The MIT License (MIT)

Copyright (c) <2013> <colsy2@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
