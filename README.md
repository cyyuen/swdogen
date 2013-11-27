# SWDOGEN

[![Build Status](https://api.travis-ci.org/dotcy/swdogen.png)](https://api.travis-ci.org/dotcy/swdogen)


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

## Required Components

* [atdgen](https://github.com/mjambon/atdgen)

## Installation

Under the top-level of swdogen, run

```
ocaml setup.ml -all
```

to install and run

```
ocaml setup.ml -distclean
```

to clean the build and configure file. run

```
ocaml setup.ml -uninstall
```

to remove the swdogen

## Usage (Just 3 steps to go)

1. create the centre config file [_swdogen](https://github.com/dotcy/swdogen/wiki/Configuration) in the top-level of your project. For details, [see the wiki](https://github.com/dotcy/swdogen/wiki).
2. Document your project with [swdogen notation](https://github.com/dotcy/swdogen/wiki/Notation), a javadoc like notation.
3. run ```swdogen``` in the top-level of your project

## <font color="orange"> Waring </font>

**swdogen is under heavy development. please use in your own risk**

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
