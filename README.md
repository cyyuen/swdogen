# SWDOGEN

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