# silly-k
[![Build status](https://travis-ci.org/rootmos/silly-k.svg?branch=master)](https://travis-ci.org/rootmos/silly-k)

`silly-k` is an experimental hobby language inspired by [K](http://kparc.com/)
and [APL](https://en.wikipedia.org/wiki/APL_(programming_language)).

The silly prefix is meant to indicate that this is project is nothing more than an experiment
to see how these languages would perform when described in lambda calculus terms.

The compiler is written for the [nanopass framework](https://github.com/nanopass/nanopass-framework-scheme)
using the following structure:
* the `K`-like, `APL` inspired, syntax is translated into something like
  [simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
  which is used to resolve the overloaded symbols from K,
* after that, the types are thrown away and is compiled to
  [Malfunction](https://github.com/stedolan/malfunction) or to
  [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) (which is used mainly for
  quick tests and the REPL).

## Usage
Easiest way to play around with it is to run the [Docker image](https://hub.docker.com/r/rootmos/silly-k/):
```
rlwrap docker run --rm -it rootmos/silly-k
```
which starts the REPL (under [rlwrap](https://github.com/hanslub42/rlwrap)).

## Examples
Here's some examples of the syntax, taken directly from the [tests](https://github.com/rootmos/silly-k/blob/master/tests.scm):

Code | Stdin | Stdout
---- | ----- | ------
`]7` | | `7`
`]1 2 3` | | `1 2 3`
`]1=2` | | `0`
`]2=2` | | `1`
`]2<3` | | `1`
`]2<2` | | `0`
`]3<2` | | `0`
`]1<1 2 3` | | `0 1 1`
`]1 2 3>2` | | `0 0 1`
`]2>3` | | `0`
`]2>2` | | `0`
`]3>2` | | `1`
`]@{w>1}'1 2 3` | | `0 1 1`
`]3>1 2 3` | | `1 1 0`
`]1 2 3>2` | | `0 0 1`
`]1=2` | | `0`
`]2=2` | | `1`
`]@{w=2}'1 2 3` | | `0 1 0`
`]1 2 3=3` | | `0 0 1`
`]1=1 2 3` | | `1 0 0`
`]~1=2` | | `1`
`]~2=2` | | `0`
`]~0` | | `1`
`]~7` | | `0`
`]~2=1 2 3` | | `1 0 1`
`](1=2)\|2=3` | | `0`
`](1=2)\|2=2` | | `1`
`](2=2)\|2=3` | | `1`
`](2=2)\|3=3` | | `1`
`](1=2)&2=3` | | `0`
`](1=2)&2=2` | | `0`
`](2=2)&2=3` | | `0`
`](2=2)&3=3` | | `1`
`]2&3` | | `2`
`]2\|3` | | `3`
`]1+2` | | `3`
`]1+2 3` | | `3 4`
`]1 2+3 4` | | `4 6`
`]1 2+3` | | `4 5`
`](1=1)+(2=2)` | | `2`
`]1+(2=2)` | | `2`
`]1+2=1 2 3` | | `1 2 1`
`](2=1 2 3)+1` | | `1 2 1`
`]2-3` | | `-1`
`]1-(-2)` | | `3`
`]1 2-3 4` | | `-2 -2`
`]@-7` | | `-7`
`]@-(-2)` | | `2`
`]1-2 3` | | `-1 -2`
`]1 2-3` | | `-2 -1`
`]2*3` | | `6`
`]1 2*3` | | `3 6`
`]4*2 3` | | `8 12`
`]1 2*3 4` | | `3 8`
`]!1` | | `0`
`]!4` | | `0 1 2 3`
`]*1 2 3` | | `1`
`]*0=0 1` | | `1`
`]*0=1 0` | | `0`
`](*0=0 1;7;8)` | | `7`
`](*0=1 0;7;8)` | | `8`
`]#1 2 3` | | `3`
`]#1 2 3 4` | | `4`
`]4#1 2` | | `1 2 1 2`
`]3#1` | | `1 1 1`
`]1 2 3@1` | | `2`
`]((2=1 2 3)@1;7;8)` | | `7`
`]((2=1 2 3)@2;7;8)` | | `8`
`]1 2 3@0 2` | | `1 3`
`]@{w+1}'1 2 3` | | `2 3 4`
`]@{1-w}'3 4 5` | | `-2 -3 -4`
`]2{a+w}'3 4 5` | | `5 6 7`
`]2{w-a}'3 4 5` | | `1 2 3`
`]2+'3 4 5` | | `5 6 7`
`]2-'3 4 5` | | `-1 -2 -3`
`]{w@1}'{+w}'1 2 3` | | `2 3 4`
`]{w@1}'{-w}'1 2 3` | | `0 -1 -2`
`]+/1 2 3` | | `6`
`]-/1 2 3` | | `2`
`]{w-a}/1 2 3` | | `0`
`]+/2<!5` | | `2`
`]&/0<1 2 3` | | `1`
`]&/0<1 0 3` | | `0`
`]1:` | `7` | `7`
`]0:` | `1 2 3` | `1 2 3`
`](1:)+1` | `7` | `8`
`]1+0:` | `1 2 3` | `2 3 4`
`](0:)+1:` | `2\n1 2 3` | `3 4 5`
`](1=1;2;3)` | | `2`
`](1=2;1 2;3 4)` | | `3 4`
`]7{w=1;w;a}1` | | `1`
`]7{w=1;w;a}8` | | `7`
`]{w=1;w+1;w=2;w+2;w+3}1` | | `2`
`]{w=1;w+1;w=2;w+2;w+3}2` | | `4`
`]{w=1;w+1;w=2;w+2;w+3}3` | | `6`
`]{w=1;w+1;w=2;w+2;w+3}4` | | `7`
`](1=1;2;3)` | | `2`
`]{w=0;0;w+_f(w-1)}6` | | `21`
`]{w=1;1;w=2;1;(_f(w-2))+_f(w-1)}1:` | `7` | `13`
`]x+x:7` | | `14`
`](x:1)+x:2` | | `3`
`]x+(x:1)+x:2` | | `4`
`]x+x{x:1+a-w}x:2` | | `3`
