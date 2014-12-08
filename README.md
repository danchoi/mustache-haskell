# mustache-haskell

A Haskell implementation of mustache templates.

Should be compatible with the the [mustache
specification](http://mustache.github.io/mustache.5.html).


## Install

You need the Haskell platform on your system.

```
cabal install mustache
```

Or alternatively

``` 
git clone git@github.com:danchoi/mustache-haskell.git
cd mustache-haskell
cabal sandbox init
cabal install
# Now copy .cabal-sandbox/bin/mus to your PATH
```

## Usage

```
mus template.mustache < input.json
```


## List separator syntax

mustache-haskell adds one additional feature to the mustache specification.  If
you are outputing elements of a list, you can designate an optional list
separator with the following syntax:


```
{{#hobbies , }}{{#name}}{{/hobbies}}
```

This designates ", " as the list separator will output

```
sewing, brewing, cooking
```

when the input is 

```json
{"hobbies":[{"name":"sewing"},{"name":"cooking"}]}
```

