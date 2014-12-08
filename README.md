# mustache-haskell

A Haskell implementation of mustache templates. Expects input data to be JSON. 

Should be compatible with the the [mustache
specification](http://mustache.github.io/mustache.5.html).  Except lambdas are
not supported.

Supported syntax:

* basic variables
* sections
* inverted sections
* comments (are suppressed)
* partials
* set delimiter

This project currently only provides a command line interface.
Later versions will provide a library API for templating ToJSON
instances.


## Install

You need the Haskell platform on your system.

```
cabal install mustache-haskell
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

```
mus 

Usage: mus [-c] [-d TEMPLATE_DIRECTORY] TEMPLATE_FILE
  A Haskell implementation of Mustache templates. On STDIN provide the JSON to
  insert into the template.

Available options:
  -h,--help                Show this help text
  -c                       Just output parse tree of template file
  -d TEMPLATE_DIRECTORY    Template directory
```

## List separator syntax

mustache-haskell adds one additional feature to the mustache specification.  If
you are outputing elements of a list, you can designate an optional list
separator with the following syntax:


```
{{#hobbies, }}{{#name}}{{/hobbies}}
```

This designates `", "` as the list separator and will output

```
sewing, brewing, cooking
```

when the input is 

```json
{"hobbies":[{"name":"sewing"},{"name":"brewing"},{"name":"cooking"}]}
```


## Performance

In a few informal tests, 

```
mus template.mustache < input.json
```

is about 10x faster than the Ruby mustache gem command-line implementation,

```
mustache - template.mustache < input.json
```


## Related projects

* [whiskers](https://github.com/nejstastnejsistene/whiskers) Mustache templates with Template Haskell ([reddit](http://www.reddit.com/r/haskell/comments/2kjgg2/whiskers_moustache_templates_with_template_haskell/))
* [hastache](https://github.com/lymar/hastache) Haskell implementation of Mustache templates
* [mustache2hs](http://hackage.haskell.org/package/mustache2hs) takes in Haskell records (single data constructor only) and a list of mustache template and record name pairs, and generates Haskell code for functions that take an escape function and one of the records
* [mustache.go](https://github.com/hoisie/mustache) an implementation of the mustache template language in Go


