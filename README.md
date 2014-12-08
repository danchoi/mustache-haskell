# text-mustache

A Haskell implementation of mustache templates

Currently not production


time dist mus2 test/tmpl1 < test/input1

Should be renamed parse template or something
dist mus < test/tmpl1

## list separator

Added to the mustache spec is the list separator syntax

```
{{# hobbies , }}{{name}}{{/hobbies}}
```

will insert a comma and a space between the elements

