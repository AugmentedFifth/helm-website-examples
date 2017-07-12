# Helm Website Examples

All tutorials on [the Helm website](http://helm-engine.org/) link here to show
completed source code. Said example code is located in `/src`.

## Executing examples

Make sure you have
[the Stack tool](https://docs.haskellstack.org/en/stable/README/) installed.

Then run

```bash
$ stack build

$ stack exec helm-website-examples-exe <examplename>
```

where `<examplename>` is the desired one of

* `colors`
* `gradients`
* `curves-and-animation`
