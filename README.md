# AAL_code
Source code for Array Animation Language

Try out AAL [here](https://github.com/bagnalla/AAL).

[About AAL](https://bagnalla.github.io/AAL/about.html).

[Example programs](https://bagnalla.github.io/AAL/examples.html)

## Required OPAM packages

* batteries
* js_of_ocaml
* js_of_ocaml-camlp4
* menhir
* ppx_sexp_conv

```
opam install batteries js_of_ocaml js_of_ocaml-camlp4 menhir ppx_sexp_conv
```

npm and browserify are also required.

## Build

Separate makefiles are included for the OCaml and JavaScript developments in src/ and web/scripts/, respectively.

#### src/

`make` will compile an AAL interpreter that can be used to evaluate AAL programs locally with no animation.

`make test` will run the local AAL interpreter on all files ending with '.aal' in src/tests/

`make js` will compile the JavaScript version to the file 'aaljs.js'. Copy this file into web/scripts/.

#### web/scripts

`make` will use browserify to package all of the scripts into the file 'bundle.js'.

## Info

AAL is a simple imperative language with a few built-in facilities for working with arrays. An AAL program is compiled down to a sort of bytecode called AIS, which is then interpreted by an interactive JavaScript animation runtime built on HTML5 canvas.

There are many existing resources for visualizing sorting algorithms, but AAL can be used to easily experiment with new algorithms and interactively step through their execution in a visual environment. It's also useful for illustrating the call stack (it even grows downward!).

## Disclaimer

This code was written a while ago and was never really intended to be seen by anyone. I don't even remember how half of it works. View at your own peril.
