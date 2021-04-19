Running `stack build` in this folder produces two executables:

## synoptic-view 
Very preliminary version of a tool to visualize the CoNNL-U trees a GF `fun` was generated from side by side.

Usage example (very subject to changes):

```
stack exec -- synoptic-view 
```

and follow the link.

It is possible to search:
- automatically generated function names such as `vehicle_auto_Fahrzeug_bil_CN`
- pipe-separated arbitrary strings (one per language), such as `the vehicle | auto | das Fahrzeug | bilen`

NOTE:
- right now, it only works for function names that correspond to one-word concepts in all languages
- right now, conllu file names are hardcoded
