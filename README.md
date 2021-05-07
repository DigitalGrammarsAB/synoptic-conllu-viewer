Running `stack build` in this folder produces two executables:

## synoptic-view 
Very preliminary version of a tool to visualize the CoNNL-U trees a GF `fun` was generated from side by side.

Usage example (very subject to changes):

```
stack exec -- synoptic-view 
```

and follow the link.

It is possible to search:
- automatically generated function names such as `n01002042__bank_account__bankkonto_CN`
- pipe-separated arbitrary strings (one per language), such as `bank account | bankkonto`

NOTE:
- right now, conllu file names are hardcoded
