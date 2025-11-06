# CompCert-Mod
A fork of CompCert, modified for [RefinedCC](https://github.com/PrincetonUniversity/VST/tree/wp_semax)'s frontend.

## Build
Requires Rocq/Coq.

```[bash]
./configure [option …] target
make depend
make refinedcc
```

This will create a `refinedcc` binary undert the current directory:
```[bash]
./refinedcc --version
```


To register it as a command in a unix-system, create a symlink (if /usr/local/bin is in the PATH):
```[bash]
ln -s /absolute/path/to/refinedcc /usr/local/bin/refinedcc
refinedcc --version
```
