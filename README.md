# OCaml implementation of BLS12-381

## Encoding

### Scalar

The scalar field is `Fr = GF(0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001)`, encoded on 32 bytes in little endian.

### Groups

For G1, the base field is `Fq:
GF(0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab`)
and `E(Fq) := y^2 = x^3 + 4`. An element of the base field can be encoded on 48 bytes (using only
381 bits, leaving 3 bits unused).

For G2, the base field is `Fq2 := Fq[Z]/(X^2 + 1)` and `E(Fq2) := y^2 = x^3 + 4
(Z + 1)`. An element of the base field can be encoded on 2 * 48 bytes
representing each coefficient of the polynomial. 3 bits of each coefficient
encoding are unused.

The « uncompressed » form `(x, y)` of G1 and G2 is the concatenation of the elements `x` and `y` encoded in big endian.

The « compressed » form uses the first 3 most significant (and unused) bits of
the coordinate `x`.
- the first most significant bit is always set to `1` to carry the information it
is the compressed encoding of a point.
- the second most significant bit is set to `1` if the element is the identity of the curve.
- the third most significant bit is the sign of `y`. It is set to `1` if `y` is
  lexicographically larger than `-y`.

## Install


```shell
# if you implement a library and you don't need an actual implementation
opam install bls12-381
# to target UNIX
opam install bls12-381-unix
```

By default, if the architecture supports ADX, `bls12-381-unix` with be compiled using ADX
opcodes (giving optimisations up to 20% for some arithmetic operations). If you
don't want to build using ADX, you can add the environment variable
`BLST_PORTABLE` and set it to any value.
For instance,
```
BLST_PORTABLE=y opam install bls12-381-unix
```
will instruct to build bls12-381-unix without ADX. This might be useful if you
build docker images on ADX machines but you need the image to be portable on
architecture not supporting ADX.

If the architecture does not support ADX, `bls12-381-unix` will be compiled without ADX opcodes.

## Run tests

```
dune runtest
```

To get the coverage (only ok for bls12-381-unix and bls12-381-gen)
```
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
```

## How to use in my project

If you are developing a library using `bls12-381`, you only need to add `bls12-381` in the dependency list.
However, if you are writing a binary, three packages are relevant:
- `bls12-381-unix`: to be used for UNIX

## Run the benchmarks

Install `core_bench`:

```
opam install core_bench
```

See files listed in the directory `benchmark` and execute it with `dune exec`. For instance:
```
dune exec ./benchmark/bench_fr.exe
```

## Documentation

```
opam install odoc
dune build @doc
```
