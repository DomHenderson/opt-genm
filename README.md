# GenM Optimiser

The GenM optimiser performs low-level cross-language optimisations on a
low-level IR which can be easily generated from various compilers.

## Setup

The opt, llvm, clang and ocaml projects should be checked out in the following folders:

```
<prefix>
├─ llvm               https://github.com/nandor/llvm-genm
│  └─ tools
│     └─ clang        https://github.com/nandor/clang-genm
├─ ocaml              https://github.com/nandor/ocaml-genm
├─ opt                https://github.com/nandor/opt-genm
└─ dist               install prefix
```

### llvm and clang

To generate GenM using LLVM, the llvm-genm and clang-genm forks are required. To configure:

```
cmake ..                               \
  -G Ninja                             \
  -DCMAKE_BUILD_TYPE=MinSizeRel        \
  -DCMAKE_INSTALL_PREFIX=<prefix>/dist \
  -DLLVM_TARGETS_TO_BUILD="X86;GenM"   \
  -DLLVM_ENABLE_DUMP=ON
```

### ocaml

To generate GenM from OCaml, the ocaml-genm fork is required and a symbolic link
to the llvm fork must be added to `$PATH` under the `genm-gcc` alias. The OCaml
compiler can be built using the following commands:

```
./configure                     \
  --target genm                 \
  --target-bindir <prefix>/dist \
  --prefix <prefix>/dist        \
  -no-ocamlbuild                \
  -no-ocamldoc                  \
  -no-debugger                  \
  -no-instrumented-runtime      \
  -no-debug-runtime             \
  -no-pthread                   \
  -no-graph                     \
  -fPIC                         \
  -flambda                      \
  -no-cfi
make world.opt
make install
```

### opt

Debug builds are configured as follows:
```
mkdir build
cd build
cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug -DLLVM_DIR=<prefix>/dist
```

Since the instruction selectors require target-specific information, the headers
of the available targets must be included in the custom LLVM install location.
