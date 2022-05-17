#!/bin/sh -e

# Modifying `libblst/build.sh` is avoided because we want to keep the fork
# clean.

# If you update the build.sh flags, you must also update
# .github/workflows/build-blst-on-macos accordingly

# Adding -Wno-missing-braces for clang-11 (and also should be for clang-10, but clang 10
# is not officially supported). See .github/workflows/build-blst-on-macos for the reason

# Use BLST_PORTABLE environment variable to overwrite the check in
# libblst/build.sh to use ADX instructions. build.sh uses /proc/cpuinfo to
# decide to use ADX or not. Useful if you build binaries for archs not
# supporting ADX on a arch supporting ADX.
cd libblst

build_for_unix() {
  if [ $(uname --machine) = "s390x" ]; then
      echo "()" > ../c_flags_blst.sexp
      ./build.sh -shared -Wno-missing-braces -D__BLST_NO_ASM__
  elif [ -n "${BLST_PORTABLE}" ]; then
      echo "(-D__BLST_PORTABLE__)" > ../c_flags_blst.sexp
      ./build.sh -shared -Wno-missing-braces -D__BLST_PORTABLE__
  else
      echo "()" > ../c_flags_blst.sexp
      ./build.sh -shared -Wno-missing-braces
  fi
}

case "$(uname -s)" in
CYGWIN*|MINGW32*|MSYS*)
    echo "()" > ../c_flags_blst.sexp
    ./build.sh flavour=mingw64 CC=x86_64-w64-mingw32-gcc "-shared"
        ;;
*)
    build_for_unix
esac

