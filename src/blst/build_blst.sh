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

ARGS=
if [ -n "${BLST_PORTABLE}" ]; then
  ARGS="-shared -Wno-missing-braces -D__BLST_PORTABLE__"
else
  ARGS="-shared -Wno-missing-braces"
fi

case "$1" in
	install)
    case "$(uname -s)" in
	CYGWIN*|MINGW32*|MSYS*)
        cp "libblst.a" "../libblst.a"
        cp "blst.dll" "../dllblst.dll"
        ;;
	Darwin)
        cp "libblst.a" "../libblst.a"
        cp "libblst.dylib" "../dllblst.so" # Because https://github.com/ocaml/dune/issues/4948 and https://github.com/ocaml/ocaml/pull/988#issuecomment-269288136
	    ;;
	*)
        cp "libblst.a" "../libblst.a"
        cp "libblst.so" "../dllblst.so"
    esac
	;;
    *)
    case "$(uname -s)" in
	CYGWIN*|MINGW32*|MSYS*)
	    ./build.sh flavour=mingw64 CC=x86_64-w64-mingw32-gcc "-shared"
            ;;
	*)
	    ./build.sh ${ARGS}
    esac
esac

