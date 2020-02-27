#!/bin/sh
cd "$1"
patch -p1 -i ../71cc1a5a59bff33dd577a8b9c3761f7e0f179848.patch
cd "$1/lisp-kernel/win64"
make -j$(nproc)
cd "$1"
./wx86cl64.exe -n -l lib/x8664env.lisp -e '(ccl:xload-level-0)' -e '(ccl:compile-ccl)' -e '(#__exit 0)' # https://github.com/Clozure/ccl/issues/266
 echo '(ccl:save-application "wx86cl64.image")'|./wx86cl64.exe --image-name wx86-boot64.image --no-init --quiet --batch
