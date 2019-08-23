#!/bin/sh
cd "$1/lisp-kernel/win64"
make -j$(nproc)
cd "$1"
./wx86cl64.exe -n -l lib/x8664env.lisp -e '(ccl:xload-level-0)' -e '(ccl:compile-ccl)' -e '(#__exit 0)'
 echo '(ccl:save-application "wx86cl64.image")'|./wx86cl64.exe --image-name wx86-boot64.image --no-init --quiet --batch
