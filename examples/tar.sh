#!/bin/bash
# Create $1 files of $2 bytes each and writes a TAR archive to stdout.
# $1 defaults to 1024 files and
# $2 defaults to 1048576 bytes (1MB)
FILES="${1:-1024}"
BYTES="${2:-1048576}"
# replace NUL with 'O' to make file contents in the TAR file more discoverable
head -c $BYTES < /dev/zero | tr '\0' 'O' > tar-file
mkdir -p tar-files
for i in $(seq 1 $FILES); do
    cp tar-file tar-files/file$i
done
(cd tar-files && tar cf - *)
rm -r tar-file tar-files
