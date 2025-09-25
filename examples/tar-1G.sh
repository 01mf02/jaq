#!/bin/bash
# Create 1024 files of 1MB each and write a 1GB TAR archive to stdout.
head -c 1048576 < /dev/zero | tr '\0' 'O' > 1M
mkdir -p 1M-files
for i in $(seq 1 1024); do
    cp 1M 1M-files/file$i
done
(cd 1M-files && tar cf - *)
rm -r 1M 1M-files
