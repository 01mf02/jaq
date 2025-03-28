#!/bin/bash
# Generate initial input corpus for fuzzing.

if [ ! -d "jaq-core" ]; then
  echo Error: run this script from the main jaq directory, such as:
  echo jaq-core/fuzz/init_corpus.sh
  exit 1
fi

cargo test -- --nocapture 2> programs
sed -i '/^   /d' programs

CORPUS=jaq-core/fuzz/corpus/load_and_compile
mkdir -p $CORPUS
cp examples/*.jq $CORPUS/

i=0
while IFS= read -r line
do
  echo "$line" > $CORPUS/test$i
  i=$((i+1))
done < programs

