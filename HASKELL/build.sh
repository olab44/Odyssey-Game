#!/bin/bash

OUTPUT="odyssey"

FILES=$(find . -type f -name "*.hs")

echo "Compiling project..."
ghc -o "$OUTPUT" $FILES

if [ $? -eq 0 ]; then
    echo "Compilation successful! Executable created: ./$OUTPUT"
else
    echo "Compilation failed."
    exit 1
fi
echo "Cleaning up intermediate files..."
find . -type f \( -name "*.o" -o -name "*.hi" \) -delete
