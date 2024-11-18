#!/bin/bash

# Check if unlit is available
if ! command -v unlit &> /dev/null; then
    echo "Error: 'unlit' command not found. Please ensure GHC is installed and in your PATH."
    exit 1
fi

# Use provided directory or current directory
DIR="${1:-.}"

if [ ! -d "$DIR" ]; then
    echo "Error: '$DIR' is not a valid directory"
    exit 1
fi

echo "Converting .lhs files in $(realpath "$DIR")"

# Find all .lhs files and convert them
find "$DIR" -type f -name "*.lhs" | while read -r file; do
    output_file="${file%.lhs}.hs"
    echo "Converting $file to $output_file"
    
    if unlit -i "$file" -o "$output_file"; then
        echo "✓ Successfully converted $(basename "$file")"
    else
        echo "✗ Failed to convert $(basename "$file")"
    fi
done
