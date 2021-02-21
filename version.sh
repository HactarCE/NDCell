#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Provide a version number as argument; e.g."
    echo
    echo "$0 1.0.0"
    exit 1
fi

# Set package `version`.
sed -i "s/^version = \"[[:digit:]\\.]*\"/version = \"$1\"/" ui/Cargo.toml

# Set Windows metadata `ProductVersion`.
sed -i "s/^ProductVersion = \"[[:digit:]\\.]*\"/ProductVersion = \"$1\"/" ui/Cargo.toml

# Set environment variable `NDCELL_VERSION` in GitHub Actions workflow
sed -i "s/NDCELL_VERSION: [[:digit:]\\.]*/NDCELL_VERSION: $1/" .github/workflows/*.yml
