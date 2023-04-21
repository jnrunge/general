#!/bin/bash

# Supported by ChatGPT

# Get the input number
input_number=$1

# Use du, awk, sort, head, and numfmt commands to find the largest files
du -a --block-size=1K . | awk '{ filename = substr($2, length($2) - '$input_number'); files[filename] += $1 } END { for (key in files) print files[key], key }' | sort -nr | numfmt --field=1 --from-unit=K --to=iec --suffix=B