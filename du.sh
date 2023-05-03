#!/bin/bash

depth=${1:-3}
find . -maxdepth $depth -type d -print0 | while read -d '' -r dir; do du -h -s "$dir"; done | sort -rh > du
less du



