#!/bin/bash
cd "$(dirname "$0")"
rustc -O -o solution solution.rs
