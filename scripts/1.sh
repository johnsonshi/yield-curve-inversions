#!/bin/bash

mkdir -p img

./inversions.R img/yc_1.jpg 1 && open img/yc_1.jpg || true

