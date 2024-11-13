#!/bin/bash

mkdir -p img

./inversions.R img/yc_5.jpg 5 && open img/yc_5.jpg || true

