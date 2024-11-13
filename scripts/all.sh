#!/bin/bash

mkdir -p img

./inversions.R img/yc_all.jpg && open img/yc_all.jpg || true

