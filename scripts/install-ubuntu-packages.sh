#!/usr/bin/env bash

sudo apt update
sudo apt install -y \
    r-base \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libmagick++-dev
