#!/bin/bash
spago bundle --module Test.Demo
parcel build ./web/app.html --no-cache
parcel serve ./web/app.html --no-cache
