#!/bin/bash
spago bundle --module Test.Demo --outfile ./web/app.js --platform browser --bundle-type app
parcel build ./web/app.html --no-cache
parcel serve ./web/app.html --no-cache
