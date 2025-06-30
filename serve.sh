#!/bin/bash
spago bundle
parcel build ./web/app.html --no-cache
parcel serve ./web/app.html --no-cache

